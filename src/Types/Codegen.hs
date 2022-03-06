{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Types.Codegen
  ( Codegen
  , evalCodegen
  , codegenModule
  , emptyModule
  , cgen
  )
where

import qualified Types.AST as A

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short
import qualified Data.Map as Map

import LLVM.AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.FloatingPointPredicate as AST
import LLVM.IRBuilder as IRB
import qualified LLVM.PassManager as LLPM

cgen :: [A.Phrase] -> IO Module
cgen = evalCodegen . codegenModule

-- codegen type {{{
type Codegen = StateT CodegenState IO
data CodegenState = CodegenState
  { symbolTable :: Map.Map Name Operand
  , functionTable :: Map.Map Name Operand
  , modDefinitions :: [Definition]
  , nameSupply :: Word
  }

emptyModule :: String -> Module
emptyModule label = defaultModule { moduleName = packShort label }

evalCodegen :: Codegen a -> IO a
evalCodegen = flip evalStateT emptyCodegen

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
  { symbolTable = Map.empty
  , functionTable = Map.empty
  , modDefinitions = []
  , nameSupply = 0
  }

doubleTy :: Type
doubleTy = FloatingPointType DoubleFP

ptrTy :: [Type] -> Type
ptrTy atys = PointerType (FunctionType doubleTy atys False) (AddrSpace 0)

getVar :: Name -> ModuleBuilderT Codegen Operand
getVar name = do
  res <- gets (Map.lookup name . symbolTable)
  case res of
    Just x -> pure x
    Nothing -> error ("unknown variable: " ++ show name)

getFun :: Name -> [Type] -> ModuleBuilderT Codegen Operand
getFun name tys = gets symbolTable >>= (\case
    Just x -> pure x
    Nothing -> pure . ConstantOperand
      $ C.GlobalReference (ptrTy tys) name) . Map.lookup name

assignvar :: Name -> Operand -> ModuleBuilderT Codegen ()
assignvar name var = modify (\s -> s {symbolTable = Map.insert name var (symbolTable s)})
-- }}}

-- actual gen {{{
codegen :: A.AST -> IRBuilderT (ModuleBuilderT Codegen) Operand
codegen (A.Literal d) = pure $ double d
codegen (A.Identifier name) = lift (getVar name) >>= flip load 0
codegen (A.UnOp op e) = codegen $ A.Call (prefixName "unary" op) [e]
codegen (A.BinOp "=" (A.Identifier var) val) = do
  i <- lift $ getVar var
  v <- codegen val
  store i 0 v
  pure v
codegen (A.BinOp op l r) = case op of
    "+" -> withOp fadd
    "-" -> withOp fsub
    "*" -> withOp fmul
    "<" -> withOp (fcmp AST.ULT) >>= (`uitofp` doubleTy)
    _ -> codegen (A.Call (prefixName "binary" op) [l, r])
  where withOp f = join $ f <$> codegen l <*> codegen r

codegen (A.Call name args) = do
  largs <- traverse (fmap (,[]) . codegen) args
  lfun <- lift $ getFun name (replicate (Prelude.length largs) doubleTy)
  call lfun largs

codegen (A.IfExpr cond tr fl) = mdo
  condOp <- codegen cond
  test <- fcmp AST.ONE false condOp
  condBr test thenBlock elseBlock
  block `named` "if.then"
  trval <- codegen tr
  thenBlock <- currentBlock
  br contBlock
  block `named` "if.else"
  flval <- codegen fl
  elseBlock <- currentBlock
  br contBlock
  contBlock <- block `named` "if.cont"
  phi [(trval, thenBlock), (flval, elseBlock)]
    where false = double 0

codegen (A.ForExpr ivar start cond step body) = mdo
  i <- alloca doubleTy Nothing 0 `named` "i"
  istart <- codegen start
  stepVal <- codegen step
  store i 0 istart
  lift $ assignvar ivar i
  br loopBlock
  loopBlock <- block `named` "for.loop"
  codegen body
  iCurrent <- load i 0
  iNext <- fadd iCurrent stepVal
  store i 0 iNext
  condOp <- codegen cond
  let falseOp = double 0
  test <- fcmp AST.ONE falseOp condOp
  condBr test loopBlock contBlock
  contBlock <- block `named` "for.cont"
  pure (double 0)

codegen (A.Let var val body) = do
  i <- alloca doubleTy Nothing 0 `named` unpackName var
  v <- codegen val
  store i 0 v
  lift $ assignvar var i
  codegen body

codegen (A.WhileExpr _var _body) = undefined

codegenDefn :: A.Defn -> (ModuleBuilderT Codegen) Operand
codegenDefn (A.UnaryDef name args body) = codegenDefn
  $ A.Function (prefixName "unary" name) [args] body
codegenDefn (A.BinaryDef name args body) = codegenDefn
  $ A.Function (prefixName "binary" name) args body
codegenDefn (A.Extern name args) = do
    extOp <- extern name (fmap (const doubleTy) args) doubleTy
    modify $ \s -> s {functionTable=Map.insert name extOp (functionTable s)}
    pure extOp

codegenDefn (A.Function name args body) = do
    fnOp <- undefined LLVM.AST.function name
      [(doubleTy, ParameterName (unpackName nm)) | nm <- args]
      doubleTy
      $ \ops -> do
        _ <- block `named` "entry"
        forM_ (zip args ops) $ \(name, arg) -> do
          a <- alloca doubleTy Nothing 0
          store a 0 arg
          lift $ assignvar name a
        retval <- codegen body
        ret retval
    modify $ \s -> s {functionTable = Map.insert name fnOp (functionTable s)}
    pure fnOp

codegenModule :: [A.Phrase] -> Codegen Module
codegenModule phrases = do
  modDefs <- gets modDefinitions
  anonPhrases <- traverse toAnon phrases
  defs <- IRB.execModuleBuilderT IRB.emptyModuleBuilder
    (mapM_ codegenDefn anonPhrases)
  let defs' = modDefs ++ defs
  modify (\s -> s {modDefinitions = defs'})
  IRB.buildModuleT (packShort "<stdin>") (traverse IRB.emitDefn defs')
  -- liftIO (T.putStrLn (ppllvm mod))
-- }}}

-- helpers {{{
prefixName :: String -> Name -> Name
prefixName pre (Name nm) = mkName (pre <> unpackBS nm)
prefixName pre (UnName nm) = mkName (pre <> show nm)

unpackBS :: ShortByteString -> String
unpackBS = BS.unpack . fromShort

packShort :: String -> ShortByteString
packShort = toShort . BS.pack

unpackName :: Name -> ShortByteString
unpackName (Name   nm) = nm
unpackName (UnName nm) = packShort $ show nm
-- unpackName (UnName nm) = error $ "unpackName on UnName" <> show nm

toAnon :: A.Phrase -> Codegen A.Defn
toAnon (A.DefnPhrase f) = pure f
toAnon (A.ExprPhrase e) = do
  lastId <- gets nameSupply
  let newId = UnName $ lastId + 1
  modify $ \s -> s {nameSupply=lastId + 1}
  pure $ A.Function (prefixName "anon" newId) [] e

getLastAnon :: Codegen (Maybe String)
getLastAnon = gets nameSupply >>= \lastId ->
  pure $ if lastId == 0
            then Nothing
            else Just $ "anon" ++ show lastId

passes :: LLPM.PassSetSpec
passes = LLPM.defaultCuratedPassSetSpec {LLPM.optLevel = Just 1}
-- }}}
