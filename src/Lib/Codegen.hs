{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecursiveDo #-}

module Lib.Codegen
  ( codegen
  , codegenModule
  , codegenDefn
  ) where

import Control.Monad.State
import qualified LLVM.AST.FloatingPointPredicate as AST
import LLVM.AST
import LLVM.IRBuilder as IRB
-- import qualified Data.Text.Lazy.IO as T
-- import LLVM.Pretty

import qualified Data.Map as Map

import Types.Codegen
import qualified Types.AST as A

{- HLINT ignore "long functions" -}
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

codegen (A.WhileExpr cond body) = mdo
  br loopBlock
  loopBlock <- block `named` "while.loop"
  codegen body
  condOp <- codegen cond
  let falseOp = double 0
  test <- fcmp AST.ONE falseOp condOp
  condBr test loopBlock contBlock
  contBlock <- block `named` "while.cont"
  pure (double 0)

{- HLINT ignore "long functions" -}
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
  fnOp <- IRB.function name
    [(doubleTy, ParameterName (unpackName nm)) | nm <- args]
    doubleTy $ funGen args body
  modify $ \s -> s {functionTable = Map.insert name fnOp (functionTable s)}
  pure fnOp

funGen :: [Name] -> A.AST -> [Operand]
       -> IRBuilderT (ModuleBuilderT Codegen) ()
funGen args body ops = do
  entryBlock <- block `named` "entry"
  forM_ (zip args ops) $ \(name, arg) -> do
    a <- alloca doubleTy Nothing 0
    store a 0 arg
    lift $ assignvar name a
  codegen body >>= ret

codegenModule :: FilePath -> [A.Phrase] -> Codegen Module
codegenModule fp phrases = do
  modDefs <- gets modDefinitions
  anonPhrases <- traverse toAnon phrases
  defs <- IRB.execModuleBuilderT IRB.emptyModuleBuilder
    (mapM_ codegenDefn anonPhrases)
  let defs' = modDefs ++ defs
  modify (\s -> s {modDefinitions = defs'})
  IRB.buildModuleT (packShort fp) (traverse IRB.emitDefn defs')
  -- liftIO (T.putStrLn (ppllvm mod))
