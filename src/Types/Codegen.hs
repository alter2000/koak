{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Types.Codegen
    where

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.ByteString.Short
import qualified Data.Map as Map

import LLVM.AST
import LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant as C
import LLVM.IRBuilder as IRB

import qualified Types.AST as A

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
getVar name = gets (Map.lookup name . symbolTable) >>= \case
  Just x  -> pure x
  Nothing -> error ("unknown variable: " ++ show name)

getFun :: Name -> [Type] -> ModuleBuilderT Codegen Operand
getFun name tys = gets symbolTable >>= (\case
  Just x -> pure x
  Nothing -> pure . ConstantOperand
    $ C.GlobalReference (ptrTy tys) name) . Map.lookup name

assignvar :: Name -> Operand -> ModuleBuilderT Codegen ()
assignvar name var = modify (\s -> s
  {symbolTable = Map.insert name var (symbolTable s)})
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

-- getLastAnon :: Codegen (Maybe String)
-- getLastAnon = gets nameSupply >>= \lastId ->
--   pure $ if lastId == 0
--             then Nothing
--             else Just $ "anon" ++ show lastId

-- }}}
