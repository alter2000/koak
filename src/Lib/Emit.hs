{-# LANGUAGE OverloadedStrings #-}

module Lib.Emit where

import LLVM.Module
import LLVM.Context

import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

import Control.Monad.Except
import qualified Data.Map as Map
import qualified Data.ByteString.Short as BSS
import qualified Data.ByteString.Char8 as BS

import Types.Codegen
import Lib.Codegen
import qualified Types.AST as A
import RecursionSchemes

toSignature :: [BSS.ShortByteString] -> [(AST.Type, AST.Name)]
toSignature = map (\x -> (double, AST.Name x))

codegenTop :: A.AST' -> LLVM ()
codegenTop (Fix (A.Function name args body)) = define double name
  (toSignature (packShort <$> args))
  $ createBlocks . execCodegen $ do
    entry' <- addBlock entryBlockName
    setBlock entry'
    forM_ args $ \a -> do
      var <- alloca double
      store var $ local (AST.Name (packShort a))
      assign a var
    cgen body >>= ret

codegenTop (Fix (A.Extern name args)) = external double name fnargs
  where fnargs = Map.fromList $ toSignature (packShort <$> args)

codegenTop exp = define double "main" [] blks
  where blks = createBlocks . execCodegen $ do
          addBlock entryBlockName >>= setBlock
          cgen exp >>= ret

-------------------------------------------------------------------------------
-- Operations
-------------------------------------------------------------------------------

lt :: AST.Operand -> AST.Operand -> Codegen AST.Operand
lt a b = do
  test <- fcmp FP.ULT a b
  uitofp double test

binops :: Map.Map A.BinFunc
  (AST.Operand -> AST.Operand -> Codegen AST.Operand)
binops = Map.fromList [
      (A.Plus,   fadd)
    , (A.Minus,  fsub)
    , (A.Times,  fmul)
    , (A.Divide, fdiv)
    , (A.LessThan, lt)
  ]

cgen :: A.AST' -> Codegen AST.Operand
cgen (Fix (A.UnOp op a)) = do  -- TODO: show op?
  cgen $ Fix $ A.Call ("unary" ++ show op) [a]
cgen (Fix (A.Assignment var val)) = do
  a <- getVar var
  cval <- cgen val
  store a cval >> pure cval
cgen (Fix (A.BinOp op a b)) = case Map.lookup op binops of
    Just f  -> (cgen b >>=) . f =<< cgen a
    Nothing -> error "No such operator"
cgen (Fix (A.Identifier x)) = getVar x >>= load
cgen (Fix (A.Literal n)) = return $ cons $ C.Float (F.Double n)
cgen (Fix (A.Call fn args)) = traverse cgen args
  >>= call (externf (AST.Name $ packShort fn))

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail pure

codegen :: AST.Module -> [A.AST'] -> IO AST.Module
codegen mod fns = withContext $ \ctx ->
  withModuleFromAST ctx newAST $ \m ->
    moduleLLVMAssembly m >>= BS.putStrLn >> pure newAST
  where newAST = runLLVM mod $ traverse codegenTop fns
