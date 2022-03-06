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

codegenToplevel :: A.AST' -> LLVM ()
codegenToplevel (Fix (A.Function name args body)) = define double name
  (toSignature (packShort <$> args))
  $ createBlocks . execCodegen $ do
    addBlock entryBlockName >>= setBlock
    forM_ args $ \a -> do
      var <- alloca double
      store var $ local (AST.Name (packShort a))
      assign a var
    cgen body >>= ret

codegenToplevel (Fix (A.Extern name args)) = external double name
 $ Map.fromList $ toSignature (packShort <$> args)

codegenToplevel ex = define double "main" []
  $ createBlocks . execCodegen $
    addBlock entryBlockName >>= setBlock >> cgen ex >>= ret

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

true, false :: AST.Operand
true  = cons . C.Float $ F.Double 1.0
false = cons . C.Float $ F.Double 0.0

cgen :: A.AST' -> Codegen AST.Operand
cgen (Fix (A.UnOp op a)) = do  -- TODO: show op?
  cgen $ Fix $ A.Call ("unary" ++ show op) [a]
cgen (Fix (A.Let var val expr)) = do
  i <- alloca double
  val <- cgen val
  store i val
  assign var i
  cgen expr
cgen (Fix (A.BinOp op a b)) = case Map.lookup op binops of
    Just f  -> (cgen b >>=) . f =<< cgen a
    Nothing -> error "No such operator"

cgen (Fix (A.Identifier x)) = getVar x >>= load
cgen (Fix (A.Literal n)) = pure $ cons $ C.Float (F.Double n)
cgen (Fix (A.Call fn args)) = traverse cgen args
  >>= call (externf (AST.Name $ packShort fn))

cgen (Fix (A.IfExpr cond if' else')) = do
  (ifthen, ifelse, ifexit) <- (,,)
    <$> addBlock "if.then" <*> addBlock "if.else" <*> addBlock "if.exit"
  cond <- cgen cond
  test <- fcmp FP.ONE false cond
  cbr test ifthen ifelse
  blkTrue  <- setBlock ifthen >> cgen if'
  ifthen   <- br ifexit >> getBlock
  blkFalse <-setBlock ifelse >> cgen else'
  ifelse   <- br ifexit >> getBlock
  setBlock ifexit
  phi double [(blkTrue, ifthen), (blkFalse, ifelse)]

cgen (Fix (A.ForExpr ivar start cond step body)) = do
  forloop <- addBlock "for.loop"
  forexit <- addBlock "for.exit"

  -- %entry
  ------------------
  i <- alloca double
  istart <- cgen start           -- Generate loop variable initial value
  stepval <- cgen step           -- Generate loop variable step

  store i istart                 -- Store the loop variable initial value
  assign ivar i                  -- Assign loop variable to the variable name
  br forloop                     -- Branch to the loop body block

  -- for.loop
  ------------------
  setBlock forloop
  cgen body                      -- Generate the loop body
  ival <- load i                 -- Load the current loop iteration
  inext <- fadd ival stepval     -- Increment loop variable
  store i inext

  cond <- cgen cond              -- Generate the loop condition
  test <- fcmp FP.ONE false cond -- Test if the loop condition is True ( 1.0 )
  cbr test forloop forexit       -- Generate the loop condition

  -- for.exit
  ------------------
  setBlock forexit
  return false

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail pure

codegen :: AST.Module -> [A.AST'] -> IO AST.Module
codegen mod fns = withContext $ \ctx ->
  withModuleFromAST ctx newAST $ \m ->
    moduleLLVMAssembly m >>= BS.putStrLn >> pure newAST
  where newAST = runLLVM mod $ traverse codegenToplevel fns
