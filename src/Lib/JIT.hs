{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unused-do-bind #-}

module Lib.JIT
  ( runJIT,
  )
where

import Control.Exception (SomeException, catch)
import qualified Data.ByteString.Char8 as ByteString
import Foreign.Ptr (FunPtr, castFunPtr)
import qualified LLVM.AST as AST
import LLVM.Analysis
import LLVM.Context
import qualified LLVM.ExecutionEngine as EE
import LLVM.Module as Mod
import LLVM.PassManager

foreign import ccall "dynamic" haskFun :: FunPtr (IO Double) -> IO Double

run :: FunPtr a -> IO Double
run fn = haskFun (castFunPtr fn :: FunPtr (IO Double))

jit :: Context -> (EE.MCJIT -> IO a) -> IO a
jit c = EE.withMCJIT c optLevel model ptrElim fastIns
  where optLevel = Just 0   -- optimization level
        model    = Nothing  -- code model ( Default )
        ptrElim  = Nothing  -- frame pointer elimination
        fastIns  = Nothing  -- fast instruction selection

verifyAndRecover :: Mod.Module -> IO String
verifyAndRecover m = catch (verify m >> pure "") (\e -> pure
  $ "\nVerification error:\n" ++ show (e :: SomeException) ++ "\n")

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec {optLevel = Just 3}

runJIT :: AST.Module -> IO AST.Module
runJIT mod = withContext $ \context -> jit context $ \executionEngine ->
  withModuleFromAST context mod $ \m -> withPassManager passes $ \pm -> do
    -- runPassManager pm m
    verify m
    verifyErr <- verifyAndRecover m
    optmod <- moduleAST m
    runInLLVMASM m executionEngine
    pure optmod

runInLLVMASM :: EE.ExecutionEngine e (FunPtr a)
             => Module -> e -> IO ()
runInLLVMASM m executionEngine = do
  s <- moduleLLVMAssembly m
  ByteString.putStrLn s
  EE.withModuleInEngine executionEngine m $ \ee ->
    EE.getFunction ee "anon5" >>= \case
      Just fn -> run fn >>= \res -> putStrLn $ "Evaluated to: " ++ show res
      Nothing -> putStrLn "Could not evaluate main function"
