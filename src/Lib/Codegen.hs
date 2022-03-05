{-# LANGUAGE OverloadedStrings #-}

module Lib.Codegen where

import qualified Data.Map.Strict as Map
import Control.Monad.State

import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

import Types.Codegen

-- Module {{{
addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn . GlobalDefinition $ functionDefaults
  { name        = Name $ packShort label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = body
  }

external ::  Type -> String -> Map.Map Type Name -> LLVM ()
external retty label argts = addDefn . GlobalDefinition $ functionDefaults
  { name       = Name $ packShort label
  , linkage    = L.External
  , parameters = ([Parameter t n [] | (t, n) <- Map.toList argts], False)
  , returnType = retty
  , basicBlocks = []
  }
-- }}}

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = Map.insert var x lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case Map.lookup var syms of
    Just x  -> pure x
    Nothing -> error $ "Local variable not in scope: " <> show var


fadd, fsub, fmul, fdiv :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd noFastMathFlags a b []
fsub a b = instr $ FSub noFastMathFlags a b []
fmul a b = instr $ FMul noFastMathFlags a b []
fdiv a b = instr $ FDiv noFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

call :: Operand -> [Operand] -> Codegen Operand
call fn as = instr $ Call Nothing CC.C [] (Right fn) (toArgs as) [] []
  where toArgs = map (\x -> (x, []))

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

br :: Name -> Codegen (Named Terminator)
br x = terminator . Do $ Br x []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator . Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator . Do $ Ret (Just val) []
