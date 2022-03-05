module Lib.Lib
  where

import Data.List as L
import qualified Data.Map.Strict as M

import Types.AST
-- ABSOLUTELY USELESS JUST TO COMPILE

runStep :: AST' -> Env -> IO (AST', Env)
runStep = undefined

printAST :: (String -> IO ()) -> AST' -> IO ()
printAST f = f . show

primEnv :: Env
primEnv = Env M.empty

list :: [AST'] -> AST'
list = mkBlock
