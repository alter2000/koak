{-# LANGUAGE LambdaCase #-}
-- | Random useful functions most probably used as glue between other modules
module Util
    where

import System.Exit ( exitWith, ExitCode(..) )
import System.IO
import Control.Exception
import System.Console.Haskeline
import Control.Arrow

import Control.Monad
import Control.Monad.State
import Data.Char
import qualified LLVM.AST as AST

import Types.Exceptions ( HALError )
import Types.AST
import Parser.ASTParser
import Lib.Emit
import Parser.ParseError

-- TODO
import Types.Codegen (emptyModule)

-- | needs more flesh, usable while inside 'Control.Monad.Except.ExceptT'
pExcept :: (String -> IO ()) -> IO a -> ParseError -> IO a
pExcept f = flip $ displayException >>> f >>> (>>)

-- | separate from 'parserExcept' to carry over elsewhere
replExcept :: HALError -> IO ()
replExcept = hPutStrLn stderr . displayException

-- | catch /any/ exception
except :: SomeException -> IO ()
except = hPutStrLn stderr . displayException

-- | actually stop the program
halt :: SomeException -> IO a
halt e = except e >> exitWith (ExitFailure 84)

type Repl = InputT (StateT AST.Module IO)

settings :: Settings (StateT AST.Module IO)
settings = Settings
  { complete = noCompletion
  , historyFile = Just "./koak_history"
  , autoAddHistory = True
  }


primEnv :: AST.Module
primEnv = emptyModule "repl"

-- completeExpr :: CompletionFunc (StateT Env IO)
-- completeExpr (bef, _after) = do
--   (Env e) <- get
--   let comps = filter (before `isPrefixOf`)
--         $ M.keys e <> ["define", "let", "lambda"]
--   pure (remainder, simpleCompletion <$> comps)
--     where (before, remainder) = let (revbef, revrem) = span isAlphaNum bef
--                                 in (reverse revbef, reverse revrem)

repl :: AST.Module -> IO ()
repl env = flip evalStateT env $ runInputT settings
  $ till $ getInputLine "><> :: " >>= \case
    Nothing -> pure False
    Just i -> handleInput i

handleInput :: [Char] -> Repl Bool
handleInput i  | filter (not . isSpace) i == "" = pure True
  | otherwise = do
    pp <- getExternalPrint
    either (liftIO . pExcept pp (pure True)) threadMod (parse i)

threadMod :: AST' -> Repl Bool
threadMod ast = do
  mod <- lift get
  (r, m') <- liftIO $ handle (except >>> (>> pure (True, mod))) $ do
    m' <- codegen mod [ast]
    pure (True, m')
  lift (put m') >> pure r


-- prettyPrintEnv :: InputT (StateT Env IO) ()
-- prettyPrintEnv = liftIO . mapM_ putStrLn . showKeyVal
--   . M.toList . dropPrimitives . getEnv =<< lift get
--   where showKeyVal :: [(VarName, AST')] -> [String]
--         showKeyVal = fmap $ \(a, b) -> a <> " : " <> show b
--         dropPrimitives = flip M.difference $ getEnv primEnv


interpretFile :: AST.Module -> FilePath -> IO AST.Module
interpretFile mod f = readFile f >>= either
  (pExcept (hPutStrLn stderr) (exitWith (ExitFailure 84)))
  (evalFile mod) . parseFile f

evalFile :: AST.Module -> [AST'] -> IO AST.Module
evalFile env  [] = pure env
evalFile env ast = codegen env ast

-- | interpret list of files, then return resulting env and return value
interpret :: AST.Module -> [String] -> IO AST.Module
interpret mod [] = pure mod
interpret mod [fp] = interpretFile mod fp
interpret mod (fp:fps) = interpretFile mod fp >>= flip interpret fps

till :: Monad m => m Bool -> m ()
till p = go where go = p >>= flip when go
