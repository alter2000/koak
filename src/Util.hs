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
import Parser.ParseError

-- TODO
import Types.Codegen
import Lib.JIT

cgen :: [Phrase] -> IO AST.Module
cgen = evalCodegen . codegenModule >=> runJIT

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

threadMod :: [Phrase] -> Repl Bool
threadMod ast = do
  mod <- lift get
  (r, m') <- liftIO $ handle (except >>> (>> pure (True, mod))) $ do
    m' <- cgen ast >>= runJIT
    pure (True, m')
  lift (put m') >> pure r


interpretFile :: AST.Module -> FilePath -> IO AST.Module
interpretFile mod f = readFile f >>= either
  (pExcept (hPutStrLn stderr) (exitWith (ExitFailure 84)))
  (evalFile mod) . parseFile f

evalFile :: AST.Module -> [Phrase] -> IO AST.Module
evalFile mod  [] = pure mod
evalFile   _ ast = cgen ast >>= runJIT

-- | interpret list of files, then return resulting env and return value
interpret :: AST.Module -> [String] -> IO AST.Module
interpret mod [] = pure mod
interpret mod [fp] = interpretFile mod fp
interpret mod (fp:fps) = interpretFile mod fp >>= flip interpret fps

till :: Monad m => m Bool -> m ()
till p = go where go = p >>= flip when go
