{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Exception ( handle )
import System.Environment ( getArgs )

-- import Lib.AST ( primEnv, printAST )
import Lib.Lib
import Util ( halt, interpret, repl )

main :: IO ()
main = handle halt $ getArgs >>= \case
  [] -> repl primEnv
  ["-i"] -> repl primEnv
  as | "-i" `elem` as && "-i" `isLast` as ->
    (dropI as >>= interpret primEnv) >>= repl . snd
  as -> dropI as >>= interpret primEnv
    >>= (printAST putStrLn . fst)

isLast :: Eq a => a -> [a] -> Bool
isLast _ [] = False
isLast p [x] = p == x
isLast p (_:xs) = isLast p xs

dropI :: [String] -> IO [String]
dropI = pure . filter (/= "-i")
