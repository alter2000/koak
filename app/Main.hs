{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Exception ( handle )
import System.Environment ( getArgs )

import Util
import Text.Pretty.Simple

main :: IO ()
main = handle halt $ getArgs >>= \case
  [] -> repl primEnv
  ["-i"] -> repl primEnv
  as -> interpret primEnv as >>= pPrint
  -- as | "-i" `elem` as && "-i" `isLast` as ->
  --   (dropI as >>= interpret primEnv) >>= repl

isLast :: Eq a => a -> [a] -> Bool
isLast _ [] = False
isLast p [x] = p == x
isLast p (_:xs) = isLast p xs

dropI :: [String] -> IO [String]
dropI = pure . filter (/= "-i")
