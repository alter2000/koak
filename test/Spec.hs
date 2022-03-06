{-# LANGUAGE OverloadedStrings #-}

-- READ TODO.MD P L S

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

import Tests.Parser.ASTParser

tests :: IO Bool
tests = checkParallel $ Group "Test.Example"
  [ ("prop_reverse", property $ do
      xs <- forAll $ G.list (R.linear 0 10) G.alpha
      reverse (reverse xs) === xs)
  ]

main :: IO ()
main = hspec $ do
  literalsParserTest
  keywordsParserTest
  ifExprParserTest
  forExprParserTest
  whileExprParserTest
  functionCallParserTest
  unaryOpParserTest
  sequencingParserTest
