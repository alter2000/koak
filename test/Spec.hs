{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Hedgehog
import qualified Hedgehog.Gen as G
import qualified Hedgehog.Range as R

tests :: IO Bool
tests = checkParallel $ Group "Test.Example"
  [ ("prop_reverse", property $ do
      xs <- forAll $ G.list (R.linear 0 10) G.alpha
      reverse (reverse xs) === xs)
  ]

main :: IO ()
main = hspec $ do
  describe "Parser.AST" $ do
    it "works idk explain what it should do" $ do
      1 + 1 `shouldBe` 2
