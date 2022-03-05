module Tests.Parser.ASTParser
  where

import Test.Hspec

import Parser.ASTParser
import Types.AST

basicParserTest :: SpecWith ()
basicParserTest = describe "Parser.ASTParser" $ do
    it "should make 123 from \"123\"" $ do
      parse "123" `shouldBe` Right (mkBlock [mkLiteral 123.0])
    it "should make 123 from \"(123)\"" $ do
      parse "(123)" `shouldBe` Right (mkBlock [mkLiteral 123.0])
    it "should make 123 from \"123.0\"" $ do
      parse "123.0" `shouldBe` Right (mkBlock [mkLiteral 123.0])
    it "should make 123 from \"(123.0)\"" $ do
      parse "(123.0)" `shouldBe` Right (mkBlock [mkLiteral 123.0])
    it "should make foo identifier from \"foo\"" $ do
      parse "foo" `shouldBe` Right (mkBlock [mkIdentifier "foo"])
    it "should make foo identifier from \"(foo)\"" $ do
      parse "(foo)" `shouldBe` Right (mkBlock [mkIdentifier "foo"])
    it "should make extern foo from \"extern foo()\"" $ do
      parse "extern foo()" `shouldBe` Right (mkBlock [mkExtern "foo" []])
    it "should make extern foo a b from \"extern foo(a b)\"" $ do
      parse "extern foo(a b)" `shouldBe` Right (mkBlock [mkExtern "foo" ["a", "b"]])