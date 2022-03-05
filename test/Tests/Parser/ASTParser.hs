module Tests.Parser.ASTParser
  where

import Test.Hspec

import Parser.ASTParser
import Types.AST

literalsParserTest :: SpecWith ()
literalsParserTest = describe "Parser.ASTParser" $ do
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

keywordsParserTest :: SpecWith ()
keywordsParserTest = describe "Parser.ASTParser" $ do
  it "should make extern foo from \"extern foo()\"" $ do
    parse "extern foo()" `shouldBe` Right (mkBlock [mkExtern "foo" []])
  it "should make extern foo a b from \"extern foo(a b)\"" $ do
    parse "extern foo(a b)" `shouldBe` Right (mkBlock [mkExtern "foo" ["a", "b"]])
  it "should make def foo from \"def foo() 123.0\"" $ do
    parse "def foo() 123.0" `shouldBe` Right (mkBlock [mkFunction "foo" [] (mkLiteral 123.0)])
  it "should make def foo a b from \"def foo(a b) 123.0\"" $ do
    parse "def foo(a b) 123.0" `shouldBe` Right (mkBlock [mkFunction "foo" ["a", "b"] (mkLiteral 123.0)])

flowControlParserTest :: SpecWith ()
flowControlParserTest = describe "Parser.ASTParser" $ do
  it "should make if foo then bar else 123" $ do
    parse "if foo then bar else 123"
    `shouldBe`
    Right (mkBlock [mkIfExpr (mkIdentifier "foo") (mkIdentifier "bar") (mkLiteral 123.0)])
  it "should make for a = 1, a < 3, a + 1 in 123" $ do
    parse "for a = 1, a < 3, a + 1 in 123"
    `shouldBe`
    Right (mkBlock [mkIfExpr (mkIdentifier "foo") (mkIdentifier "bar") (mkLiteral 123.0)])
