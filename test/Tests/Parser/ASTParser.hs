module Tests.Parser.ASTParser
  where

import Test.Hspec

import Parser.ASTParser
import Types.AST

literalsParserTest :: SpecWith ()
literalsParserTest = describe "Literals" $ do
  it "should make 123 from \"123\"" $ do
    parse "123" `shouldBe` Right [mkLiteral 123.0]
  it "should make 123 from \"(123)\"" $ do
    parse "(123)" `shouldBe` Right [mkLiteral 123.0]
  it "should make 123 from \"123.0\"" $ do
    parse "123.0" `shouldBe` Right [mkLiteral 123.0]
  it "should make 123 from \"(123.0)\"" $ do
    parse "(123.0)" `shouldBe` Right [mkLiteral 123.0]
  it "should make foo identifier from \"foo\"" $ do
    parse "foo" `shouldBe` Right [mkIdentifier "foo"]
  it "should make foo identifier from \"(foo)\"" $ do
    parse "(foo)" `shouldBe` Right [mkIdentifier "foo"]

keywordsParserTest :: SpecWith ()
keywordsParserTest = describe "Keywords" $ do
  it "should make extern foo from \"extern foo()\"" $ do
    parse "extern foo()" `shouldBe` Right [mkExtern "foo" []]
  it "should make extern foo a b from \"extern foo(a b)\"" $ do
    parse "extern foo(a b)" `shouldBe` Right [mkExtern "foo" ["a", "b"]]
  it "should make def foo from \"def foo() 123.0\"" $ do
    parse "def foo() 123.0"
    `shouldBe` Right [mkFunction "foo" [] (mkLiteral 123.0)]
  it "should make def foo a b from \"def foo(a b) 123.0\"" $ do
    parse "def foo(a b) 123.0"
    `shouldBe` Right [mkFunction "foo" ["a", "b"] (mkLiteral 123.0)]

ifExprParserTest :: SpecWith ()
ifExprParserTest = describe "If Expressions" $ do
  it "should make if from \"if foo>2 then bar else 123\"" $ do
    parse "if foo>2 then bar else 123"
    `shouldBe`
    Right [mkIfExpr
      (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
      (mkIdentifier "bar")
      (mkLiteral 123.0)]

forExprParserTest :: SpecWith ()
forExprParserTest = describe "For Expressions" $ do
  it "should make for from \"for a = 1, a < 3, a + 1 in 123*2\"" $ do
    parse "for a = 1, a < 3, a + 1 in 123*2"
    `shouldBe`
    Right [mkForExpr "a" (mkLiteral 1)
      (mkBinOp LessThan (mkIdentifier "a") (mkLiteral 3))
      (mkBinOp Plus (mkIdentifier "a") (mkLiteral 1))
      (mkBinOp Times (mkLiteral 123) (mkLiteral 2))]
