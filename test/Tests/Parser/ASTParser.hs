module Tests.Parser.ASTParser
  where

import Test.Hspec

import Parser.ASTParser
import Types.AST

literalsParserTest :: SpecWith ()
literalsParserTest = describe "Literals" $ do
  it "should make 123 from \"123\"" $ do
    parse "123" `shouldBe` Right [ExprPhrase $ mkLiteral 123.0]
  it "should make 123 from \"(123)\"" $ do
    parse "(123)" `shouldBe` Right [ExprPhrase $ mkLiteral 123.0]
  it "should make 123 from \"123.0\"" $ do
    parse "123.0" `shouldBe` Right [ExprPhrase $ mkLiteral 123.0]
  it "should make 123 from \"(123.0)\"" $ do
    parse "(123.0)" `shouldBe` Right [ExprPhrase $ mkLiteral 123.0]
  it "should make foo identifier from \"foo\"" $ do
    parse "foo" `shouldBe` Right [ExprPhrase $ mkIdentifier "foo"]
  it "should make foo identifier from \"(foo)\"" $ do
    parse "(foo)" `shouldBe` Right [ExprPhrase $ mkIdentifier "foo"]

keywordsParserTest :: SpecWith ()
keywordsParserTest = describe "Keywords" $ do
  it "should make extern foo from \"extern foo()\"" $ do
    parse "extern foo()" `shouldBe` Right [DefnPhrase $ mkExtern "foo" []]
  it "should make extern foo a b from \"extern foo(a b)\"" $ do
    parse "extern foo(a b)" `shouldBe` Right [DefnPhrase $ mkExtern "foo" ["a", "b"]]
  it "should make def foo from \"def foo() 123.0\"" $ do
    parse "def foo() 123.0"
    `shouldBe` Right [DefnPhrase $ mkFunction "foo" [] (mkLiteral 123.0)]
  it "should make def foo a b from \"def foo(a b) 123.0\"" $ do
    parse "def foo(a b) 123.0"
    `shouldBe` Right [DefnPhrase $ mkFunction "foo" ["a", "b"] (mkLiteral 123.0)]

ifExprParserTest :: SpecWith ()
ifExprParserTest = describe "If Expressions" $ do
  it "should make if from \"if foo>2 then bar else 123\"" $ do
    parse "if foo>2 then bar else 123"
    `shouldBe`
    Right [ExprPhrase $ mkIfExpr
      (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
      (mkIdentifier "bar")
      (mkLiteral 123.0)]
  it "should make same if with bracketed expression" $ do
    parse "if (foo>2) then bar else 123"
      `shouldBe`
      Right [ExprPhrase $ mkIfExpr
        (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
        (mkIdentifier "bar")
        (mkLiteral 123.0)]
    parse "if foo>2 then (bar) else 123"
      `shouldBe`
      Right [ExprPhrase $ mkIfExpr
        (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
        (mkIdentifier "bar")
        (mkLiteral 123.0)]
    parse "if foo>2 then bar else (123)"
      `shouldBe`
      Right [ExprPhrase $ mkIfExpr
        (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
        (mkIdentifier "bar")
        (mkLiteral 123.0)]
    parse "if (foo>2) then (bar) else (123)"
      `shouldBe`
      Right [ExprPhrase $ mkIfExpr
        (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
        (mkIdentifier "bar")
        (mkLiteral 123.0)]

forExprParserTest :: SpecWith ()
forExprParserTest = describe "For Expressions" $ do
  it "should make for from \"for a = 1, a < 3, a + 1 in 123*2\"" $ do
    parse "for a = 1, a < 3, a + 1 in 123*2"
    `shouldBe`
    Right [ExprPhrase $ mkForExpr "a" (mkLiteral 1)
      (mkBinOp LessThan (mkIdentifier "a") (mkLiteral 3))
      (mkBinOp Plus (mkIdentifier "a") (mkLiteral 1))
      (mkBinOp Times (mkLiteral 123) (mkLiteral 2))]
  it "should make same for with bracketed expressions" $ do
    parse "for a = (1), (a < 3), (a + 1) in (123*2)"
      `shouldBe`
      Right [ExprPhrase $ mkForExpr "a" (mkLiteral 1)
        (mkBinOp LessThan (mkIdentifier "a") (mkLiteral 3))
        (mkBinOp Plus (mkIdentifier "a") (mkLiteral 1))
        (mkBinOp Times (mkLiteral 123) (mkLiteral 2))]


whileExprParserTest :: SpecWith ()
whileExprParserTest = describe "While Expressions" $ do
  it "should make while from \"while a < 3 do a+1\"" $ do
    parse "while a < 3 do a+1"
      `shouldBe`
      Right [ExprPhrase $ mkWhileExpr
        (mkBinOp LessThan (mkIdentifier "a") (mkLiteral 3))
        (mkBinOp Plus (mkIdentifier "a") (mkLiteral 1))]
  it "should make same while with bracketed expressions" $ do
    parse "while (a < 3) do (a+1)"
      `shouldBe`
      Right [ExprPhrase $ mkWhileExpr
        (mkBinOp LessThan (mkIdentifier "a") (mkLiteral 3))
        (mkBinOp Plus (mkIdentifier "a") (mkLiteral 1))]

functionCallParserTest :: SpecWith ()
functionCallParserTest = describe "Function call" $ do
  it "should make foo function call from \"foo(foo())\"" $ do
    parse "foo()" `shouldBe` Right [ExprPhrase $ mkCall "foo" []]
  it "should make foo function call from \"foo(1, b, C)\"" $ do
    parse "foo(1, b, C)"
      `shouldBe`
      Right [ExprPhrase $ mkCall "foo" [mkLiteral 1, mkIdentifier "b", mkIdentifier "C"]]
  it "should make same function call with bracketed expressions" $ do
    parse "foo((1), (b), (C))"
      `shouldBe`
      Right [ExprPhrase $ mkCall "foo" [mkLiteral 1, mkIdentifier "b", mkIdentifier "C"]]
  it "should make nested call" $ do
    parse "foo(bar(a), 123)"
      `shouldBe`
      Right [ExprPhrase $ mkCall "foo" [
        mkCall "bar" [mkIdentifier "a"],
        mkLiteral 123
      ]]

unaryOpParserTest :: SpecWith ()
unaryOpParserTest = describe "Unary Operators" $ do
  it "should make Neg" $ do
    parse "-1"
      `shouldBe` Right [ExprPhrase $ mkUnOp Neg (mkLiteral 1)]
  it "should make Invert" $ do
    parse "!a"
      `shouldBe` Right [ExprPhrase $ mkUnOp Invert (mkIdentifier "a")]
  it "should make double Invert" $ do
    parse "!!a"
      `shouldBe` Right [ExprPhrase $ mkUnOp Invert $ mkUnOp Invert (mkIdentifier "a")]
  it "should make double Neg" $ do
    parse "--1"
      `shouldBe` Right [ExprPhrase $ mkUnOp Neg $ mkUnOp Neg (mkLiteral 1)]
  it "should mix Neg and Invert " $ do
    parse "-!a"
      `shouldBe` Right [ExprPhrase $ mkUnOp Neg $ mkUnOp Invert (mkIdentifier "a")]

sequencingParserTest :: SpecWith ()
sequencingParserTest = describe "Sequencing" $ do
  it "should make two expressions" $ do
    parse "if foo>2 then bar else 123; for a = 1, a < 3, a + 1 in 123*2"
      `shouldBe` Right [ExprPhrase $ mkIfExpr
      (mkBinOp MoreThan (mkIdentifier "foo") (mkLiteral 2))
      (mkIdentifier "bar")
      (mkLiteral 123.0),
      ExprPhrase $ mkForExpr "a" (mkLiteral 1)
      (mkBinOp LessThan (mkIdentifier "a") (mkLiteral 3))
      (mkBinOp Plus (mkIdentifier "a") (mkLiteral 1))
      (mkBinOp Times (mkLiteral 123) (mkLiteral 2))]
  it "should do nothing" $ do
    parse "" `shouldBe` Right []