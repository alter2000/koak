module Parser.ASTParser
  where

import Control.Applicative
import Data.Maybe

import Types.Pos
import Types.AST
import Parser.ParserImpl
import Parser.ParseError

data ASTDerivs = ASTDerivs
  { adNatural     :: Result ASTDerivs ASTF
  , adDecimal     :: Result ASTDerivs ASTF
  , adLiteral     :: Result ASTDerivs ASTF
  , adIdentifier  :: Result ASTDerivs ASTF
  , adPrimary     :: Result ASTDerivs ASTF
  , adFuncCall    :: Result ASTDerivs ASTF
  , adUnaryOp     :: Result ASTDerivs ASTF
  , adPostfix     :: Result ASTDerivs ASTF
  , adExpression  :: Result ASTDerivs ASTF
  , adArithmetics :: Result ASTDerivs ASTF
  , adTerm        :: Result ASTDerivs ASTF
  , adFactor      :: Result ASTDerivs ASTF
  , adFlowCont    :: Result ASTDerivs ASTF
  , adExtern      :: Result ASTDerivs Defn
  , adFuncDecl    :: Result ASTDerivs Defn
  , adDefn        :: Result ASTDerivs Phrase
  , adIfExpr      :: Result ASTDerivs ASTF
  , adForExpr     :: Result ASTDerivs ASTF
  , adWhileExpr   :: Result ASTDerivs ASTF
  , adComp        :: Result ASTDerivs ASTF
  , adTopLevel    :: Result ASTDerivs [Phrase]
  , adLetExpr     :: Result ASTDerivs ASTF

  , adIgnore     :: Result ASTDerivs String

  , adChar        :: Result ASTDerivs Char
  , adPos         :: Pos
  }

instance Derivs ASTDerivs where
  dvChar = adChar
  dvPos  = adPos

evalDerivs :: Pos -> String -> ASTDerivs
evalDerivs pos s = d where
  d = ASTDerivs
    { adChar = case s of
     (c:s') -> Parsed c (evalDerivs (nextPos pos c) s') $ nullError d
     [] -> NoParse $ eofError d
    , adPos    = pos
    , adNatural     = pNatural d
    , adDecimal     = pDecimal d
    , adLiteral     = pLiteral d
    , adIdentifier  = pIdentifier d
    , adPrimary     = pPrimary d
    , adFuncCall    = pFuncCall d
    , adUnaryOp     = pUnaryOp d
    , adPostfix     = pPostfix d
    , adExpression  = pExpression d
    , adArithmetics = pArithmetics d
    , adTerm        = pTerm d
    , adFactor      = pFactor d
    , adFlowCont    = pFlowCont d
    , adExtern      = pExtern d
    , adFuncDecl    = pFuncDecl d
    , adDefn        = pDefn d
    , adIfExpr      = pIfExpr d
    , adForExpr     = pForExpr d
    , adWhileExpr   = pWhileExpr d
    , adComp        = pComp d
    , adTopLevel    = pTopLevel d
    , adLetExpr     = pLetExpr d

    , adIgnore = pIgnore d
    }

parse :: String -> Either ParseError [Phrase]
parse s = case pExpr $ evalDerivs (Pos "<stdin>" 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e

testParser :: (ASTDerivs -> Result ASTDerivs ASTF) -> String -> Either ParseError ASTF
testParser p s = case p $ evalDerivs (Pos "<stdin>" 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e

parseFile :: FilePath -> String -> Either ParseError [Phrase]
parseFile fname s = case pExpr $ evalDerivs (Pos fname 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e

pExpr :: ASTDerivs -> Result ASTDerivs [Phrase]
P pExpr = P adTopLevel <* eof'

pIgnore :: ASTDerivs -> Result ASTDerivs String
P pIgnore = concat <$> (spaces *> many (P pComment <* spaces))
  <?> "non-code"

pTopLevel :: ASTDerivs -> Result ASTDerivs [Phrase]
P pTopLevel = (P adDefn `sepBy` (spaces >> char ';' >> spaces)) <* many (char ';')

pDefn :: ASTDerivs -> Result ASTDerivs Phrase
P pDefn = (DefnPhrase <$> (P adExtern <|> P adFuncDecl)) <|> (ExprPhrase <$> P adExpression)

pExpression :: ASTDerivs -> Result ASTDerivs ASTF
P pExpression = do
  first <- P adArithmetics <* spaces
  foll <- many ((char '=' <* spaces) *> (P adArithmetics <* spaces))
  return (foldl (mkBinOp Assignment) first foll)
  <?> "Assignment"

pArithmetics :: ASTDerivs -> Result ASTDerivs ASTF
P pArithmetics = do
  first <- P adTerm <* spaces
  foll <- many ((,) <$> (oneOf "+-" <* spaces) <*> (P adTerm <* spaces))
  return (foldl foldFn first foll) <?> "Expression"
    where
      foldFn acc ('-', e) = mkBinOp Minus acc e
      foldFn acc ('+', e) = mkBinOp Plus acc e
      foldFn _ _ = undefined

pTerm :: ASTDerivs -> Result ASTDerivs ASTF
P pTerm = do
    first <- P adComp <* spaces
    foll <- many ((,) <$> (oneOf "*/" <* spaces) <*> (P adComp <* spaces))
    return (foldl foldFn first foll) <?> "Term"
        where
            foldFn acc ('/', e) = mkBinOp Divide acc e
            foldFn acc ('*', e) = mkBinOp Times acc e
            foldFn _ _ = undefined

pComp :: ASTDerivs -> Result ASTDerivs ASTF
P pComp = do
  a <- P adFactor <* spaces
  op <- (string ">" <|> string "<" <|> string "==" <|> string "!=") <* spaces
  b <- P adFactor <* spaces
  return $ case op of
    "<" -> mkBinOp LessThan a b
    ">" -> mkBinOp MoreThan a b
    "==" -> mkBinOp Equality a b
    "!=" -> mkBinOp Difference a b
    _ -> undefined
  <|> P adFactor
  <?> "Comp"

pFactor :: ASTDerivs -> Result ASTDerivs ASTF
P pFactor = P adFlowCont <|> P adUnaryOp <|> parens (P adExpression)
  <?> "Factor"

pFlowCont :: ASTDerivs -> Result ASTDerivs ASTF
P pFlowCont = P adIfExpr <|> P adLetExpr <|> P adForExpr <|> P adWhileExpr

pIfExpr :: ASTDerivs -> Result ASTDerivs ASTF
P pIfExpr = do
  string "if" >> spaces
  cond <- P adExpression <* spaces
  string "then" >> spaces
  thenBody <- P adExpression
  string "else" >> spaces
  elseBody <- P adExpression
  return $ mkIfExpr cond thenBody elseBody

pLetExpr :: ASTDerivs -> Result ASTDerivs ASTF
P pLetExpr = do
  string "var" <* spaces
  defs <- (do
    var <- identifier <* spaces
    char '=' <* spaces
    val <- P adExpression
    return (var, val)) `sepBy` (char ',' <* spaces)
  string "in" <* spaces
  body <- P adExpression
  pure $ foldr (uncurry mkLet) body defs

pForExpr :: ASTDerivs -> Result ASTDerivs ASTF
P pForExpr = mkForExpr
  <$> (string "for" *> spaces *> identifier)
  <*> ((spaces *> char '=' *> spaces) *> P adExpression)
  <*> ((string "," *> spaces) *> P adExpression)
  <*> ((string "," *> spaces) *> P adExpression)
  <*> ((spaces *> string "in" *> spaces) *> P adExpression)
  <?> "FOR"

pWhileExpr :: ASTDerivs -> Result ASTDerivs ASTF
P pWhileExpr = do
  string "while" >> spaces
  cond <- P adExpression
  string "do" >> spaces
  mkWhileExpr cond <$> P adExpression

pUnaryOp :: ASTDerivs -> Result ASTDerivs ASTF
P pUnaryOp =
  ((mkUnOp Invert <$ char '!' <|> mkUnOp Neg <$ char '-')
  <*> (P adUnaryOp <|> P adPostfix))
  <|> P adPostfix
  <?> "Unary"

pPostfix :: ASTDerivs -> Result ASTDerivs ASTF
P pPostfix = P adFuncCall <|> P adPrimary
  <?> "Postfix"

pExtern :: ASTDerivs -> Result ASTDerivs Defn
P pExtern = do
 string "extern" >> some space
 name <- identifier
 args <- parens (identifier `sepBy` spaces)
 pure $ mkExtern name args

pFuncDecl :: ASTDerivs -> Result ASTDerivs Defn
P pFuncDecl = do
  string "def" >> some space
  name <- identifier
  args <- parens (identifier `sepBy` spaces) <* spaces
  body <- P adExpression
  pure $ mkFunction name args body

pFuncCall :: ASTDerivs -> Result ASTDerivs ASTF
P pFuncCall = do
  name <- identifier
  arg <- parens $ optional $ P adExpression `sepBy` (spaces >> char ',' >> spaces)
  return $ mkCall name (fromMaybe [] arg)
  <?> "Call"

pPrimary :: ASTDerivs -> Result ASTDerivs ASTF
P pPrimary = P adLiteral <|> P adIdentifier <?> "Primary"

pIdentifier :: ASTDerivs -> Result ASTDerivs ASTF
P pIdentifier = mkIdentifier <$> identifier <?> "Identifier"

{- HLINT ignore "Avoid restricted function" -}
pLiteral :: ASTDerivs -> Result ASTDerivs ASTF
P pLiteral = P adDecimal <|> P adNatural <?> "Literal"

pNatural :: ASTDerivs -> Result ASTDerivs ASTF
P pNatural = mkLiteral . read <$> some digit <?> "integer"

pDecimal :: ASTDerivs -> Result ASTDerivs ASTF
P pDecimal = mkLiteral . read <$> do
    n <- many digit
    dt <- char '.'
    d <- some digit
    return (n ++ [dt] ++ d)
  <?> "double"

parens :: Parser ASTDerivs a -> Parser ASTDerivs a
parens p = char '(' *> P adIgnore `around` p <* char ')' <?> "parentheses"

pComment :: ASTDerivs -> Result ASTDerivs String
P pComment = char '#' *> many (noneOf "\n") <?> "comment"
