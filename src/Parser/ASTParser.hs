module Parser.ASTParser
  where

import Control.Applicative
import Data.Functor
import Data.Foldable
import Data.Maybe

import Types.AST
import RecursionSchemes
import Types.Pos
import Parser.ParserImpl
import Parser.ParseError

data ASTDerivs = ASTDerivs
  { adNatural     :: Result ASTDerivs AST'
  , adDecimal     :: Result ASTDerivs AST'
  , adLiteral     :: Result ASTDerivs AST'
  , adIdentifier  :: Result ASTDerivs AST'
  , adPrimary     :: Result ASTDerivs AST'
  , adFuncCall    :: Result ASTDerivs AST'
  , adUnaryOp     :: Result ASTDerivs AST'
  , adPostfix     :: Result ASTDerivs AST'
  , adExpression  :: Result ASTDerivs AST'
  , adTerm        :: Result ASTDerivs AST'
  , adFactor      :: Result ASTDerivs AST'
  , adBlock       :: Result ASTDerivs AST'
  , adAssign      :: Result ASTDerivs AST'
  , adExtern      :: Result ASTDerivs AST'
  , adFuncDecl    :: Result ASTDerivs AST'
  -- , adBool   :: Result ASTDerivs AST'
  -- , adString :: Result ASTDerivs AST'

   , adIgnore     :: Result ASTDerivs String
  -- , adElem  :: Result ASTDerivs AST'

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
    , adTerm        = pTerm d
    , adFactor      = pFactor d
    , adBlock       = pBlock d
    , adAssign      = pAssignment d
    , adExtern      = pExtern d
    , adFuncDecl    = pFuncDecl d

    , adIgnore = pIgnore d
    -- , adElem   = pElem d
    }

pExpr :: ASTDerivs -> Result ASTDerivs AST'
pExpr = pExpression

parse :: String -> Either ParseError AST'
parse s = case pExpr $ evalDerivs (Pos "<stdin>" 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e

parseFile :: FilePath -> String -> Either ParseError [AST']
parseFile fname s = case pExpr $ evalDerivs (Pos fname 1 1) s of
    Parsed v _ _ -> Right v
    NoParse e -> Left e
  where P pExpr = undefined -- P adIgnore *> some (P adElem <* P adIgnore) <* eof'

pIgnore :: ASTDerivs -> Result ASTDerivs String
P pIgnore = concat <$>
    (optional spaces *> many (P pComment <* spaces))
              <?> "non-code"

pBlock :: ASTDerivs -> Result ASTDerivs AST'
P pBlock = pIfExpr <|> pForExpr <|> pWhileExpr <|> Fix . Block <$> blockCont
  <?> "Block"
  where
    blockCont :: Parser ASTDerivs [AST']
    blockCont = (:) <$> P adExpression <*> many (char ':' *> P adExpression)

    pIfExpr :: Parser ASTDerivs AST'
    pIfExpr = do
      string "if" >> spaces
      cond <- P adExpression
      string "then" >> spaces
      thenBody <- blockCont
      elseBody <- optional (spaces *> string "else" *> spaces *> blockCont)
      return $ Fix $ IfExpr cond thenBody (fromMaybe [] elseBody)

    pForExpr :: Parser ASTDerivs AST'
    pForExpr = do
      string "for" *> spaces
      assignment <- P adAssign
      string "," >> spaces
      cond <- P adExpression
      string "," >> spaces
      inc <- P adExpression
      spaces
      Fix . ForExpr assignment cond inc <$> blockCont

    pWhileExpr :: Parser ASTDerivs AST'
    pWhileExpr = do
      string "while" >> spaces
      cond <- P adExpression
      string "do" >> spaces
      Fix . WhileExpr cond <$> blockCont

pAssignment :: ASTDerivs -> Result ASTDerivs AST'
P pAssignment = do
  str <- identifier
  char '='
  exp <- P adExpression
  return $ Fix (Assignment str exp)

pExpression :: ASTDerivs -> Result ASTDerivs AST'
P pExpression = {- P adAssign <|> -} do
  first <- P adTerm
  foll <- many ((,) <$> oneOf "+-" <*> P adTerm)
  return (foldl foldFn first foll) <?> "Expression"
    where
      foldFn acc ('-', e) = Fix $ BinOp Minus acc e
      foldFn acc ('+', e) = Fix $ BinOp Plus acc e
      foldFn _ _ = undefined

pTerm :: ASTDerivs -> Result ASTDerivs AST'
P pTerm = do
    first <- P adFactor
    foll <- many ((,) <$> oneOf "*/" <*> P adFactor)
    return (foldl foldFn first foll) <?> "Term"
        where
            foldFn acc ('/', e) = Fix $ BinOp Divide acc e
            foldFn acc ('*', e) = Fix $ BinOp Times acc e
            foldFn _ _ = undefined

pFactor :: ASTDerivs -> Result ASTDerivs AST'
P pFactor = P adUnaryOp <|> parens (P adExpression) <?> "Factor"

pUnaryOp :: ASTDerivs -> Result ASTDerivs AST'
P pUnaryOp =
  ((Fix . UnOp Invert <$ char '!' <|> Fix . UnOp Neg <$ char '-')
  <*> (P adUnaryOp <|> P adPostfix))
  <|> P adPostfix
  <?> "Unary"

pPostfix :: ASTDerivs -> Result ASTDerivs AST'
P pPostfix = P adPrimary <|> P adExtern <|> P adFuncDecl <|> P adFuncCall <?> "Postfix"

pFuncDecl :: ASTDerivs -> Result ASTDerivs AST'
P pFuncDecl = do
  string "def" >> spaces
  name <- identifier
  args <- parens $ many (P adIdentifier <* spaces)
  body <- P adExpression
  return $ Fix $ Function name args body

pExtern :: ASTDerivs -> Result ASTDerivs AST'
P pExtern = do
 string "extern" >> spaces
 name <- identifier
 args <- parens $ many (P adIdentifier <* spaces)
 return $ Fix (Extern name args)

pFuncCall :: ASTDerivs -> Result ASTDerivs AST'
P pFuncCall = do
  name <- identifier
  arg <- parens $ optional $ (:) <$> P adExpression <*> many (char ',' *> P adExpression)
  return $ Fix (Call name (fromMaybe [] arg))
  <?> "Call"

pPrimary :: ASTDerivs -> Result ASTDerivs AST'
P pPrimary = P adLiteral <|> P adIdentifier <|> parens (P adBlock) <?> "Primary"

pIdentifier :: ASTDerivs -> Result ASTDerivs AST'
P pIdentifier = Fix . Identifier <$> identifier <?> "Identifier"

{- HLINT ignore "Avoid restricted function" -}
pLiteral :: ASTDerivs -> Result ASTDerivs AST'
P pLiteral = P adDecimal <|> P adNatural <?> "Literal"

pNatural :: ASTDerivs -> Result ASTDerivs AST'
P pNatural = Fix . Literal . read <$> some digit <?> "integer"

pDecimal :: ASTDerivs -> Result ASTDerivs AST'
P pDecimal = Fix . Literal . read <$> do
    n <- many digit
    dt <- char '.'
    d <- some digit
    return (n ++ [dt] ++ d)
  <?> "double"

parens :: Parser ASTDerivs a -> Parser ASTDerivs a
parens p = char '(' *> P adIgnore `around` p <* char ')' <?> "parentheses"

pComment :: ASTDerivs -> Result ASTDerivs String
P pComment = char '#' *> many (noneOf "\n") <?> "comment"
