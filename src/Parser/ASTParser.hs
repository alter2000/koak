module Parser.ASTParser
  where

import Control.Applicative
import Data.Maybe

import Types.Pos
import Types.AST
import Parser.ParserImpl
import Parser.ParseError
import Debug.Trace (trace)

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
  , adFlowCont    :: Result ASTDerivs AST'
  , adAssign      :: Result ASTDerivs AST'
  , adExtern      :: Result ASTDerivs AST'
  , adFuncDecl    :: Result ASTDerivs AST'
  , adDefn        :: Result ASTDerivs AST'
  , adIfExpr      :: Result ASTDerivs AST'
  , adForExpr     :: Result ASTDerivs AST'
  , adWhileExpr   :: Result ASTDerivs AST'
  , adComp        :: Result ASTDerivs AST'
  , adTopLevel    :: Result ASTDerivs [AST']

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
    , adTerm        = pTerm d
    , adFactor      = pFactor d
    , adFlowCont    = pFlowCont d
    , adAssign      = pAssignment d
    , adExtern      = pExtern d
    , adFuncDecl    = pFuncDecl d
    , adDefn        = pDefn d
    , adIfExpr      = pIfExpr d
    , adForExpr     = pForExpr d
    , adWhileExpr   = pWhileExpr d
    , adComp        = pComp d
    , adTopLevel    = pTopLevel d

    , adIgnore = pIgnore d
    -- , adElem   = pElem d
    }

pExpr :: ASTDerivs -> Result ASTDerivs [AST']
P pExpr = P adTopLevel <* eof'

parse :: String -> Either ParseError [AST']
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

pAssignment :: ASTDerivs -> Result ASTDerivs AST'
P pAssignment = do
  str <- identifier
  char '='
  expr <- P adExpression
  return $ mkAssignment str expr

pTopLevel :: ASTDerivs -> Result ASTDerivs [AST']
P pTopLevel = P adDefn `sepBy` (spaces >> char ';' >> spaces)

pDefn :: ASTDerivs -> Result ASTDerivs AST'
P pDefn = P adExtern <|> P adFuncDecl <|> P adExpression

pFlowCont :: ASTDerivs -> Result ASTDerivs AST'
P pFlowCont = P adIfExpr {- <|> P adForExpr -} {- <|> P adWhileExpr <|> mkBlock <$> blockCont
  <?> "Block"
  where
    blockCont :: Parser ASTDerivs [AST']
    blockCont = (:) <$> P adExpression <*> many (char ':' *> P adExpression) -}

pIfExpr :: ASTDerivs -> Result ASTDerivs AST'
P pIfExpr = trace "azef" $ do
  string "if" >> some space
  cond <- P adExpression <* some space
  string "then" >> some space
  thenBody <- P adExpression
  string "else" >> some space
  elseBody <- P adExpression
  return $ mkIfExpr cond thenBody elseBody

pForExpr :: ASTDerivs -> Result ASTDerivs AST'
P pForExpr = mkForExpr
  <$> (string "for" *> some space *> identifier)
  <*> ((spaces *> char '=' *> spaces) *> P adExpression)
  <*> ((string "," *> spaces) *> P adExpression)
  <*> ((string "," *> spaces) *> P adExpression)
  <*> ((some space *> string "in" *> some space) *> P adExpression)
  <?> "FOR"

pWhileExpr :: ASTDerivs -> Result ASTDerivs AST'
P pWhileExpr = do
  string "while" >> some space
  cond <- P adExpression
  string "do" >> some space
  mkWhileExpr cond <$> P adExpression

pExpression :: ASTDerivs -> Result ASTDerivs AST'
P pExpression = {- P adAssign <|> -} do
  first <- P adTerm <* spaces
  foll <- many ((,) <$> (oneOf "+-" <* spaces) <*> (P adTerm <* spaces))
  return (foldl foldFn first foll) <?> "Expression"
    where
      foldFn acc ('-', e) = mkBinOp Minus acc e
      foldFn acc ('+', e) = mkBinOp Plus acc e
      foldFn _ _ = undefined

pTerm :: ASTDerivs -> Result ASTDerivs AST'
P pTerm = do
    first <- P adComp <* spaces
    foll <- many ((,) <$> (oneOf "*/" <* spaces) <*> (P adComp <* spaces))
    return (foldl foldFn first foll) <?> "Term"
        where
            foldFn acc ('/', e) = mkBinOp Divide acc e
            foldFn acc ('*', e) = mkBinOp Times acc e
            foldFn _ _ = undefined

pComp :: ASTDerivs -> Result ASTDerivs AST'
P pComp = do
  a <- P adFactor <* spaces
  op <- (string ">" <|> string "<" <|> string "==" <|> string "!=") <* spaces
  b <- P adFactor <* spaces
  return $ case op of
    "<" -> mkBinOp LessThan a b
    ">" -> mkBinOp MoreThan a b
    "==" -> mkBinOp Equality a b
    "!=" -> mkBinOp Difference a b
    s -> mkBinOp (Err s) a b
  <|> P adFactor
  <?> "Comp"

pFactor :: ASTDerivs -> Result ASTDerivs AST'
P pFactor = P adFlowCont <|> P adUnaryOp <|> parens (P adExpression) <?> "Factor"

pUnaryOp :: ASTDerivs -> Result ASTDerivs AST'
P pUnaryOp =
  ((mkUnOp Invert <$ char '!' <|> mkUnOp Neg <$ char '-')
  <*> (P adUnaryOp <|> P adPostfix))
  <|> P adPostfix
  <?> "Unary"

pPostfix :: ASTDerivs -> Result ASTDerivs AST'
P pPostfix = P adExtern <|> P adFuncDecl <|> P adFuncCall <|> P adPrimary <?> "Postfix"

pExtern :: ASTDerivs -> Result ASTDerivs AST'
P pExtern = do
 string "extern" >> some space
 name <- identifier
 args <- parens (identifier `sepBy` spaces)
 pure $ mkExtern name args

pFuncDecl :: ASTDerivs -> Result ASTDerivs AST'
P pFuncDecl = do
  string "def" >> some space
  name <- identifier
  args <- parens (identifier `sepBy` spaces)
  spaces
  body <- P adExpression
  pure $ mkFunction name args body

pFuncCall :: ASTDerivs -> Result ASTDerivs AST'
P pFuncCall = do
  name <- identifier
  arg <- parens $ optional $ P adExpression `sepBy` (spaces >> char ',' >> spaces)
  return $ mkCall name (fromMaybe [] arg)
  <?> "Call"

pPrimary :: ASTDerivs -> Result ASTDerivs AST'
P pPrimary = P adLiteral <|> P adIdentifier <?> "Primary"

pIdentifier :: ASTDerivs -> Result ASTDerivs AST'
P pIdentifier = mkIdentifier <$> identifier <?> "Identifier"

{- HLINT ignore "Avoid restricted function" -}
pLiteral :: ASTDerivs -> Result ASTDerivs AST'
P pLiteral = P adDecimal <|> P adNatural <?> "Literal"

pNatural :: ASTDerivs -> Result ASTDerivs AST'
P pNatural = mkLiteral . read <$> some digit <?> "integer"

pDecimal :: ASTDerivs -> Result ASTDerivs AST'
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
