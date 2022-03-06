{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
-- | Everything getting us from the language into an AST
-- and from an AST into a better AST
module Types.AST
  where


import qualified Data.Map.Strict as M

import Control.Monad.State
import Control.Monad.Reader
import LLVM.AST (mkName, Name)

type VarName = Name
type Interp = StateT Env (ReaderT Env IO)

newtype Env = Env { getEnv :: M.Map VarName AST }
  deriving (Eq, Semigroup, Monoid)

instance Show Env where show = show . M.toList . getEnv


data AST
  = Literal Double
  | Identifier VarName
  | BinOp VarName AST AST
  | UnOp VarName AST
  | ForExpr VarName AST AST AST AST
  | WhileExpr AST AST
  | IfExpr AST AST AST
  | Call VarName [AST]
  | Let VarName AST AST
  deriving (Eq, Show, Ord
  )

{-  | Function VarName [VarName] rec
  | Extern VarName [VarName] -}

data UnFunc = Neg | Invert
  deriving (Eq, Ord, Show)

data BinFunc
  = Plus
  | Minus
  | Times
  | Divide
  | LessThan
  | MoreThan
  | Equality
  | Difference
  | Assignment
  deriving (Eq, Ord, Show)

data Defn
  = Function VarName [VarName] AST
  | Extern VarName [VarName]
  | BinaryDef VarName [VarName] AST
  | UnaryDef VarName VarName AST
  deriving (Eq, Ord, Show)

data Phrase
  = DefnPhrase Defn
  | ExprPhrase AST
  deriving (Eq, Ord, Show)

-- type AST' = Fix AST

-- Smart constructors {{{

mkLiteral :: Double -> AST
mkLiteral = Literal

mkIdentifier :: String -> AST
mkIdentifier = Identifier . mkName

mkBinOp :: BinFunc -> AST -> AST -> AST
mkBinOp Plus       a b = BinOp (mkName "+")  a b
mkBinOp Minus      a b = BinOp (mkName "-")  a b
mkBinOp Times      a b = BinOp (mkName "*")  a b
mkBinOp Divide     a b = BinOp (mkName "/")  a b
mkBinOp LessThan   a b = BinOp (mkName "<")  a b
mkBinOp MoreThan   a b = BinOp (mkName ">")  a b
mkBinOp Equality   a b = BinOp (mkName "==") a b
mkBinOp Difference a b = BinOp (mkName "!=") a b
mkBinOp Assignment a b = BinOp (mkName "=")  a b

mkUnOp :: UnFunc -> AST -> AST
mkUnOp Neg    a = UnOp (mkName "-") a
mkUnOp Invert a = UnOp (mkName "!") a

mkForExpr :: String -> AST -> AST -> AST -> AST -> AST
mkForExpr = ForExpr . mkName

mkWhileExpr :: AST -> AST -> AST
mkWhileExpr = WhileExpr

mkIfExpr :: AST -> AST -> AST -> AST
mkIfExpr = IfExpr

mkCall :: String -> [AST] -> AST
mkCall = Call . mkName

mkLet :: String -> AST -> AST -> AST
mkLet = Let . mkName

mkFunction :: String -> [String] -> AST -> Defn
mkFunction fname args = Function (mkName fname) (mkName <$> args)

mkExtern :: String -> [String] -> Defn
mkExtern ename args = Extern (mkName ename) (mkName <$> args)

-- }}}
