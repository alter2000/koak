{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
-- | Everything getting us from the language into an AST
-- and from an AST into a better AST
module Types.AST
  where


import Data.Functor.Classes
import Control.Arrow
import Data.List as L
import qualified Data.Map.Strict as M

import RecursionSchemes ( Fix(..) )
import Control.Monad.State
import Control.Monad.Reader
-- import Types.Cofree as C
-- import Types.Pos

type VarName = String
type Interp = StateT Env (ReaderT Env IO)

newtype Env = Env { getEnv :: M.Map VarName AST' }
  deriving (Eq, Semigroup, Monoid)

instance Show Env where show = show . M.toList . getEnv


data ASTF rec
  = Literal Double
  | Identifier VarName
  | BinOp BinFunc rec rec
  | UnOp UnFunc rec
  | Block [rec]
  | ForExpr VarName rec rec rec rec
  | WhileExpr rec rec
  | IfExpr rec rec rec
  | Call VarName [rec]
  | Assignment VarName rec
  | Function VarName [VarName] rec
  | Extern VarName [VarName]
  deriving (Eq, Show
           , Functor, Foldable, Traversable
  )

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
  | Err String
  deriving (Eq, Ord, Show)

-- Instances {{{

instance Show1 ASTF where
  -- liftShowsPrec showsPrecFunc showListFunc prio item = shows smth
  liftShowsPrec _ _ _  (Literal s) = shows s
  liftShowsPrec _ _ _  (Identifier s) = shows s
  liftShowsPrec spf _ p (BinOp s a b) =
    spf p a . shows (" " ++ show s ++ " ") . spf p b
  liftShowsPrec spf _ p (Assignment s val) = shows (s ++ " = ") . spf p val
  liftShowsPrec spf _ p (UnOp  s a)   = shows s . shows " " . spf p a
  liftShowsPrec spf _ p (WhileExpr cond expr)
    = shows "while " . spf p cond
    . shows " do " . spf p expr
  liftShowsPrec spf _ p (ForExpr varName assign cond inc expr)
    = shows ("for " ++ varName ++ " = ")
    . spf p assign . shows ", "
    . spf p cond . shows ", "
    . spf p inc
    . shows " do " . spf p expr
  liftShowsPrec spf _ p (IfExpr cond b1 b2) = shows "if"
    . spf p cond . shows " then " . spf p b1 . shows " else " . spf p b2
  liftShowsPrec _ slf _ (Call s b) = shows ("Call " <> s <> " with ") . slf b
  liftShowsPrec _ slf _ (Block b) = shows "block " . slf b
  liftShowsPrec spf _ p (Function name args body) =
    shows ("Fn def " ++ name) . showList args . shows ": " . spf p body
  liftShowsPrec _ _ _ (Extern name args) =
    shows ("Extern def " ++ name) . showList args

showList' :: ShowS -> (a -> b -> ShowS) -> a -> [b] -> ShowS
showList' end pf p =
  fmap (pf p) >>> L.intersperse (showChar ' ') >>> foldr (.) end

instance Eq1 ASTF where
  liftEq _    (Literal a)    (Literal b) = a == b
  liftEq _ (Identifier a) (Identifier b) = a == b
  liftEq f (BinOp f1 a1 b1) (BinOp f2 a2 b2) = f1 == f2 && f a1 a2 && f b1 b2
  liftEq f (UnOp f1 a1) (UnOp f2 a2) = f1 == f2 && f a1 a2
  liftEq f (Block a) (Block b) = liftEq f a b
  liftEq f (ForExpr v1 a1 e1 i1 b1) (ForExpr v2 a2 e2 i2 b2) = v1 == v2 && f a1 a2 && f e1 e2 && f i1 i2 && f b1 b2
  liftEq f (WhileExpr c1 e1) (WhileExpr c2 e2) = f c1 c2 && f e1 e2
  liftEq f (IfExpr c1 t1 e1) (IfExpr c2 t2 e2) = f c1 c2 && f t1 t2 && f e1 e2
  liftEq f (Call n1 a1) (Call n2 a2) = n1 == n2 && liftEq f a1 a2
  liftEq f (Assignment n1 v1) (Assignment n2 v2) = n1 == n2 && f v1 v2
  liftEq _ (Extern a argsA) (Extern b argsB) = a == b && argsA == argsB
  liftEq f (Function s1 a1 b1) (Function s2 a2 b2) = s1 == s2 && a1 == a2 && f b1 b2
  liftEq _ _ _ = False
-- }}}

type AST' = Fix ASTF

-- Smart constructors {{{

mkLiteral :: Double -> AST'
mkLiteral = Fix . Literal

mkIdentifier :: VarName -> AST'
mkIdentifier = Fix . Identifier

mkBinOp :: BinFunc -> AST' -> AST' -> AST'
mkBinOp = ((Fix .) .) . BinOp

mkUnOp :: UnFunc -> AST' -> AST'
mkUnOp = (Fix .) . UnOp

mkBlock :: [AST'] -> AST'
mkBlock = Fix . Block

mkForExpr :: VarName -> AST' -> AST' -> AST' -> AST' -> AST'
mkForExpr = ((((Fix .) .) .) .) . ForExpr

mkWhileExpr :: AST' -> AST' -> AST'
mkWhileExpr = (Fix .) . WhileExpr

mkIfExpr :: AST' -> AST' -> AST' -> AST'
mkIfExpr = ((Fix .) .) . IfExpr

mkCall :: VarName -> [AST'] -> AST'
mkCall = (Fix .) . Call

mkAssignment :: VarName -> AST' -> AST'
mkAssignment = (Fix .) . Assignment

mkFunction :: VarName -> [VarName] -> AST' -> AST'
mkFunction = ((Fix .) .) . Function

mkExtern :: VarName -> [VarName] -> AST'
mkExtern = (Fix .) . Extern

-- }}}
