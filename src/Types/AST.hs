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

newtype Env = Env { getEnv :: M.Map VarName ASTF }
  deriving (Eq, Semigroup, Monoid)

instance Show Env where show = show . M.toList . getEnv


data ASTF
  = Literal Double
  | Identifier VarName
  | BinOp BinFunc ASTF ASTF
  | UnOp UnFunc ASTF
  | ForExpr VarName ASTF ASTF ASTF ASTF
  | WhileExpr ASTF ASTF
  | IfExpr ASTF ASTF ASTF
  | Call VarName [ASTF]
  | Let VarName ASTF ASTF
  deriving (Eq, Show, Ord
          --  , Functor, Foldable, Traversable
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
  = Function VarName [VarName] ASTF
  | Extern VarName [VarName]
  | BinaryDef VarName [VarName] ASTF
  | UnaryDef VarName VarName ASTF
  deriving (Eq, Ord, Show)

data Phrase
  = DefnPhrase Defn
  | ExprPhrase ASTF
  deriving (Eq, Ord, Show)

-- Instances {{{

-- instance Show1 ASTF where
--   -- liftShowsPrec showsPrecFunc showListFunc prio item = shows smth
--   liftShowsPrec _ _ _  (Literal s) = shows s
--   liftShowsPrec _ _ _  (Identifier s) = shows s
--   liftShowsPrec spf _ p (BinOp s a b) =
--     spf p a . shows (" " ++ show s ++ " ") . spf p b
--   liftShowsPrec spf _ p (Let s val expr)
--     = shows (s ++ " = ") . spf p val . shows " in " . spf p expr
--   liftShowsPrec spf _ p (UnOp  s a)   = shows s . shows " " . spf p a
--   liftShowsPrec spf _ p (WhileExpr cond expr)
--     = shows "while " . spf p cond
--     . shows " do " . spf p expr
--   liftShowsPrec spf _ p (ForExpr varName assign cond inc expr)
--     = shows ("for " ++ varName ++ " = ")
--     . spf p assign . shows ", "
--     . spf p cond . shows ", "
--     . spf p inc
--     . shows " do " . spf p expr
--   liftShowsPrec spf _ p (IfExpr cond b1 b2) = shows "if"
--     . spf p cond . shows " then " . spf p b1 . shows " else " . spf p b2
--   liftShowsPrec _ slf _ (Call s b) = shows ("Call " <> s <> " with ") . slf b
--   liftShowsPrec spf _ p (Function name args body) =
--     shows ("Fn def " ++ name) . showList args . shows ": " . spf p body
--   liftShowsPrec _ _ _ (Extern name args) =
--     shows ("Extern def " ++ name) . showList args

showList' :: ShowS -> (a -> b -> ShowS) -> a -> [b] -> ShowS
showList' end pf p =
  fmap (pf p) >>> L.intersperse (showChar ' ') >>> foldr (.) end

-- instance Eq1 ASTF where
--   liftEq _    (Literal a)    (Literal b) = a == b
--   liftEq _ (Identifier a) (Identifier b) = a == b
--   liftEq f (BinOp f1 a1 b1) (BinOp f2 a2 b2) = f1 == f2 && f a1 a2 && f b1 b2
--   liftEq f (UnOp f1 a1) (UnOp f2 a2) = f1 == f2 && f a1 a2
--   liftEq f (ForExpr v1 a1 e1 i1 b1) (ForExpr v2 a2 e2 i2 b2) = v1 == v2 && f a1 a2 && f e1 e2 && f i1 i2 && f b1 b2
--   liftEq f (WhileExpr c1 e1) (WhileExpr c2 e2) = f c1 c2 && f e1 e2
--   liftEq f (IfExpr c1 t1 e1) (IfExpr c2 t2 e2) = f c1 c2 && f t1 t2 && f e1 e2
--   liftEq f (Call n1 a1) (Call n2 a2) = n1 == n2 && liftEq f a1 a2
--   liftEq f (Let n1 v1 e1) (Let n2 v2 e2) = n1 == n2 && f v1 v2 && f e1 e2
--   liftEq _ (Extern a argsA) (Extern b argsB) = a == b && argsA == argsB
--   liftEq f (Function s1 a1 b1) (Function s2 a2 b2) = s1 == s2 && a1 == a2 && f b1 b2
--   liftEq _ _ _ = False
-- }}}

-- type AST' = Fix ASTF

-- Smart constructors {{{

mkLiteral :: Double -> ASTF
mkLiteral = Literal

mkIdentifier :: VarName -> ASTF
mkIdentifier = Identifier

mkBinOp :: BinFunc -> ASTF -> ASTF -> ASTF
mkBinOp = BinOp

mkUnOp :: UnFunc -> ASTF -> ASTF
mkUnOp = UnOp

mkForExpr :: VarName -> ASTF -> ASTF -> ASTF -> ASTF -> ASTF
mkForExpr = ForExpr

mkWhileExpr :: ASTF -> ASTF -> ASTF
mkWhileExpr = WhileExpr

mkIfExpr :: ASTF -> ASTF -> ASTF -> ASTF
mkIfExpr = IfExpr

mkCall :: VarName -> [ASTF] -> ASTF
mkCall = Call

mkLet :: VarName -> ASTF -> ASTF -> ASTF
mkLet = Let

mkFunction :: VarName -> [VarName] -> ASTF -> Defn
mkFunction = Function

mkExtern :: VarName -> [VarName] -> Defn
mkExtern = Extern

-- }}}
