{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
-- | Everything getting us from the language into an AST
-- and from an AST into a better AST
module Types.AST
  where


import Data.Functor.Classes
import Control.Arrow
import Data.Function
import Data.List as L
import qualified Data.Map.Strict as M

import RecursionSchemes ( Fix(..) )
import Types.Cofree as C
-- import Types.Pos
import Control.Monad.State
import Control.Monad.Reader

type VarName = String
type Interp = StateT Env (ReaderT Env IO)

newtype Env = Env { getEnv :: M.Map VarName AST' }
  deriving (Eq)

instance Semigroup Env where
  -- | 'Data.Map.union' used in reverse, since '(<>)' has right fixity but
  -- 'Data.Map.union' is left-biased. This means that e.g. if one calls
  -- @e1 <> e2@ then all keys found on both @e1@ and @e2@ will be assigned the
  -- values in @e2@.
  -- (<>) = (Env .) . (flip M.union `on` getEnv)
  (<>) = (Env .) . ((<>) `on` getEnv)

instance Monoid Env where mempty = Env mempty

instance Show Env where show = show . M.toList . getEnv


data ASTF rec
  = Literal Double
  | Identifier VarName
  | BinOp BinFunc rec rec
  | UnOp UnFunc rec
  | Block [rec]
  | ForExpr rec rec rec [rec]
  | WhileExpr rec [rec]
  | IfExpr rec [rec] [rec]
  | Call VarName [rec]
  | Assignment VarName rec
  | Function VarName [rec] rec
  -- | Function { fnName :: VarName
  --            , fnArgs :: ![VarName]
  --            , fnBody :: ![rec]
  --            , fnEnv  :: Env }
  | Extern VarName [rec]
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
  deriving (Eq, Ord, Show)

-- Instances {{{

instance Show1 ASTF where
  -- liftShowsPrec showsPrecFunc showListFunc prio item = shows smth
  liftShowsPrec _ _ _  (Literal s) = shows s
  liftShowsPrec _ _ _  (Identifier s) = shows s
  liftShowsPrec spf _ p (BinOp s a b) = spf p a . shows (" " ++ show s ++ " ") . spf p b
  liftShowsPrec spf _ p (Assignment s val) = shows (s ++ " = ") . spf p val
  liftShowsPrec spf _ p (UnOp  s a)   = shows s . shows " " . spf p a
  liftShowsPrec spf slf p (WhileExpr cond block)
    = shows "while " . spf p cond . shows " do " . slf block
  liftShowsPrec spf slf p (ForExpr assign cond inc block)
    = shows "for "
      . spf p assign . shows ", "
      . spf p cond . shows ", "
      . spf p inc
      . shows " do " . slf block
  liftShowsPrec spf slf p (IfExpr cond b1 b2) = shows "if"
    . spf p cond . shows " then " . slf b1 . shows " else " . slf b2
  liftShowsPrec _ slf _ (Call s b) = shows ("Call " <> s <> " with ") . slf b
  liftShowsPrec _ slf _ (Block b) = shows "block " . slf b
  liftShowsPrec spf slf p (Function name args body) = shows ("Function def " ++ name)
    . slf args . shows ": " . spf p body
  liftShowsPrec _ slf _ (Extern name args) = shows ("Extern def " ++ name)
    . slf args

showList' :: ShowS -> (a -> b -> ShowS) -> a -> [b] -> ShowS
showList' end pf p =
  fmap (pf p) >>> L.intersperse (showChar ' ') >>> foldr (.) end

instance Eq1 ASTF where
  liftEq _ (Literal a)  (Literal b) = a == b
  liftEq _ (Identifier a) (Identifier b) = a == b
  liftEq _ _ _ = False
-- }}}

type AST' = Fix ASTF

-- Smart constructors {{{

newtype ASTWithHist =
  ASTWithHist { runASTWithHist :: Cofree ASTF [ASTWithHist] }

-- | Get an AST, handle said AST in such a way that it can see its
--   history, finally return an @IO ()@
-- type AST' = Cofree ASTF Pos

-- node :: f (Cofree f Pos) -> Cofree f Pos
-- node a = nPos C.:< a

-- str :: String -> AST'
-- str = Fix . Str

-- int :: Integer -> AST'
-- int = Fix . Int

-- }}}
