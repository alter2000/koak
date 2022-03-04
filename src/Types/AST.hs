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
  | ForExpr rec [rec]
  | WhileExpr rec [rec]
  | IfExpr rec [rec] [rec]
  | Call VarName [rec]
  -- | Function VarName [rec] rec
  | Function { fnName :: VarName
             , fnArgs :: ![VarName]
             , fnBody :: ![rec]
             , fnEnv  :: Env }
  | Extern VarName [rec]
  deriving (Eq, Show)

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

-- | whole AST definition
-- data ASTF r = Atom !VarName
--             -- | Int  !Integer
--             | Bool !Bool
--             -- | Real !Rational
--             | Str  !String
--             | List ![r]
--             | DottedList ![r] !r
--             -- lambda grabs closure from current env (a la python)
--             | Builtin (Func Interp)
--             | Lambda { fnArgs :: ![VarName]
--                      , fnBody :: !r
--                      , fnEnv  :: Env }
--             deriving (Show, Eq)

newtype Func m = Func { getFn :: [AST'] -> m AST' }

instance Show (Func m) where show _ = "#<func>"
-- | since 'Func' is only used by primitives and 'M.lookup' is trusted
instance   Eq (Func m) where _ == _ = True

-- Instances {{{
instance Functor ASTF where
  -- fmap _  (Str a) = Str  a
  -- fmap _  (Int a) = Int a
  -- fmap _ (Real a) = Real a

instance Foldable ASTF where
  -- foldMap _  (Str _) = mempty
  -- foldMap _  (Int _) = mempty
  -- foldMap _ (Real _) = mempty

instance Traversable ASTF where
  -- traverse _  (Str a) = pure $ Str a
  -- traverse _  (Int a) = pure $ Int a
  -- traverse _ (Real a) = pure $ Real a

-- instance Show1 ASTF where
--   liftShowsPrec _ _ _  (Literal s) = shows s
--   liftShowsPrec _ _ _  (Identifier s) = shows s
--   liftShowsPrec arg1 arg2 arg3  (BinOp s a b)
--     = shows s
--     . (liftShowsPrec arg1 arg2 arg3 a)
--     . (liftShowsPrec arg1 arg2 arg3 b)
--   liftShowsPrec arg1 arg2 arg3  (UnOp s a)
--     = shows s
--     . (liftShowsPrec arg1 arg2 arg3 a)
--   liftShowsPrec arg1 arg2 arg3  (WhileExpr cond block)
--     = (liftShowsPrec arg1 arg2 arg3 cond)
--     . (foldl ((.) . liftShowsPrec arg1 arg2 arg3) block)
--   liftShowsPrec arg1 arg2 arg3  (IfExpr cond block1 block2)
--     = (liftShowsPrec arg1 arg2 arg3 cond)
--     . (foldl ((.) . liftShowsPrec arg1 arg2 arg3) block1)
--     . (foldl ((.) . liftShowsPrec arg1 arg2 arg3) block2)
--   liftShowsPrec arg1 arg2 arg3  (Call s block)
--     = shows s
--     . (foldl ((.) . liftShowsPrec arg1 arg2 arg3) block)
--   liftShowsPrec _ _ _ _ = shows "Function def or Extern"


instance Show1 ASTF where
  liftShowsPrec _ _ _  (Literal s) = shows s
  liftShowsPrec _ _ _  (Identifier s) = shows s
  liftShowsPrec arg1 arg2 arg3  (BinOp s a b)
    = shows s
  liftShowsPrec arg1 arg2 arg3  (UnOp s a)
    = shows s
  liftShowsPrec arg1 arg2 arg3  (WhileExpr cond block)
    = shows "WhileExpr"
  liftShowsPrec arg1 arg2 arg3  (IfExpr cond block1 block2)
    = shows "IfExpr"
  liftShowsPrec arg1 arg2 arg3  (Call s block)
    = shows (s ++ " Call")
  liftShowsPrec _ _ _ _ = shows "Function def or Extern"


showList' :: ShowS -> (a -> b -> ShowS) -> a -> [b] -> ShowS
showList' end pf p =
  fmap (pf p) >>> L.intersperse (showChar ' ') >>> foldr (.) end

instance Eq1 ASTF where
  -- liftEq _  (Int a)  (Int b) = a == b
  -- liftEq _  (Str a)  (Str b) = a == b
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
