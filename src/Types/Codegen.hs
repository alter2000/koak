{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types.Codegen
  where

import Data.List
import Data.Function

import LLVM.AST
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C

import Control.Monad.State
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Map.Strict as Map

-- Module level {{{
newtype LLVM a = LLVM { unLLVM :: State AST.Module a }
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM = flip (execState . unLLVM)

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = packShort label }

type Names = Map.Map BSS.ShortByteString Int

uniqueName :: BSS.ShortByteString -> Names -> (BSS.ShortByteString, Names)
uniqueName n ns = maybe (n, Map.insert n 1 ns)
  (\ix -> (n <> packShort (show ix), Map.insert n (succ ix) ns))
  $ Map.lookup n ns

packShort :: [Char] -> BSS.ShortByteString
packShort = BSS.toShort . BS.packChars
-- }}}

double :: Type
double = FloatingPointType DoubleFP

-- Codegen state {{{
data CodegenState = CodegenState
  { currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

type SymbolTable = Map.Map String Operand

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " <> show l

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState
  { currentBlock = Name "entry"
  , blocks = Map.empty
  , symtab = Map.empty
  , blockCount = 1
  , count = 0
  , names = Map.empty
  }

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return $ i + 1

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i } )
  return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm
-- }}}

-- Blocks {{{
data BlockState
  = BlockState {
    idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

entry :: Codegen Name
entry = gets currentBlock

addBlock :: BSS.ShortByteString -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix  <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply }
  pure $ Name qname

setBlock :: Name -> Codegen Name
setBlock bname = modify (\s -> s { currentBlock = bname })
  >> pure bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x  -> return x
    Nothing -> error $ "No such block: " <> show c

local ::  Name -> Operand
local = LocalReference double

global ::  Name -> C.Constant
global = C.GlobalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double
-- }}}
