{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

--import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

-- setup

double :: Type
double = FloatingPointType 64 IEEE

type SymbolTable = [(String, Operand)]

data CodegenState = CodegenState { currentBlock :: Name
                                 , bocks :: Map.Map Name BlockState
                                 , symtab :: SymbolTable
                                 , blockCount :: Int
                                 , count :: Word
                                 , names :: Names
                                 } deriving Show

data BlockState = BlockState { idx :: Int
                             , stack :: [Named Instruction]
                             , term :: Maybe (Named Terminator)
                             } deriving Show

newtype Codegen a = Codegen { runCodegen :: State Codegen a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
    defs <- gets moduleDefinitions
    modify $ \s -> s { moduleDefinitions = defs ++ [d] }

define :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $ GlobalDefinition $ functionDefaults { name = Name label
                                                                               , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                                                               , returnType = retty
                                                                               , basicBlocks = body
                                                                               }

extern :: Type -> String -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
extern retty label argtys body = addDefn $ GlobalDefinition $ functionDefaults { name = Name label
                                                                               , linkage = L.External
                                                                               , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                                                               , returnType = retty
                                                                               , basicBlocks = []
                                                                               }

-- Basic Blocks

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
    bls <- gets blocks
    ix  <- gets blockCount
    nms <- gets names
    let name = emptyBlock ix
        (qname, supply) = uniqueName bname nms
    modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (Name qname)

setBlock :: Codegen Name
setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { block = Map.insert active new (block s) }

current :: Codegen BlockState
current = do
    c    <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
      Just x -> return x
      Nothing -> error $ "No such block: " ++ show c

-- Instructions

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = i + 1 }
    return $ i + 1

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns = case Map.lookup nm ns of
                     Nothing -> (nm, Map.insert nm 1 ns)
                     Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: String -> Operand -> Codegen ()
assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: String -> Codegen Operand
getvar var = do
    syms <- gets symtab
    case lookup var syms of
      Just x  -> return x
      Nothing -> error $ "Local variable not in scope: " ++ show var

instr :: Instruction -> Codegen Operand
instr ins = do
    n   <- fresh
    let ref = UnName n
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (ref := ins) : i })
    return $ local ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm})
    return trm

-- operation functions
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr $ FDiv NoFastMathFlags a b []

-- control flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

-- effect functions
call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca ty = instr $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []

