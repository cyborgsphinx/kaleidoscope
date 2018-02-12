{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen where

import Data.Monoid ((<>))
import Data.ByteString.Short
import Data.Word
import Data.String
import Data.List
import Data.Function
import qualified Data.Map as Map

import Control.Monad.State
import Control.Applicative

import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

-- setup

double :: AST.Type
--double = FloatingPointAST.Type 64 IEEE
double = AST.FloatingPointType { AST.floatingPointType = AST.DoubleFP }

type SymbolTable = [(ShortByteString, AST.Operand)]

data CodegenState = CodegenState { currentBlock :: AST.Name
                                 , blocks :: Map.Map AST.Name BlockState
                                 , symtab :: SymbolTable
                                 , blockCount :: Int
                                 , count :: Word
                                 , names :: Names
                                 } deriving Show

data BlockState = BlockState { idx :: Int
                             , stack :: [AST.Named AST.Instruction]
                             , term :: Maybe (AST.Named AST.Terminator)
                             } deriving Show

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
    deriving (Functor, Applicative, Monad, MonadState CodegenState)

newtype LLVM a = LLVM (State AST.Module a)
    deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: ShortByteString -> AST.Module
emptyModule label = AST.defaultModule { AST.moduleName = label }

addDefn :: AST.Definition -> LLVM ()
addDefn d = do
    defs <- gets AST.moduleDefinitions
    modify $ \s -> s { AST.moduleDefinitions = defs <> [d] }

define :: AST.Type -> ShortByteString -> [(AST.Type, AST.Name)] -> [BasicBlock] -> LLVM ()
define retty label argtys body = addDefn $ AST.GlobalDefinition $ functionDefaults { name = AST.Name label
                                                                               , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                                                               , returnType = retty
                                                                               , basicBlocks = body
                                                                               }

extern :: AST.Type -> ShortByteString -> [(AST.Type, AST.Name)] -> [BasicBlock] -> LLVM ()
extern retty label argtys body = addDefn $ AST.GlobalDefinition $ functionDefaults { name = AST.Name label
                                                                               , linkage = L.External
                                                                               , parameters = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
                                                                               , returnType = retty
                                                                               , basicBlocks = []
                                                                               }

-- Basic Blocks

entry :: Codegen AST.Name
entry = gets currentBlock

addBlock :: ShortByteString -> Codegen AST.Name
addBlock bname = do
    bls <- gets blocks
    ix  <- gets blockCount
    nms <- gets names
    let new = emptyBlock ix
        (qname, supply) = uniqueName bname nms
    modify $ \s -> s { blocks = Map.insert (AST.Name qname) new bls
                     , blockCount = ix + 1
                     , names = supply
                     }
    return (AST.Name qname)

setBlock :: AST.Name -> Codegen AST.Name
setBlock bname = do
    modify $ \s -> s { currentBlock = bname }
    return bname

getBlock :: Codegen AST.Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
    active <- gets currentBlock
    modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
    c    <- gets currentBlock
    blks <- gets blocks
    case Map.lookup c blks of
      Just x -> return x
      Nothing -> error $ "No such block: " <> show c

-- Codegen stuff that I missed I guess?

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

-- Instructions

fresh :: Codegen Word
fresh = do
    i <- gets count
    modify $ \s -> s { count = i + 1 }
    return $ i + 1

type Names = Map.Map ShortByteString Int

uniqueName :: ShortByteString -> Names -> (ShortByteString, Names)
uniqueName nm ns = case Map.lookup nm ns of
                     Nothing -> (nm, Map.insert nm 1 ns)
                     Just ix -> (nm <> fromString (show ix), Map.insert nm (ix+1) ns)

local :: AST.Name -> AST.Operand
local = AST.LocalReference double

externf :: AST.Name -> AST.Operand
externf = AST.ConstantOperand . C.GlobalReference double

assign :: ShortByteString -> AST.Operand -> Codegen ()
assign var x = do
    lcls <- gets symtab
    modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: ShortByteString -> Codegen AST.Operand
getvar var = do
    syms <- gets symtab
    case lookup var syms of
      Just x  -> return x
      Nothing -> error $ "Local variable not in scope: " <> show var

instr :: AST.Instruction -> Codegen AST.Operand
instr ins = do
    n   <- fresh
    let ref = AST.UnName n
    blk <- current
    let i = stack blk
    modifyBlock (blk { stack = (ref AST.:= ins) : i })
    return $ local ref

terminator :: AST.Named AST.Terminator -> Codegen (AST.Named AST.Terminator)
terminator trm = do
    blk <- current
    modifyBlock (blk { term = Just trm})
    return trm

-- operation functions
fadd :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fadd a b = instr $ AST.FAdd AST.NoFastMathFlags a b []

fsub :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fsub a b = instr $ AST.FSub AST.NoFastMathFlags a b []

fmul :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fmul a b = instr $ AST.FMul AST.NoFastMathFlags a b []

fdiv :: AST.Operand -> AST.Operand -> Codegen AST.Operand
fdiv a b = instr $ AST.FDiv AST.NoFastMathFlags a b []

-- control flow
br :: AST.Name -> Codegen (AST.Named AST.Terminator)
br val = terminator $ AST.Do $ AST.Br val []

cbr :: AST.Operand -> AST.Name -> AST.Name -> Codegen (AST.Named AST.Terminator)
cbr cond tr fl = terminator $ AST.Do $ AST.CondBr cond tr fl []

ret :: AST.Operand -> Codegen (AST.Named AST.Terminator)
ret val = terminator $ AST.Do $ AST.Ret (Just val) []

-- missed these too I guess
toArgs :: [AST.Operand] -> [(AST.Operand,[A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- effect functions
call :: AST.Operand -> [AST.Operand] -> Codegen AST.Operand
call fn args = instr $ AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: AST.Type -> Codegen AST.Operand
alloca ty = instr $ AST.Alloca ty Nothing 0 []

store :: AST.Operand -> AST.Operand -> Codegen AST.Operand
store ptr val = instr $ AST.Store False ptr val Nothing 0 []

load :: AST.Operand -> Codegen AST.Operand
load ptr = instr $ AST.Load False ptr Nothing 0 []

