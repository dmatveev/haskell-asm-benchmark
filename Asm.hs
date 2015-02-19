{-# LANGUAGE RecursiveDo, CPP #-}
module Main where

import qualified Data.ByteString.Char8 as B

import Control.Monad (liftM, forM_)
import Control.Monad.Trans.State.Strict
import           Data.Sequence (Seq, ViewL(..), (<|), (|>), (><))
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

#ifdef CRI
import Criterion.Main
#endif
import System.IO

-- | Definitions

data Register = R1 | R2 | R3 | R4 | R5 | R6
                deriving (Show)
     
data Operator = ADD | SUB | MUL | DIV
              | LESS | EQUAL | AND | OR | NOT
              | MOV
              | JMT | JMF | JMP
              | PRN | NOP
                deriving (Show)

class OperandClass a where
   toOperand :: a -> Operand

instance OperandClass Register where
   toOperand = R

instance OperandClass Operand where
   toOperand = id

instance OperandClass () where
   toOperand _ = N

data Operand = R !Register | V !Double | I !Int | N
               deriving (Show)

type Instruction = (Operator, Operand, Operand) 

-- | Monad for programming instructions 

type ASM a = State (Seq Instruction) a

compile :: ASM a -> [Instruction]
compile c = toList $ execState c Seq.empty

op :: (OperandClass s, OperandClass d) => Operator -> s -> d -> ASM ()
op cmd src dst = modify $ \s -> s |> (cmd, toOperand src, toOperand dst)

pos :: ASM Int
pos = liftM Seq.length get

nop :: ASM Int
nop = do { p <- pos; op NOP () (); return p}

putOp :: (OperandClass s, OperandClass d) => Int -> Operator -> s -> d -> ASM ()
putOp p cmd src dst = do
    let instr = (cmd, toOperand src, toOperand dst)
    (before,after) <- liftM (Seq.splitAt p) get
    put $ before >< instr <| Seq.drop 1 after

-- | Monad for executing instructions

data Registers = Registers
               { r1 :: {-# UNPACK #-} !Double
               , r2 :: {-# UNPACK #-} !Double
               , r3 :: {-# UNPACK #-} !Double
               , r4 :: {-# UNPACK #-} !Double
               , r5 :: {-# UNPACK #-} !Double
               , r6 :: {-# UNPACK #-} !Double
               } deriving (Show)

initialRs :: Registers
{-# INLINE initialRs #-}
initialRs = Registers
            { r1 = 0
            , r2 = 0
            , r3 = 0
            , r4 = 0
            , r5 = 0
            , r6 = 0
            }

type CPU a = Registers -> IO a

execute :: Registers -> [Instruction] -> IO Registers
execute rs code = exec rs code
  where
   {-# INLINE exec #-}
   exec r []       = return r
   exec r (op:ops) = case op of
      (JMP, I pos, _    ) -> {-# SCC "JMP" #-} exec r $ drop pos code
      (JMF,   reg, I pos) -> {-# SCC "JMF" #-} readVal reg r >>= \v -> exec r $ 
                                                 if toBool v 
                                                 then ops
                                                 else drop pos code
      (JMT,   reg, I pos) -> {-# SCC "JMT" #-} readVal reg r >>= \v -> exec r $
                                                 if toBool v
                                                 then drop pos code
                                                 else ops
      (ins,   src,  dst) -> {-# SCC "OP"  #-} execOP ins src dst r >>= \r' -> exec r' ops


execOP :: Operator -> Operand -> Operand -> CPU Registers
{-# INLINE execOP #-}
execOP ADD   src dst r = {-# SCC "ADD"   #-} arith ADD   src dst r
execOP SUB   src dst r = {-# SCC "SUB"   #-} arith SUB   src dst r
execOP MUL   src dst r = {-# SCC "MUL"   #-} arith MUL   src dst r
execOP DIV   src dst r = {-# SCC "DIV"   #-} arith DIV   src dst r
execOP LESS  src dst r = {-# SCC "LESS"  #-} logic LESS  src dst r
execOP EQUAL src dst r = {-# SCC "EQUAL" #-} logic EQUAL src dst r
execOP AND   src dst r = {-# SCC "AND"   #-} logic AND   src dst r
execOP OR    src dst r = {-# SCC "OR"    #-} logic OR    src dst r
execOP NOT   src dst r = {-# SCC "NOT"   #-} logic NOT   src dst r
execOP MOV   src dst r = {-# SCC "MOV"   #-} readVal src r >>= \v -> putVal r dst $! v
execOP PRN   src _   r = {-# SCC "PRN"   #-} readVal src r >>= \v -> print v >> return r 

arith :: Operator -> Operand -> Operand -> CPU Registers
{-# INLINE arith #-}
arith op src dst r = do
    v1 <- readVal src r
    v2 <- readVal dst r
    case op of
       ADD -> putVal r dst $! v2 + v1
       SUB -> putVal r dst $! v2 - v1
       MUL -> putVal r dst $! v2 * v1
       DIV -> putVal r dst $! v2 / v1

logic :: Operator -> Operand -> Operand -> CPU Registers
{-# INLINE logic #-}
logic op src dst r = do
     v1 <- readVal src r 
     v2 <- readVal dst r
     case op of
        LESS  -> putVal r dst $! fromBool $ v2 <  v1
        EQUAL -> putVal r dst $! fromBool $ v2 == v1
        AND   -> putVal r dst $! fromBool $ toBool v1 && toBool v2
        OR    -> putVal r dst $! fromBool $ toBool v1 && toBool v2
        NOT   -> putVal r dst $! fromBool . not . toBool $ v1

fromBool :: Bool -> Double
{-# INLINE fromBool #-}
fromBool True  = 1
fromBool False = 0

toBool :: Double -> Bool
{-# INLINE toBool #-}
toBool 0 = False
toBool _ = True

readVal :: Operand -> CPU Double
{-# INLINE readVal #-}
readVal (R R1) r = return $! r1 r
readVal (R R2) r = return $! r2 r
readVal (R R3) r = return $! r3 r
readVal (R R4) r = return $! r4 r
readVal (R R5) r = return $! r5 r
readVal (R R6) r = return $! r6 r
readVal (V v)  _ = return $! v

putVal :: Registers -> Operand -> Double -> IO Registers
{-# INLINE putVal #-}
putVal r (R R1) v = return $! r { r1 = v }
putVal r (R R2) v = return $! r { r2 = v }
putVal r (R R3) v = return $! r { r3 = v }
putVal r (R R4) v = return $! r { r4 = v }
putVal r (R R5) v = return $! r { r5 = v }
putVal r (R R6) v = return $! r { r6 = v }


-- | Sample code

-- | Heron's method to calculate square root
-- Inputs:  r1 - value to calculate square root from
--          r2 - number of iterations
-- Outputs: r6 - output value
heron :: ASM ()
heron = mdo op MOV (V 1) R5
            op MOV (V 0) R3
            iterStart <- pos
            op MOV R3 R4
            op EQUAL R2 R4
            op JMT R4 (I loopEnd)
            op MOV R1 R6
            op DIV R5 R6
            op ADD R5 R6
            op MUL (V 0.5) R6
            op MOV R6 R5
            op ADD (V 1) R3
            op JMP (I iterStart) ()
            loopEnd <- pos
            op PRN R6 ()


#ifdef CRI
main :: IO ()
main = defaultMain [
         bench "10000" $ nfIO $ fmap (r6.cpuRegs) $ execute initialRs {r1=10000, r2=20} (compile heron)
       ]
#else
-- | Test
main :: IO ()
main = do
  let h = compile heron
  input <- {-# SCC "READ" #-} liftM (map (read . B.unpack) . B.words) $ B.hGetContents stdin
  forM_ input $ \i -> {-# SCC "ITER" #-} execute (initialRs {r1 = i, r2 = 20 }) h 
#endif
