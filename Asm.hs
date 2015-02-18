{-# LANGUAGE RankNTypes, RecursiveDo, CPP #-}

module Main where

import qualified Data.ByteString.Char8 as B

import Control.Monad (liftM, mapM_, forM_)
import Control.Monad.IO.Class
import qualified Control.Monad.Trans.State.Strict as T
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

type ASM a = T.State (Seq Instruction) a

compile :: ASM a -> [Instruction]
compile c = toList $ T.execState c Seq.empty

op :: (OperandClass s, OperandClass d) => Operator -> s -> d -> ASM ()
op cmd src dst = T.modify $ \s -> s |> (cmd, toOperand src, toOperand dst)

pos :: ASM Int
pos = liftM Seq.length T.get

nop :: ASM Int
nop = do { p <- pos; op NOP () (); return p}

putOp :: (OperandClass s, OperandClass d) => Int -> Operator -> s -> d -> ASM ()
putOp p cmd src dst = do
    let instr = (cmd, toOperand src, toOperand dst)
    (before,after) <- liftM (Seq.splitAt p) T.get
    T.put $ before >< instr <| Seq.drop 1 after

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

-- type CPU a = StateT Registers IO a

newtype CPU a = CPU { runCPU :: forall r. Registers -> (a -> Registers -> IO r) -> IO r }

instance Monad CPU where
   return = retCPU
   (>>=)  = bindCPU

instance MonadIO CPU where
   liftIO = lioCPU

retCPU :: a -> CPU a
{-# INLINE retCPU #-}
retCPU x = CPU $ \s k -> k x s

bindCPU :: CPU a -> (a -> CPU b) -> CPU b
{-# INLINE bindCPU #-}
bindCPU (CPU m) f = CPU $ \s0 k -> m s0 $ \a s1 -> runCPU (f a) s1 k

lioCPU :: IO a -> CPU a
{-# INLINE lioCPU #-}
lioCPU f = CPU $ \s k -> f >>= \x -> k x s

get :: CPU Registers
{-# INLINE get #-}
get = CPU $ \s k -> k s s

gets :: (Registers -> a) -> CPU a
{-# INLINE gets #-}
gets f = get >>= \s -> return $! f s

put :: Registers -> CPU ()
{-# INLINE put #-}
put s = CPU $ \_ k -> k () s

modify :: (Registers -> Registers) -> CPU ()
{-# INLINE modify #-}
modify f = get >>= \s -> let s' = f s in put $! s'

execute ::Registers -> [Instruction] -> IO Registers
execute rs code = runCPU (exec code) rs $ \_ s -> return s
  where
   {-# INLINE exec #-}
   exec []     = return ()
   exec (i:is) = run i >>= exec
       where
         run (JMP, I pos, _    ) = {-# SCC "JMP" #-} return $! drop pos code
         run (JMF,   reg, I pos) = {-# SCC "JMF" #-} readVal reg >>= \v ->
                                                       return $! if toBool v
                                                                 then is
                                                                 else drop pos code
         run (JMT,   reg, I pos) = {-# SCC "JMT" #-} readVal reg >>= \v ->
                                                           return $! if toBool v
                                                                     then drop pos code
                                                                     else is
         run ((ins,   src,   dst)) = {-# SCC "OP"  #-} execOP ins src dst >> return is

execOP :: Operator -> Operand -> Operand -> CPU ()
{-# INLINE execOP #-}
execOP ADD   src dst = {-# SCC "ADD"   #-} arith ADD   src dst
execOP SUB   src dst = {-# SCC "SUB"   #-} arith SUB   src dst
execOP MUL   src dst = {-# SCC "MUL"   #-} arith MUL   src dst
execOP DIV   src dst = {-# SCC "DIV"   #-} arith DIV   src dst
execOP LESS  src dst = {-# SCC "LESS"  #-} logic LESS  src dst
execOP EQUAL src dst = {-# SCC "EQUAL" #-} logic EQUAL src dst
execOP AND   src dst = {-# SCC "AND"   #-} logic AND   src dst
execOP OR    src dst = {-# SCC "OR"    #-} logic OR    src dst
execOP NOT   src dst = {-# SCC "NOT"   #-} logic NOT   src dst
execOP MOV   src dst = {-# SCC "MOV"   #-} readVal src >>= \v -> putVal dst $! v
execOP PRN   src _   = {-# SCC "PRN"   #-} readVal src >>= \v -> liftIO $ print v 

arith :: Operator -> Operand -> Operand -> CPU ()
{-# INLINE arith #-}
arith op src dst = do
    v1 <- readVal src
    v2 <- readVal dst
    case op of
       ADD -> putVal dst $! v2 + v1
       SUB -> putVal dst $! v2 - v1
       MUL -> putVal dst $! v2 * v1
       DIV -> putVal dst $! v2 / v1

logic :: Operator -> Operand -> Operand -> CPU ()
{-# INLINE logic #-}
logic op src dst = do
     v1 <- readVal src
     v2 <- readVal dst
     case op of
        LESS  -> putVal dst $! fromBool $ v2 <  v1
        EQUAL -> putVal dst $! fromBool $ v2 == v1
        AND   -> putVal dst $! fromBool $ toBool v1 && toBool v2
        OR    -> putVal dst $! fromBool $ toBool v1 && toBool v2
        NOT   -> putVal dst $! fromBool . not . toBool $ v1

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
readVal (R R1) = gets r1
readVal (R R2) = gets r2
readVal (R R3) = gets r3
readVal (R R4) = gets r4
readVal (R R5) = gets r5
readVal (R R6) = gets r6
readVal (V v)  = return v

putVal :: Operand -> Double -> CPU ()
{-# INLINE putVal #-}
putVal (R R1) v = modify $ \s -> s { r1 = v }
putVal (R R2) v = modify $ \s -> s { r2 = v }
putVal (R R3) v = modify $ \s -> s { r3 = v }
putVal (R R4) v = modify $ \s -> s { r4 = v }
putVal (R R5) v = modify $ \s -> s { r5 = v }
putVal (R R6) v = modify $ \s -> s { r6 = v }


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
            return ()
            -- op PRN R6 ()


#ifdef CRI
main :: IO ()
main = defaultMain [
         bench "10000" $ nfIO $ fmap r6 $ execute initialRs {r1=10000, r2=20} (compile heron)
       ]
#else
-- | Test
main :: IO ()
main = do
  let h = compile heron
  input <- {-# SCC "READ" #-} liftM (map (read . B.unpack) . B.words) $ B.hGetContents stdin
  forM_ input $ \i -> {-# SCC "ITER" #-} execute (initialRs {r1 = i, r2 = 20 }) h 
#endif
