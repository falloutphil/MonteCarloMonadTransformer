{-# LANGUAGE BangPatterns, TypeSynonymInstances #-}

import Control.Monad.State.Strict
import Debug.Trace

import Data.Word (Word64)
import Data.Bits (shift,xor)

-- Typeclass for RNG Types
class RngClass myType where
  nextRand :: myType [Double] 

type QuasiRandomState = State (Int,[Int])

-- State Monad for QRNGs - stores current iteration and list of
-- bases to compute
instance RngClass QuasiRandomState where
  nextRand = do (n,bases) <- get
                let !nextN = n+1
	        put (nextN,bases)
	        return $ map (reflect (n,1,0)) bases

type ReflectionThreadState = (Int,Double,Double)

reflect :: ReflectionThreadState -> Int -> Double
reflect (k,f,h) base
  | k <= 0 = h
  | otherwise = reflect (newK,newF,newH) base
      where
        newK = k `div` base
        newF = f / fromIntegral base
        newH = h + fromIntegral(k `mod` base) * newF


-- State Monad for PRNG - just to test it

convertToWord64 :: Word64 -> Word64
convertToWord64 = (*2685821657736338717)

convertToDouble :: Word64 -> Double
convertToDouble = (*5.42101086242752217E-20) . fromIntegral . convertToWord64
  
ranq1Increment :: Word64 -> Word64
ranq1Increment =  ( `ranq1XorShift` (-4) ) . ( `ranq1XorShift` 35 ) . ( `ranq1XorShift` (-21) )   

ranq1XorShift :: Word64 -> Int -> Word64
ranq1XorShift v = (xor v) . (shift v)
 
ranq1Init :: Word64 -> Word64
ranq1Init = convertToWord64 . ranq1Increment . ( xor 4101842887655102017 )

type Ranq1RandomState = State Word64

-- This is daft returning a [Double] - just mashing into the Quasi framework for now
instance RngClass Ranq1RandomState where
  nextRand = do state <- get
                let !nextState = ranq1Increment state
                    !andAgain  = ranq1Increment state
                    !r1 = convertToDouble state 
                    !r2 = convertToDouble nextState
                put andAgain
                return $ r2:r1:[]

-- What RNG do you want to use?
type MyRng = QuasiRandomState --Ranq1RandomState
-- So we are defining a state transform which has state of 'maybe double' and an
-- operating function for the inner monad of type QuasiRandomMonad returning a [Double]
-- We then say that it wraps an QuasiRandomMonad (State Monad) - it must of course
-- if we pass it a function that operates on these Monads we must wrap the same
-- type of Monad.  And finally it returns a double
type BoxMullerStateT = StateT (Maybe Double, MyRng [Double])
type BoxMullerQuasiState = BoxMullerStateT MyRng


generateNormal :: BoxMullerQuasiState Double  
--(RngClass myType) => (StateT (Maybe Double, myType [Double]) myType) Double
generateNormal = StateT $ \s -> case s of
				(Just d,qrnFunc) -> return (d,(Nothing,qrnFunc))
				(Nothing,qrnFunc) -> do qrnBaseList <- qrnFunc
					                let (norm1,norm2) = boxMuller (head qrnBaseList) (head $ tail qrnBaseList)
					                return (norm1,(Just norm2,qrnFunc))

boxMuller :: Double -> Double -> (Double,Double)
-- boxMuller rn1 rn2 | trace ( "rn1 " ++ show rn1 ++ " rn2 " ++ show rn2 ) False=undefined 
boxMuller rn1 rn2 = (normal1,normal2)
  where
    r        = sqrt ( (-2)*log rn1)
    twoPiRn2 = 2 * pi * rn2
    normal1  = r * cos ( twoPiRn2 )
    normal2  = r * sin ( twoPiRn2 )

type MonteCarloStateT = StateT Double

mc :: MonteCarloStateT BoxMullerQuasiState ()  
-- (RngClass myType) => MonteCarloStateT (StateT (Maybe Double, myType [Double]) myType) ()
mc = StateT $ \s -> do nextNormal <- generateNormal
                       let stochastic = 0.2*1*nextNormal
                           drift = 0.05 - (0.5*(0.2*0.2))*1
                           !newStockSum = payOff 100 ( 100 * exp ( drift + stochastic ) ) Call + s
                       return ((),newStockSum)

data PutCall = Put
             | Call
               deriving (Eq, Show)

putCallMult :: Num a => PutCall -> a
putCallMult Call = 1
putCallMult Put  = -1

payOff :: Double -> Double -> PutCall -> Double
payOff strike stock putcall | profit > 0 = profit
                            | otherwise = 0
  where
    profit = (putCallMult putcall)*(stock - strike)


iterations = 200000
main :: IO()
-- sumOfPayOffs is a mc monad evaluated with box muller which in turn is evaluated using Halton which
-- is initalised in the outter evalStateT
main = do let sumOfPayOffs = evalState bmState (1,[3,5]) -- (ranq1Init 981110)
                where 
                  mcState = execStateT (do replicateM_ iterations mc) 0
                  bmState = evalStateT mcState (Nothing,nextRand) 
              averagePO = sumOfPayOffs / fromIntegral iterations
              discountPO = averagePO * exp (-0.05)
          print discountPO
 
