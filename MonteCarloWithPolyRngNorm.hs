{-# LANGUAGE BangPatterns #-}

import Control.Monad.State.Strict
import Data.Word (Word64)
import Data.Bits (shift,xor)
-- Unboxing the arrays doesn't really buy us much
-- but I consider it more correct for this 'Array' will probably
-- introduce one level of indirection (pointer to array value),
-- as all values are going to be used, boxing is pointless.
import Data.Array.Unboxed

import Debug.Trace

-- Trace internal varaibles just like function
-- parameters.  May maintain your sanity.
tracePassThrough :: Show a => a -> String -> a
--tracePassThrough value string
-- | trace ( "Debug: " ++ string ++ " " ++ show value ) False = undefined
tracePassThrough value string = value


-- Classtype insists we produce a functional
-- that returns a State monad which returns
-- a list of doubles (n-dimensional random vector)
class RngClass a where
  rngStateFn :: Int -> State a [Double]
  
-- Halton Implementation

type ReflectionState = (Int,Double,Double)

reflect :: ReflectionState -> Int -> Double
reflect (k,f,h) base
   | k <= 0 = h
   | otherwise = reflect (newK,newF,newH) base
       where
         newK = k `div` base
         newF = f / fromIntegral base
         newH = h + fromIntegral(k `mod` base) * newF


-- This method is very simple and OK for n<1000.  
-- If you're using Halton >12D, you're not too smart, so 1000 is more than fine!
primes :: [Int]
primes = sieve [2..]
            where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]


-- The underlying state of a halton sequence
-- is just an int, but if we want to have
-- many instances of RngClass each with
-- an underlying state of Int, then
-- we must differentiate.  We need a
-- a *new* type exclusive of, but synonmous
-- with an int.  We reduce this to an Int
-- once we've drilled through the polymorphism.
-- deriving(Show) needed to use with trace function.
newtype Halton = Halton [Double]
  deriving(Show)

-- Wrapped in RngType so 'Halton' state
-- can be thread through the getResultFn
haltonInit :: Int -> Int -> RngType
haltonInit initialState totalDims = 
   -- Infinite list of primes cycled according
   -- to our dimensionality is zipped with
   -- the corresponding simulation number.
   -- Unlike PRNG we actually define the entire
   -- series before we even start simulating,
   -- such is the joy of Haskell.
   -- Our state monad just takes the next item
   -- in the list we generate and returns the rest.
   -- Can't decide if this is very clever or
   -- somewhat obfuscated :-)
   let primeList  = cycle $ take totalDims primes
       simNumList = [ tsSeeds | seeds <- [initialState..], tsSeeds <- replicate totalDims seeds ]
      in RngTypeHalton $ Halton [ reflect (sim,1,0) prime | (sim,prime) <- zip simNumList primeList ]

-- The complex function above makes this a doddle.
-- Note we NEVER force evaluation of the infinite list!
instance RngClass Halton where
  rngStateFn dims =
     State $ \(Halton s) -> let (!returned,rest) = splitAt dims s in (returned,Halton rest)



                     
-- Ranq1 Implementation

newtype Ranq1 = Ranq1 Word64
   deriving(Show)

convertToWord64 :: Word64 -> Word64
convertToWord64 = (*2685821657736338717)

convertToDouble :: Word64 -> Double
convertToDouble = (*5.42101086242752217E-20) . 
                  fromIntegral               . 
                  convertToWord64
  
ranq1Increment :: Word64 -> Word64
ranq1Increment =  ( `ranq1XorShift` (-4) ) . 
                  ( `ranq1XorShift` 35 )   . 
                  ( `ranq1XorShift` (-21) )   

ranq1XorShift :: Word64 -> Int -> Word64
ranq1XorShift v = (xor v) . (shift v)
 
ranq1Init :: Word64 -> RngType
ranq1Init = RngTypeRanq1 . Ranq1 . convertToWord64 . 
                                   ranq1Increment    . 
                                   ( xor 4101842887655102017 ) 


-- Reversing isn't ideal - but it is better than appending to the end each time.
-- The list is only rebuilt once with a reverse and will be O(Dimensions).
-- Not expecting a huge amount of dimensions, so this will suffice for now.
-- http://www.haskell.org/pipermail/beginners/2009-February/000882.html
multiRanq1 :: ([Double],Word64) -> Int -> ([Double], Word64)
multiRanq1 (vector,state) dims 
   | dims <= 0 = (reverse vector,state)
   | otherwise = multiRanq1 (newVector,newState) (dims-1)
                    where
                       newVector = convertToDouble state : vector
                       newState  = ranq1Increment state 
                                               
instance RngClass Ranq1 where
   rngStateFn dims = State $ \(Ranq1 s) -> let (vector,!state) = multiRanq1 ([],s) dims
                                              in (vector, Ranq1 state)


-- In somewhat of a paradox we need
-- to unite our purposefully different state types
-- so that they can be passed around under a single
-- guise.  We use 'data', because 'newtype' must
-- have exactly one constructor.
data RngType = RngTypeHalton Halton | 
               RngTypeRanq1  Ranq1

-- Create a StateType containing the underlying
-- inital state of our RNG depedning on user input.
-- We have to know our Halton dimensionality at initialization
-- because our state is effectively 2D (Base,Sim)
rngChooser :: String -> Int -> RngType
rngChooser rngStr totalTsDimensions
   -- Discard first 20 Haltons
   | rngStr == "Halton" = haltonInit 20 totalTsDimensions
   | rngStr == "Ranq1"  = ranq1Init 1
   | otherwise          = haltonInit 20 totalTsDimensions



-- Now for Normals

newtype BoxMuller = BoxMuller (Maybe Double)
   deriving (Show)

newtype Acklam    = Acklam ()
   deriving (Show)

data NormalType = StateBoxMuller BoxMuller |
                  StateAcklam    Acklam

class NormalClass a where
  nextNormal :: RngClass b => StateT a (State b) Double

-- Box Muller

boxMuller :: Double -> Double -> (Double,Double)
boxMuller rn1 rn2 = (normal1,normal2)
  where
    r        = sqrt ( (-2)*log rn1)
    twoPiRn2 = 2 * pi * rn2
    normal1  = r * cos ( twoPiRn2 )
    normal2  = r * sin ( twoPiRn2 )
 
instance NormalClass BoxMuller where
   nextNormal = 
      StateT $ \(BoxMuller s) -> case s of
                                    Just d  -> return (d,BoxMuller Nothing)
	                            Nothing -> do rn1:rn2:rns <- rngStateFn 2
	                                          let (norm1,norm2) = boxMuller rn1 rn2
   				                  return (norm1,BoxMuller (Just norm2))


-- Peter Acklam's method

a, b, c, d:: UArray Int Double

a = listArray (1, 6) [-3.969683028665376e+01, 2.209460984245205e+02, 
                     -2.759285104469687e+02, 1.383577518672690e+02, 
                     -3.066479806614716e+01, 2.506628277459239e+00]

b = listArray (1, 5) [-5.447609879822406e+01, 1.615858368580409e+02, 
                     -1.556989798598866e+02, 6.680131188771972e+01, 
                     -1.328068155288572e+01]

c = listArray (1, 6) [-7.784894002430293e-03, -3.223964580411365e-01, 
                     -2.400758277161838e+00, -2.549732539343734e+00, 
                     4.374664141464968e+00,  2.938163982698783e+00]
  
d = listArray (1, 4) [7.784695709041462e-03,  3.224671290700398e-01, 
                     2.445134137142996e+00,  3.754408661907416e+00]


invnorm :: Double -> Double
invnorm p 
   | p < 0.02425 = 
      let q = sqrt ( -2*log(p) ) 
         in (((((c!1*q+c!2)*q+c!3)*q+c!4)*q+c!5)*q+c!6) / 
            ((((d!1*q+d!2)*q+d!3)*q+d!4)*q+1)
                          
                            
   | p > (1-0.02425) = 
      let q = sqrt ( -2*log(1-p) ) 
         in -(((((c!1*q+c!2)*q+c!3)*q+c!4)*q+c!5)*q+c!6) / 
            ((((d!1*q+d!2)*q+d!3)*q+d!4)*q+1)
                              

   | otherwise = 
      let q = p-0.5
          r = q*q
         in (((((a!1*r+a!2)*r+a!3)*r+a!4)*r+a!5)*r+a!6)*q / 
            (((((b!1*r+b!2)*r+b!3)*r+b!4)*r+b!5)*r+1) 

                            
-- Acklam's method doesn't require any 
-- state so we are effectively creating
-- a stateless state monad transformer!
-- Compiler should (hopefully) recognise this!
instance NormalClass Acklam where
   nextNormal = StateT $ \_ -> do rn:rns <- rngStateFn 1
                                  return ( invnorm rn, Acklam () )
 
   

normalChooser :: String -> NormalType
normalChooser normStr 
   | normStr == "Box Muller" = StateBoxMuller (BoxMuller Nothing)
   | normStr == "Acklam"     = StateAcklam (Acklam () )
   | otherwise               = StateBoxMuller (BoxMuller Nothing)



-- Monte Carlo


data MonteCarloUserData = MonteCarloUserData 
   { strike       :: Double,
     underlying   :: Double,
     putCall      :: PutCall,
     volatility   :: Double,
     expiry       :: Double,
     interestRate :: Double,
     timeSteps    :: Int,
     evolveFn     :: MonteCarloUserData -> (Double -> Double -> Double) }

data PutCall = Put | Call
               deriving (Read)

putCallMult :: Num a => PutCall -> a
putCallMult Call = 1
putCallMult Put  = -1

payOff :: Double -> Double -> PutCall -> Double
payOff strike stock putcall =
   max 0 $ (putCallMult putcall)*(stock - strike) 



mc :: NormalClass a => RngClass b => 
     (Double->Double->Double) -> StateT Double (StateT a (State b)) ()
mc evolver = StateT $ \s -> do norm <- nextNormal 
                               return ((), evolver s norm)
                               
-- Need a typeclass for these! **********************************
mcPd :: NormalClass a => RngClass b => 
       (Double->Double->Double->(Double,Double)) -> StateT (Double,Double) (StateT a (State b)) ()
mcPd evolver = StateT $ \(currentV,maxV) -> do norm <- nextNormal
                                               return ((), evolver currentV norm maxV)
                               
evolveClosedForm :: MonteCarloUserData -> (Double -> Double -> Double)
--evolveClosedForm userData currentValue normal
--   | trace ( "   currentValue " ++ show currentValue ++ " normal " ++ show normal ) False=undefined 
evolveClosedForm userData currentValue normal = 
   let vol        = volatility userData
       delta_t    = expiry userData / fromIntegral (timeSteps userData)
       stochastic = vol * normal * sqrt delta_t
       drift      = ( (interestRate userData) - (0.5*vol*vol) )*delta_t
      in currentValue * exp ( drift + stochastic )

evolveStandard :: MonteCarloUserData -> (Double -> Double -> Double)
--evolveStandard userData currentValue normal
--   | trace ( "   currentValue " ++ show currentValue ++ " normal " ++ show normal ) False=undefined 
evolveStandard userData currentValue normal = 
   let vol        = volatility userData
       delta_t    = expiry userData / fromIntegral (timeSteps userData)
       stochastic = vol * normal * sqrt delta_t
       drift      = (interestRate userData) * delta_t
      in currentValue * ( 1 + drift + stochastic)
      
--  Must also handle the extra parameter here currentMaxValue ************************
evolveLookback :: MonteCarloUserData -> (Double -> Double -> Double -> (Double,Double))
evolveLookback userData currentValue normal currentMaxValue =
   let newValue = evolveStandard userData currentValue normal
      in (  max newValue currentMaxValue, newValue )


-- Here's the polymorphism!  Evaluate a monad stack
-- where the inner monad is of RngClass and outer is of NormalClass
singleResult :: RngClass a => NormalClass b => -- Show a =>
                a -> b -> MonteCarloUserData -> ( Double, (a,b) )
--singleResult initRngState initNormState userData 
--   | trace ( "   initRngState " ++ show initRngState ) False=undefined 
singleResult initRngState initNormState userData = 
   let (((mc_a,mc_s), norm_s), rng_s) = runState rngMonad initRngState
         where rngMonad     = runStateT normMonadT initNormState
               evolve       = evolveFn userData $ userData
               mcCombinator = do replicateM_ (timeSteps userData) (mc evolve)
               normMonadT   = runStateT mcCombinator (underlying userData)
      in (mc_s, (rng_s,norm_s))
                                                     

simResult :: RngClass a => NormalClass b => Show a => Show b =>
             Int -> Double -> a -> b -> MonteCarloUserData -> Double
--simResult numOfSims runTotal initRng initNorm userData  
--   | trace ( "numOfSims " ++ show numOfSims ++ "   initRng " ++ show initRng ) False=undefined 
simResult numOfSims runTotal initRng initNorm userData 
   | numOfSims <= 1 = runTotal
   | otherwise = let (!value,(!rngS,!normS)) = singleResult initRng initNorm userData
                     !newNumOfSims           = numOfSims - 1
                     profit                  = tracePassThrough (payOff (strike userData) value (putCall userData)) "Pay off"
                     !newRunTotal            = runTotal + profit
                    in simResult newNumOfSims newRunTotal rngS normS userData


-- Yuk, the last bit of boilerplate!
-- Returns a function takes the user data 
-- and produces a result.
getResultFn :: Int -> RngType -> NormalType -> ( MonteCarloUserData -> Double )
getResultFn numOfSims rng (StateBoxMuller bm) = getRngFn numOfSims rng $ bm
getResultFn numOfSims rng (StateAcklam ack)   = getRngFn numOfSims rng $ ack

-- Separating the decision across two functionals
-- reduces the amount of boilerplate.
-- Consider if we have 3 rngs and 3 normal generators
-- then under one functional we would have 3x3=9 combinations.
-- This way we only speicfy each type once, we have 3+3=6 combinations.
-- Another way to think of is that if we added a new Rng we would only
-- have to update the below function with 1 line.  If it was done
-- in one function we would have a case for each NormalType.
getRngFn :: NormalClass a => Show a =>
            Int -> RngType -> ( a -> MonteCarloUserData -> Double)
getRngFn numOfSims (RngTypeHalton halton) = simResult numOfSims 0 halton 
getRngFn numOfSims (RngTypeRanq1  ranq1)  = simResult numOfSims 0 ranq1  


main :: IO()
main = do {-putStrLn "Random Number Generator?"
          userRng <- getLine
          putStrLn "Normal Generator?"
          userNorm <- getLine
          putStrLn "Strike?"
          userStrike <- getLine
          putStrLn "Underlying Price?"
          userUnderlying <- getLine
          putStrLn "Put or Call?"
          userPutCall <- getLine
          putStrLn "Volatility?"
          userVolatility <- getLine
          putStrLn "Expiry?"
          userExpiry <- getLine
          putStrLn "Interest Rate?"
          userInterestRate <- getLine
          putStrLn "Iterations?"
          userIterations <- getLine

          let userData = MonteCarloUserData { strike       = read(userStrike), 
                                              underlying   = read(userUnderlying), 
                                              putCall      = read(userPutCall),
                                              volatility   = read(userVolatility),  
                                              expiry       = read(userExpiry), 
                                              interestRate = read(userInterestRate), 
                                              iterations   = read(userIterations) }   -}    

          -- BS = 5.191
          let userData = MonteCarloUserData { strike       = 52, --100, 
                                              underlying   = 50, --100, 
                                              putCall      = Call,
                                              volatility   = 0.4, --0.2,  
                                              expiry       = 5/12, --1, 
                                              interestRate = 0.1, --0.05,
                                              timeSteps    = 10,
                                              evolveFn     = evolveStandard }                      
              numOfSims = 5000
              userRng = "Halton"
              userNorm = "Box Muller"
              normalType = normalChooser userNorm
              -- Yuk, for QRNG we need to know our dimensionality
              -- before we start to simulate.

              -- THEROY 1 - +1 to any odd time steps
              -- This is rather nasty too, as Box Muller should
              -- only be used with EVEN time steps.  Consider
              -- if we have 3 time steps... on the final step
              -- we will generate TWO normals using bases 5,7.
              -- When we start our next simulation we will ask
              -- for a normal, and our Monad will inform us it
              -- has one saved.  But it generated with base 7,
              -- (our +1) not base 2.  So you get this sort of thing:
              -- 2 3 5 7 2 3 5 7 2 3 5 7 2 3 5 
              -- 1 1 1 2 2 2 3 3 3 4 4 4 5 5 5
              --     ***   ***   ***   ***
              -- We are not using the same prime base for each
              -- timestep on each iteration. We cycle every 4
              -- simulations.  This is probably bad!
              -- Simple way to deal with is to destroy the final
              -- normal state after each single run... but this
              -- is NOT good for PRNG.
              -- As timeSteps goes large it becomes less of an 
              -- issue for PRNG, so we could ignore.
              
              -- For now it is up to the user to be sensible,
              -- the code below prevents the VERY disasterous
              -- scenario where we initalise Halton with say 
              -- 1 timestep and do not increment.  Thus we get
              -- the same base used for both rns for normal generation.
              -- This is because each timestep repeats the same prime
              -- sequence, which here will be 2,2,2,2,2....
              -- Thus adjacent sims are always used to generate each
              -- normal and we will use [2,2] as our base pair.  Not good!
              -- If we +1 we get points on adjacent sims being valued using
              -- a different base.  It will oscilate between 2 and 3.  But
              -- results show this is at least more acceptable.

              -- THEORY 2 - only +1 for disasterous single time step scenario
              -- For the case of >3 steps being used the situaion is less clear.
              -- Here there is cross-pollination for adjacent simulations but
              -- Only on the last (and first) time step.  So for 5 steps we get.
              -- 2 3 5 7 11 2 3 5 7 11 2 3 5 7 11
              -- 1 1 1 1 1  2 2 2 2 2  3 3 3 3 3
              --         ****       ****       *** and so on.
              -- On seconds thoughts as time steps go large this
              -- has to better than arbitarilly adding a base to odd realisations.  All
              -- but two of our timesteps are being valued using adjacent
              -- bases in the same simulation number.  So for now we will
              -- say if ts=1 then 2 otherwise ts.

              -- OK, don't ask me why but the odd+1 theory works much better in practice.
              -- It is also the method used in Brandimarte, 2006 (with no explanation).
              -- So let's stick with that!
              ts = let ts' = timeSteps userData
                      in if even ts' then ts' else ts' + 1
              
              rngType          = rngChooser userRng ts
              sumOfPayOffs     = getResultFn numOfSims rngType normalType $ userData
              averagePayOff    = sumOfPayOffs / fromIntegral numOfSims
              discountedPayOff = averagePayOff * exp (-1 * interestRate userData * expiry userData)
          putStrLn "Result:"
          print discountedPayOff



