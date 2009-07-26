
import Control.Monad.State.Strict
import Data.Word (Word64)
import Data.Bits (shift,xor)
-- Unboxing the arrays doesn't really buy us much
-- but I consider it more correct for this 'Array' will probably
-- introduce one level of indirection (pointer to array value),
-- as all values are going to be used, boxing is pointless.
import Data.Array.Unboxed


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

-- (3,5) gives us better distrubtion properites for Box Muller with Halton
-- hence I've dropped the '2'.  I'm not going to discuss why here!
-- This method is very simple and OK for n<1000.  
-- If you're using Halton >13D, you're not too smart, so 1000 is more than fine!
primes :: [Int]
primes = sieve [3..]
   where sieve (p:xs) = p : sieve [x | x<-xs, x `mod` p /= 0]


-- The underlying state of a halton sequence
-- is just an int, but if we want to have
-- many instances of RngClass each with
-- an underlying state of Int, then
-- we must differentiate.  We need a
-- a *new* type exclusive of, but synonmous
-- with an int.  We reduce this to an Int
-- once we've drilled through the polymorphism.
newtype Halton = Halton Int

-- Each instance produces a lambda function on
-- the fly which is hardcoded to the chosen number
-- of dimensions.  This extra level of abstraction
-- allows us to avoid carrying around the constant dimensions
-- as part of the (variable) state.
instance RngClass Halton where
  rngStateFn dims = let bases = take dims primes
                      in State $ \(Halton s) -> (map (reflect (s,1,0)) bases,Halton (s+1))

-- Ranq1 Implementation

newtype Ranq1 = Ranq1 Word64

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

-- Reversing isn't ideal - but it is better than appending to the end each time.
-- The list is only rebuilt once with a reverse and will be O(Dimensions).
-- Not expecting a huge amount of dimensions, so this will suffice for now.
-- http://www.haskell.org/pipermail/beginners/2009-February/000882.html
multiRanq1 :: ([Double],Word64) -> Int -> ([Double], Word64)
multiRanq1 (vector,state) dims | dims <= 0 = (reverse vector,state)
                               | otherwise = multiRanq1 (newVector,newState) (dims-1)
                                               where
                                                  newVector = convertToDouble state : vector
                                                  newState  = ranq1Increment state 
                                               
instance RngClass Ranq1 where
   rngStateFn dims = State $ \(Ranq1 s) -> let (vector,state) = multiRanq1 ([],s) dims
                                              in (vector, Ranq1 state)


-- In somewhat of a paradox we need
-- to unite our purposefully different state types
-- so that they can be passed around under a single
-- guise.  We use 'data', because 'newtype' must
-- have exactly one constructor.
data RngType = StateHalton Halton | 
               StateRanq1  Ranq1

-- Create a StateType containing the underlying
-- inital state of our RNG depedning on user input.
rngChooser :: String -> RngType
rngChooser rngStr | rngStr == "Halton" = StateHalton (Halton 1)
                  | rngStr == "Ranq1"  = StateRanq1  (Ranq1 (ranq1Init 1))
                  | otherwise          = StateHalton (Halton 10)



-- Now for Normals

newtype BoxMuller = BoxMuller (Maybe Double)
newtype Acklam    = Acklam ()

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
   nextNormal = StateT $ \(BoxMuller s) ->  case s of
                                               Just d  -> return (d,BoxMuller Nothing)
	                                       Nothing -> do rn1:rn2:rns <- (rngStateFn 2)
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
invnorm p | p < 0.02425 = let q = sqrt ( -2*log(p) ) 
                          in (((((c!1*q+c!2)*q+c!3)*q+c!4)*q+c!5)*q+c!6) / ((((d!1*q+d!2)*q+d!3)*q+d!4)*q+1)
                          
                            
          | p > (1-0.02425) = let q = sqrt ( -2*log(1-p) ) 
                              in -(((((c!1*q+c!2)*q+c!3)*q+c!4)*q+c!5)*q+c!6) / ((((d!1*q+d!2)*q+d!3)*q+d!4)*q+1)
                              

          | otherwise = let q = p-0.5
                            r = q*q
                        in (((((a!1*r+a!2)*r+a!3)*r+a!4)*r+a!5)*r+a!6)*q / (((((b!1*r+b!2)*r+b!3)*r+b!4)*r+b!5)*r+1) 
                            
-- Acklam's method doesn't require any 
-- state so we are effectively creating
-- a stateless state monad transformer!
-- Compiler should (hopefully) recognise this!
instance NormalClass Acklam where
   nextNormal = StateT $ \_ -> do rn:rns <- (rngStateFn 1)
                                  return ( invnorm rn, Acklam () )


-- Here's the polymorphism!  Evaluate a monad stack
-- where the inner monad is of RngClass and outer is of NormalClass
result :: RngClass a => NormalClass b => a -> b -> [Double]
result initRngState initNormState = evalState normalState initRngState
                                        where normalState = evalStateT ( do replicateM 10 nextNormal ) initNormState

normalChooser :: String -> NormalType
normalChooser normStr | normStr == "Box Muller" = StateBoxMuller (BoxMuller Nothing)
                      | normStr == "Acklam"     = StateAcklam (Acklam () )
                      | otherwise               = StateBoxMuller (BoxMuller Nothing)


-- Yuk, the last bit of boilerplate!
printResult :: RngType -> NormalType -> IO ()
printResult (StateHalton halton) (StateBoxMuller bm)  = print $ result halton bm
printResult (StateRanq1  ranq1)  (StateBoxMuller bm)  = print $ result ranq1 bm
printResult (StateHalton halton) (StateAcklam ack)    = print $ result halton ack
printResult (StateRanq1  ranq1)  (StateAcklam ack)    = print $ result ranq1 ack



main :: IO()
main = do userRng <- getLine
          userNorm <- getLine
          printResult (rngChooser userRng) (normalChooser userNorm)


