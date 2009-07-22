import Control.Monad.State.Strict
import Data.Word (Word64)
import Data.Bits (shift,xor)

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

primes = [2,3,5,7] ::[Int]

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

-- Polymorphic function to evolve our chose RNG - the clever bit :-)
result :: RngClass a => Int -> a -> [[Double]]
result dims state = evalState (do replicateM 10 (rngStateFn dims)) state


-- In somewhat of a paradox we need
-- to unite our purposefully different state types
-- so that they can be passed around under a single
-- guise.  We use 'data', because 'newtype' must
-- have exactly one constructor.
data StateType = StateHalton Halton | 
                 StateRanq1  Ranq1

-- Boilerplate - must tell the compiler how
-- to deal with the different underlying types.
-- As i and w are different types a different
-- expression is needed.... unless we delve
-- into Haskel templates :-)
printResult :: StateType -> Int -> IO ()
printResult (StateHalton halton) dim   = print $ result dim halton
printResult (StateRanq1  ranq1)  dim   = print $ result dim ranq1

-- Create a StateType containing the underlying
-- inital state of our RNG depedning on user input.
rngChooser :: String -> StateType
rngChooser rngStr | rngStr == "Halton" = StateHalton (Halton 1)
                  | rngStr == "Ranq1"  = StateRanq1  (Ranq1 (ranq1Init 1))
                  | otherwise          = StateHalton (Halton 10)

main :: IO()
main = do userRng <- getLine
          dims <- getLine
          let iDims = read(dims) :: Int
          printResult (rngChooser userRng) iDims




