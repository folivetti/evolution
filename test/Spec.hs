{-# language TypeFamilies #-}

import Control.Evolution
import Control.Monad.State.Strict
import qualified Data.Vector as V
import System.Random
import Control.DeepSeq

data Test = Test { _chromo   :: [Double]
                 , _fit      :: Double
                 , _feasible :: Double
                 } deriving Show

instance Eq Test where
  t1 == t2 = _fit t1 == _fit t2
instance Ord Test where
  t1 <= t2 = _fit t1 <= _fit t2

instance Solution Test where
  _getFitness = _fit 
  _isFeasible = (==0) . _feasible

instance NFData Test where
  rnf _ = ()

toss :: StateT StdGen IO Bool
toss = state random
  
calcFitness t = pure $ t{ _fit = sum (_chromo t) }

testCreate = do
  b1 <- state random
  b2 <- state random
  pure $ Test [b1, b2] 0.0 0.0

instance EvoClass Test where 
  data Crossover Test = OnePoint deriving (Show, Read)
  data Mutation  Test = SwapTree deriving (Show, Read)

testCX _ [p1,p2] = do
  liftIO $ putStrLn "cx"
  t <- toss
  let cp1 = _chromo p1
      cp2 = _chromo p2 
  if t 
     then pure (Test [cp1 !! 0, cp2 !! 1] 0.0 0.0)
     else pure (Test [cp2 !! 0, cp1 !! 1] 0.0 0.0)
testCX _ _ = testCreate

testMut _ p = do
  let cp = _chromo p
  liftIO $ putStrLn "mutate"
  t <- toss
  r <- state random
  if t
    then pure (Test [cp !! 0, r] 0.0 0.0)
    else pure (Test [r, cp !! 1] 0.0 0.0)

interpret :: Interpreter Test
interpret = Funs testCX testMut testCreate calcFitness

evo = Reproduce Generational 
        [With AllOf 
           :> Cross OnePoint 2 0.3 (Tournament 2) 
           :> Mutate SwapTree 0.7 
           :> Done
        ]

main :: IO ()
main = do
  g <- getStdGen 
  (xs, x) <- runEvolution 10 10 (\_ -> return ()) evo g interpret 
  print x
