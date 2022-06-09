{-# language TypeFamilies #-}

import Control.Evolution
import Control.Monad.State.Strict
import qualified Data.Vector as V
import System.Random
import Control.DeepSeq
import Control.Evolution (crowdingDistance)

data Test = Test { _chromo   :: [Double]
                 , _fit      :: [Double]
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

data TestMoo = TestMoo { _chromoMoo :: [Int]
                       , _fitMoo :: [Double]
                       , _feasibleMoo :: Double
                       } deriving Show

instance Eq TestMoo where
  t1 == t2 = all (uncurry (==)) $ zip (_fitMoo t1) (_fitMoo t2)
instance Ord TestMoo where
  t1 <= t2 = (any (uncurry (<)) (zip (_fitMoo t1) (_fitMoo t2)) && all (uncurry (<=)) (zip (_fitMoo t1) (_fitMoo t2)))
             || t1 == t2
instance Solution TestMoo where 
  _getFitness = _fitMoo -- for roulette wheel 
  _isFeasible = (==0) . _feasibleMoo

instance NFData TestMoo where 
  rnf _ = ()

toss :: StateT StdGen IO Bool
toss = state random
  
calcFitness t = pure $ t{ _fit = [sum (_chromo t)]}

calcFitnessMoo t = do
  let cp = _chromoMoo t
      tr = sum cp
      fs = product cp 
  pure $ t{ _fitMoo = [fromIntegral tr, fromIntegral fs] }

testCreate = do
  b1 <- state random
  b2 <- state random
  pure $ Test [b1, b2] [0.0] 0.0

testCreateMoo = do
  cp <- replicateM 5 (state $ randomR (1, 5)) 
  pure $ TestMoo cp [0.0, 0.0] 0.0

instance EvoClass Test where 
  data Crossover Test = OnePoint deriving (Show, Read)
  data Mutation  Test = SwapTree deriving (Show, Read)

instance EvoClass TestMoo where 
  data Crossover TestMoo = OnePointBin deriving (Show, Read)
  data Mutation  TestMoo = SwapBits deriving (Show, Read)

testCX _ [p1,p2] = do
  liftIO $ putStrLn "cx"
  t <- toss
  let cp1 = _chromo p1
      cp2 = _chromo p2 
  if t 
     then pure (Test [cp1 !! 0, cp2 !! 1] [0.0] 0.0)
     else pure (Test [cp2 !! 0, cp1 !! 1] [0.0] 0.0)
testCX _ _ = testCreate

testMut _ p = do
  let cp = _chromo p
  liftIO $ putStrLn "mutate"
  t <- toss
  r <- state (randomR (1, 5))
  if t
    then pure (Test [cp !! 0, r] [0.0] 0.0)
    else pure (Test [r, cp !! 1] [0.0] 0.0)

testCXMoo _ [p1, p2] = do
  t <- toss
  let cp1 = _chromoMoo p1
      cp2 = _chromoMoo p2
  if t 
    then pure (TestMoo (take 3 cp1 <> take 2 cp2) [0.0, 0.0] 0.0)
    else pure (TestMoo (take 3 cp2 <> take 1 cp1) [0.0, 0.0] 0.0)

testMutMoo _ p = do 
  let cp = _chromoMoo p
  t <- toss
  r <- state random
  if t 
    then pure (TestMoo (r : tail cp) [0.0, 0.0] 0.0)
    else pure (TestMoo cp [0.0, 0.0] 0.0)

interpret :: Interpreter Test
interpret = Funs testCX testMut testCreate calcFitness

interpretMoo :: Interpreter TestMoo
interpretMoo = Funs testCXMoo testMutMoo testCreateMoo calcFitnessMoo

evo = Reproduce Generational 
        [With AllOf 
           :> Cross OnePoint 2 0.3 (Tournament 2) 
           :> Mutate SwapTree 0.7 
           :> Done
        ]

main :: IO ()
main = do
  let g = mkStdGen 42 -- <- getStdGen 
  (xs, x, _) <- runEvolution 10 10 (\_ -> return ()) evo g interpret 
  print x
  pop <- evalStateT (replicateM 100 (testCreateMoo >>= calcFitnessMoo)) g
  mapM_ print pop
  let fronts = fastNondominatedSort $ V.fromList pop
      ds = crowdingDistance (V.fromList pop) (fronts !! 14) 
  print fronts 
  print ds
