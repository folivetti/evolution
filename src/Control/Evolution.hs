module Control.Evolution where

import Control.Monad.State.Strict
import Control.Monad.Reader
import System.Random
import Data.Monoid (Sum(..))
import Data.Vector (Vector(..), (!))
import qualified Data.Vector as V
import Control.Scheduler                 (traverseConcurrently, Comp(..))
import Control.DeepSeq                   (force)

type Rnd a = StateT StdGen IO a

type Population a = Vector a

class Ord a => Solution a where
  _evaluate   :: a -> Rnd a
  _getFitness :: a -> Double
  _isFeasible :: a -> Bool

data Interpreter a = Funs { _cx        :: Crossover -> [a] -> Rnd a
                          , _mut       :: Mutation -> a -> Rnd a
                          , _reproduce :: Reproduction -> Population a -> Population a -> Rnd (Population a)
                          , _select    :: Selection -> Population a -> Rnd a
                          , _filter    :: Predicate -> Population a -> Population a
                          , _create    :: Rnd a
                          } 

data Reproduction = Generational 
                  | KeepBest 
                  | ReplaceWorst 
                  | Probabilistic Selection
                  | CustomReproduction1
                  | CustomReproduction2
                  deriving (Show, Read)

data Selection = Tournament Int 
               | RouletteWheel 
               | SUS
               | CustomSelection1
               | CustomSelection2
               deriving (Show, Read)

data Crossover = OnePoint 
               | NPoints Int
               | CustomCX1
               | CustomCX2
               deriving (Show, Read)

data Mutation = SwapTree 
              | InsertVar
              | CustomMut1
              | CustomMut2
              deriving (Show, Read)

data Predicate = Feasible 
               | Infeasible
               | All
               deriving (Show, Read)

-- | DSL of an evolutionary process
data Evolution = Reproduce Reproduction Predicate EvoCycle Predicate EvoCycle
  deriving (Show, Read)

data EvoCycle = Cross Crossover Int Double Selection EvoCycle
              | Mutate Mutation Double EvoCycle
              | End
              deriving (Show, Read)

randomDbl :: StateT StdGen IO Double
randomDbl = state (randomR (0.0, 1.0))

evalCycle :: Solution a 
          => EvoCycle
          -> Population a
          -> a
          -> ReaderT (Interpreter a) (StateT StdGen IO) a
evalCycle End pop p  = pure p

evalCycle (Cross cx nParents pc sel evo) pop p = do
  prob    <- lift randomDbl
  cxf     <- asks _cx
  choose  <- asks _select
  parents <- lift $ replicateM nParents (choose sel pop)
  child   <- if prob < pc
              then lift $ cxf cx parents >>= _evaluate
              else pure $ head parents
  evalCycle evo pop child

evalCycle (Mutate mut pm evo) pop p = do
  prob  <- lift randomDbl
  mutf  <- asks _mut
  child <- if prob < pm
              then lift $ mutf mut p >>= _evaluate
              else pure p
  evalCycle evo pop child

evalEvo :: Solution a 
        => Evolution 
        -> Population a 
        -> [StdGen]
        -> ReaderT (Interpreter a) (StateT StdGen IO) (Population a, [StdGen])
evalEvo (Reproduce rep pred1 evo1 pred2 evo2) pop gs = do
  config       <- ask
  keeper       <- asks _filter
  (pop1, gs1)  <- case evo1 of
                    End -> pure (keeper pred1 pop, gs)
                    _   -> runPar evo1 config gs (keeper pred1 pop)
  (pop2, gs2)  <- case evo2 of
                    End -> pure (keeper pred2 pop, gs1)
                    _   -> runPar evo2 config gs1 (keeper pred2 pop)
  repFun <- asks _reproduce
  pop' <- lift $ repFun rep pop1 pop2
  return (pop', gs2)
    where
      runPar evo cfg g pop' = splitResponse
                            $ traverseConcurrently (ParN 0) (runCycle evo cfg pop') 
                            $ zip (V.toList pop') g

      splitResponse = fmap (go ([], [])) . liftIO 
        where
          go (accP, accG) []          = (V.fromList accP, accG)
          go (accP, accG) ((p,g):pgs) = go (p:accP, g:accG) pgs

      runCycle evo conf pop' (ix, g) = flip runStateT g $ runReaderT (evalCycle evo pop' ix) conf

genEvolution :: Solution a
             => Int 
             -> Int 
             -> (Population a -> IO ()) 
             -> Evolution 
             -> ReaderT (Interpreter a) (StateT StdGen IO) ([Double], a)
genEvolution nGens nPop logger evo = do
  createSol <- asks _create
  pop0      <- lift $ V.replicateM nPop (createSol >>= _evaluate)
  g         <- lift get
  liftIO $ logger pop0
  go nGens pop0 ([avgFit pop0], V.minimum pop0) $ splitGensWithIndex nPop g 
    where
      go 0 _   (avgs, best) gs = return (reverse avgs, best)
      go n pop (avgs, best) gs = do (pop', gs') <- evalEvo evo pop gs
                                    liftIO $ logger pop'
                                    let avgs' = avgFit pop' : avgs
                                        best' = getBest best pop'
                                    go (n-1) pop' (avgs', best') gs'

runEvolution :: Solution a 
             => Int
             -> Int
             -> (Population a -> IO ())
             -> Evolution
             -> StdGen
             -> Interpreter a
             -> IO ([Double], a)
runEvolution nGens nPop logger evo g = flip evalStateT g . runReaderT (genEvolution nGens nPop logger evo)


splitGensWithIndex :: Int -> StdGen -> [StdGen]
splitGensWithIndex n = map fst . take n . iterate (\(g1,g2) -> split g2) . split 

avgFit :: Solution a => Vector a -> Double
avgFit pop = getSum tot / fromIntegral (V.length pop)
  where
    tot = foldMap (Sum . _getFitness) pop

getBest :: Solution a => a -> Vector a -> a
getBest best pop
  | V.null feasible && _isFeasible best = best
  | V.null feasible                     = min best (V.minimum pop)
  | _isFeasible best                    = min best (V.minimum feasible)
  | otherwise                           = V.minimum feasible
  where
    feasible = V.filter _isFeasible pop
