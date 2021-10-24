{-# language BangPatterns #-}

module Control.Evolution 
  ( runEvolution
  , Rnd(..)
  , Population(..)
  , Solution(..)
  , Interpreter(..)
  , Reproduction(..)
  , Selection(..)
  , Crossover(..)
  , Mutation(..)
  , Predicate(..)
  , Evolution(..)
  , EvoCycle(..)
  )
  where

import Control.DeepSeq                   (force, NFData)
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Scheduler                 (traverseConcurrently, Comp(..))
import System.Random
import Data.Monoid                       (Sum(..))
import Data.Vector                       (Vector(..), (!))
import qualified Data.Vector             as V

-- | A random element is the state monad with StdGen as a state
type Rnd a        = StateT StdGen IO a

-- | A population of solutions is a vector 
type Population a = Vector a

-- | A Solution class must implement a function to 
-- evaluate this solution, a function to retrieve its fitness
-- and another function to check the feasibility of the solution.
--
-- The evaluation function is inside the state monad as it may
-- need to sample some instances to evaluate (i.e., lexicase selection).
class Ord a => Solution a where
  _getFitness :: a -> Double
  _isFeasible :: a -> Bool

-- | The `Interpreter` data type stores all the necessary functions
-- for the evolution process. Each of these functions should
-- receive the type of operation as the first parameter and throw an error
-- in case an operator is unsupported:
--
-- `_cx OnePoint pop = myOnePoint pop`
-- `_cx NPoints  pop = error "NPoints crossover not implemented for this algorithm"`
data Interpreter a = Funs { _cx        :: Crossover -> [a] -> Rnd a
                          , _mut       :: Mutation -> a -> Rnd a
                          , _reproduce :: Reproduction -> Population a -> Population a -> Rnd (Population a)
                          , _select    :: Selection -> Population a -> Rnd a
                          , _filter    :: Predicate -> Population a -> Population a
                          , _create    :: Rnd a
                          , _evaluate  :: a -> Rnd a
                          } 

-- | Reproduction algorithms
--
-- * Generational: the children replaces the parent population
-- * KeepBest: generational but keeping the best individual (either from parent or child population)
-- * ReplaceWorst: replaces the worst individuals with new random solutions or sampled solutions from the parent
-- * Probabilistic: sample individuals from the combined parent+child populations using a Selection strategy
-- * CustomReproduction1/2: custom reproduction strategies not covered by this data type 
data Reproduction = Generational 
                  | KeepBest 
                  | ReplaceWorst 
                  | Probabilistic Selection
                  | CustomReproduction1
                  | CustomReproduction2
                  deriving (Show, Read)

-- | Selection algorithms
--
-- * Tournament: tournament selection, samples n solutions and returns the best
-- * RouletteWheel: roulette wheel, calculates the probability of choosing an individual proportional to its fitness and sample an individual using this probability distribution
-- * SUS: similar to roulette wheel, but samples the entire population at once by using multiple evenly spaced arrows with the wheel
-- * CustomSelection1/2: custom selection strategies not covered by this data type 
data Selection = Tournament Int 
               | RouletteWheel 
               | SUS
               | CustomSelection1
               | CustomSelection2
               deriving (Show, Read)

-- | Crossover algorithms
--
-- * OnePoint: one-point crossover, exchange chromossome material on a single point
-- * NPoints: n-points crossover, exchange chromossome material on a n points
-- * CustomCX1/2: custom crossover strategies not covered by this data type 
data Crossover = OnePoint 
               | NPoints Int
               | CustomCX1
               | CustomCX2
               deriving (Show, Read)

-- | Mutation algorithms
--
-- * SwapTree: swap two branches of an expression tree (GP)
-- * InsertVar: inserts a variable into an expression tree (GP)
-- * LocalSearch: applies a local search strategy
-- * Uniform: changes each element with probability associated with the mutation 
-- * CustomMut1/2: custom mutation strategies not covered by this data type 
data Mutation = SwapTree 
              | InsertNode -- Var/Term
              | RemoveNode -- Var/Term
              | ChangeNode -- Var/Function/Exponent
              | ReplaceSubTree
              | GroupMutation -- choose from one of the mutation operators
              | LocalSearch
              | Uniform 
              | CustomMut1
              | CustomMut2
              deriving (Show, Read)
                  
-- | Predicate for population filter 
--
-- * Feasible: selects only the feasible solutions
-- * Infeasible: selects only the infeasible solutions
-- * All: selects the entire population
data Predicate = Feasible 
               | Infeasible
               | All
               deriving (Show, Read)

-- | DSL of an evolutionary process
--
-- The evolutionary process is composed of a reproduction strategy between 
-- two evolutionary cycles applied to a filtered population.
data Evolution = Reproduce Reproduction Predicate EvoCycle Predicate EvoCycle
  deriving (Show, Read)

-- | An evolution cycle is composed of a sequence of
-- crossover and mutation operations. The order and quantity of each operation 
-- is arbitrary.
--
-- * Cross: applies a `Crossover` operator to `n` individuals with `p` probability
--          and sampling individuals using `Selection` strategy
-- * Mutate: applies a `Mutation` operator with probability `p`
--
-- Common evolution cycle: 
--
-- Cross OnePoint 2 0.8 (Tournament 2) 
--      (Mutate SwapTree 0.2 End)
--
-- With Local Search: 
--
-- Cross OnePoint 2 0.8 (Tournament 2) 
--      (Mutate SwapTree 0.2
--         (Mutate LocalSearch 1.0 End))
--
data EvoCycle = Cross Crossover Int Double Selection EvoCycle
              | Mutate Mutation Double EvoCycle
              | End
              deriving (Show, Read)

randomDbl :: StateT StdGen IO Double
randomDbl = state (randomR (0.0, 1.0))

-- | Evaluates an evolution cycle.
-- Each step involves the immutable population as a context
-- and an individual as focus.
-- By the end of the cycle, a single individual will be created.
evalCycle :: (Solution a, NFData a)
          => EvoCycle
          -> Population a
          -> a
          -> ReaderT (Interpreter a) (StateT StdGen IO) a
evalCycle End pop p  = pure p

evalCycle (Cross cx nParents pc sel evo) pop !p = do
  prob    <- lift randomDbl
  cxf     <- asks _cx
  choose  <- asks _select
  eval    <- asks _evaluate 
  parents <- lift $ replicateM nParents (choose sel pop)
  child   <- if prob < pc
              then lift $ (cxf cx parents >>= eval)
              else pure $ head parents
  evalCycle evo pop child

evalCycle (Mutate mut pm evo) pop !p = do
  prob  <- lift randomDbl
  mutf  <- asks _mut
  eval  <- asks _evaluate
  child <- if prob < pm
              then lift $ (mutf mut p >>= eval)
              else pure p
  evalCycle evo pop child

-- | Evaluates the evolution process in parallel
-- The evolution process generates two branches with each
-- branch generating a population. After that, it apples the
-- reproduction strategy to generate the next population.
-- Each individual of the new populations are generated in parallel 
-- using the evolution cycle. 
evalEvo :: (Solution a, NFData a)
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

      runCycle evo conf pop' (ix, g) = runStateT (runReaderT (evalCycle evo pop' ix) conf) g

-- | Generates the evolutionary process to be evaluated using `runEvolution`
genEvolution :: (Solution a, NFData a)
             => Int 
             -> Int 
             -> (Population a -> IO ()) 
             -> Evolution 
             -> ReaderT (Interpreter a) (StateT StdGen IO) ([Double], a)
genEvolution nGens nPop logger evo = do
  createSol <- asks _create
  eval      <- asks _evaluate 
  pop0      <- lift $ V.replicateM nPop (createSol >>= eval)
  g         <- lift get
  liftIO $ logger pop0
  go nGens pop0 ([avgFit pop0], V.minimum pop0) $ splitGensWithIndex nPop g 
    where
      go 0 _   (!avgs, !best) gs = return (reverse avgs, best)
      go n pop (!avgs, !best) gs = do (pop', gs') <- evalEvo evo pop gs
                                      liftIO $ logger pop'
                                      let avgs' = force $ avgFit pop' : avgs
                                          best' = getBest best pop'
                                      go (n-1) pop' (avgs', best') gs'

-- | Runs the evolutionary process 
runEvolution :: (Solution a, NFData a)     -- ^ for a type `a` representing a solution 
             => Int                        -- ^ number of generations 
             -> Int                        -- ^ population size
             -> (Population a -> IO ())    -- ^ a logger function to run at every generation 
             -> Evolution                  -- ^ Evolutionary process 
             -> StdGen                     -- ^ random number generator
             -> Interpreter a              -- ^ Interpreter of evolutionary process 
             -> IO ([Double], a)           -- ^ returns the average fitness of every generation and the final champion
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
