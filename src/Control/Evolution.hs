{-# language BangPatterns #-}
{-# language TypeFamilies #-}
{-# language FlexibleContexts #-}
{-# language PolyKinds #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language TupleSections #-}
{-|
Module      : Control.Evolution 
Description : a generic evolutionary algorithm DSL
Copyright   : (c) Fabricio Olivetti de Franca, 2022
License     : GPL-3
Maintainer  : fabricio.olivetti@gmail.com
Stability   : experimental
Portability : POSIX

The evolution package supports the creation of evolutionary and bio-inspired algorithms
with an user friendly domain specific language.

The package provides the main reproduction and selection strategies, 
a DSL to describe the evolution process, a function that runs the 
evolution algorithm with support to concurrency, and a customizable
DSL to describe the Crossover and Mutation operators for specific
class of problems.

The main idea is that the user focus only on implementing the 
crossover and mutation operators, the individual representation,
a fitness function, and a random individual generator. After that,
the user can customize the main loop of the evolutionary process
using the DSL and without worrying about implementing the process.

__Warning:__ the main objective of this package is to create a minimal
DSL capable of describing (almost) every bio-inspired search algorithm. As such,
every new version can potentially break compatibility with the previous one.

Example - Simple GA:

Let's suppose we are working with a binary representation and we want
to support a one-point crossover, single point mutation, multi-point mutation.

We must first create a data type with the chromossome and a fitness field and
add support to `Eq, Ord` classes by comparing the fitness:

@
data Bin = Bin { _chromo :: [Bool]
               , _fit :: [Double]
               }
               
instance Eq Bin where
  t1 == t2 = _fit t1 == _fit t2
instance Ord Bin where
  t1 <= t2 = _fit t1 <= _fit t2
@

After that, we instantiate the `Solution` class to indicate
how to retrieve the fitness of the individual and check
whether it is feasible or not.

@
instance Solution Bin where
  _getFitness = _fit 
  _isFeasible = const 0.0 -- always feasible
@

Next, we instantiate the `EvoClass` to create
the DSL for the crossover and mutation operators:

@
instance EvoClass Bin where 
  data Crossover Bin = OnePoint deriving (Show, Read)
  data Mutation  Bin = SinglePoint | MultiPoints deriving (Show, Read)
@

Finally, we implement the required functions and create an `Interpreter`:

@
cx OnePoint [p1,p2] = do
  let c1 = _chromo p1
      c2 = _chromo p2
  ix <- randomIndex (length c1)
  let c = take ix c1 \<> drop ix c2
  pure $ Bin c 0.0 -- the individual will be evaluated later
  
mut SinglePoint (Bin c f) = do
  ix <- randomIndex (length c)
  let c' = swap ix c
  pure $ Bin c' 0.0
mut MultiPoints (Bin c f) = do
  c' <- traverse swapRnd c  
  pure $ Bin c' 0.0
  
randomIndividual n = do
    c \<- take n \<$> state randoms
    pure $ Bin c 0.0
    
-- how many True in the chromossome
fitness (Bin c _) = pure $ Bin c (length $ filter id c)

interpret :: Interpreter Test
interpret = Funs cx mut randomIndividual fitness
@

We can create a customized logger to store the information
at every generation.

@
logger :: Population a -> IO ()
logger pop  = do
  let best  = getBestFitness pop
      worst = getBestFitness worst
      avg   = getAvgFitness
  print $ "The best fitness is: " \<> show best
  print $ "The worst fitness is: " \<> show worst
  print $ "The avg fitness is: " \<> show avg
@

Now you can define your evolutionary algorithm using the DSL and
run it using `runEvolution`

@
evo = Reproduce Generational 
        [With AllOf 
           :> Cross OnePoint 2 0.3 (Tournament 2) 
           :> Mutate SinglePoint 0.7 
           :> Done
        ]

-- run the evolution with 100 individuals for 500 generations
-- and random seed `g`
runEvolution 500 100 logger evo g interpret
@

We can, for example, create a an evolutionary algorithm that 
creates two child population: one with crossover and mutation
and another with only mutation, and then we reproduce using 
tournament selection.

@
evo = Reproduce (Probabilistic (Tournament 2))
        [ With AllOf :> Cross OnePoint 2 0.3 (Tournament 2) :> Mutate SinglePoint 0.7 :> Done
        , With AllOf :> Mutate MultiPoints 0.9 :> Done
        ]
@

We can even parse the evolution process from a file. If we have a file "ga.evo" with the content

@
Reproduce Generational 
    [With AllOf 
       :> Cross OnePoint 2 0.3 (Tournament 2) 
       :> Mutate SinglePoint 0.7 
       :> Done
    ]
@

We can automatically parse as:

@
evo \<- read \<$> readFile "ga.evo"
@

The evolution DSL is described as a reproduction operator
followed by a list of evolution cycles.

@
Reproduce \<reproduction operator> [\<evolution cycle>]
@

The evolution cycle can be either `Parent`, when you just want
to pass the current population, `<operator> :> <evolution cycle>`, 
when you want to chain two operators, and `Done`, marking the end of
the cycle.

The operator can be 

@
Cross \<crossover operator> \<n-parents> \<probability> \<selection operator>
Mutate \<mutation operator> \<probability>
With \<predicate>
@

The `With` operator is used to filter the population and can be `AllOf` (no filter),
`Feasible`, `Infeasible`.

-}
module Control.Evolution 
  ( runEvolution
  , Rnd(..)
  , EvoClass(..)
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
  , Op(..)
  , fastNondominatedSort
  , crowdingDistance
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
import Data.List                         (sort, sortBy, groupBy, (\\), nubBy, minimumBy)
import Data.Function                     (on)
import qualified Data.IntMap.Strict      as M
import qualified Data.Set      as S
import Data.Bifunctor (first, second)
import Data.Array.ST (runSTArray, readArray, newArray, writeArray)
import Data.Array                        (array)
import qualified Data.Array as A 
import Data.Time.Clock.POSIX ( getPOSIXTime )
import Control.Monad ( replicateM, forM_ )

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
  _getFitness :: a -> [Double]
  _isFeasible :: a -> Bool
  -- _distance should calculate a normalized distance between two solutions
  _distance :: a -> a -> Double
  _distance _ _ = 1.0


-- | The `Interpreter` data type stores all the necessary functions
-- for the evolution process. Each of these functions should
-- receive the type of operation as the first parameter and throw an error
-- in case an operator is unsupported:
--
-- `_cx OnePoint pop = myOnePoint pop`
-- `_cx NPoints  pop = error "NPoints crossover not implemented for this algorithm"`
data Interpreter a = Funs { _cx        :: Crossover a -> [a] -> Rnd a
                          , _mut       :: Mutation a -> a -> Rnd a
                          , _create    :: Rnd a
                          , _evaluate  :: a -> Rnd a
                          } 

-- | Reproduction algorithms
--
-- * Generational: the children replaces the parent population
-- * KeepBest: generational but keeping the best individual (either from parent or child population)
-- * ReplaceWorst: replaces the worst individuals with new random solutions or sampled solutions from the parent
-- * Merge: merge all the populations from different generation process
-- * Probabilistic: sample individuals from the combined parent+child populations using a Selection strategy
data Reproduction = Generational 
                  | KeepBest 
                  | ReplaceWorst 
                  | Merge
                  | Probabilistic Selection
                  | NonDominated
                  deriving (Show, Read)

-- | applies a reproduction algorithm on a list of populations. It returns a random population.
reproduce :: (Ord a, Solution a) => Reproduction -> [Population a] -> Rnd (Population a)
reproduce Generational [pop] = pure pop
reproduce Merge pops = pure $ V.concat pops
reproduce KeepBest pops = pure $ V.take npop $ sortVec $ V.concat pops
  where npop = length $ head pops
reproduce ReplaceWorst (parents:pops) 
  | nChildren == nParents = pure children
  | nChildren > nParents  = pure $ V.take nParents $ sortVec children
  | otherwise             = pure $ V.take (nParents - nChildren) parents <> children 
  where
    nParents  = V.length parents
    nChildren = V.length children
    children  = V.concat pops
reproduce (Probabilistic sel) (parents:pops) = V.fromList <$> replicateM nPop (select sel everyone)
  where
    everyone = V.concat (parents:pops)
    nPop     = V.length parents
reproduce NonDominated ps@(parents:pops) = do liftIO $ print $ map length fronts
                                              return vecNext
  where 
    n          = length parents
    everyone   = V.concat (parents:pops)
    fronts     = map (nubBy ((==) `on` (everyone V.!))) $ fastNondominatedSort everyone
    nComplete  = length . takeWhile (< n) . scanl1 (+) . map length $ fronts
    lastFront  = crowdingDistance everyone $ fronts !! nComplete
    selection  = if nComplete >= length fronts then concat fronts else concat $ lastFront : take nComplete fronts
    vecNext    = V.fromList $ map (everyone !) selection 

reproduce _ [] = error "reproduction must be applied to nonempty population"
reproduce r _  = error $ "unsupported reproduction: " <> show r

fastNondominatedSort :: Solution a => Population a -> [[Int]]
fastNondominatedSort pop = go [fstFront] dominationList nDoms
  where
    (fstFront, nDoms) = first M.keys                   -- returns only the keys of non-dominated and the map of the dominated
                      $ M.partition (==0) . M.fromList -- partition the list to those non-dominated and dominated
                      $ map (second length)            -- changes the list to hold the number of dominating individuals 
                      $ createListWith (≺)             -- creates a list of who dominates each individual
    dominationList    = M.fromList $ createListWith (≻) 
    (≻)               = (<) `on` (pop !) -- a ≻ b means a dominates b
    (≺)               = flip (≻)         -- a ≺ b means a is dominated by b
    createListWith op = map (\ix -> (ix, filter (op ix) [0 .. V.length pop - 1])) [0 .. V.length pop - 1] -- creates a list of which individuals ix dominates/is dominated by

    go :: [[Int]] -> M.IntMap [Int] -> M.IntMap Int -> [[Int]]
    go [] _ _      = error "first front is empty"
    go ([]:fs) _ _ = reverse fs          -- if we generated an empty front, we don't have any more individuals
    go (f:fs) s n  = go (f':f:fs) s n'   -- creates the next front f' based on last front f, the domination list s, and number of dominations n
      where
        (f', n') = first M.keys                 -- update front and number of dominations
                 $ M.partition (==0) 
                 $ M.unionsWith (\x _ -> x - 1) (n : map updateList f) -- subtract by 1 as many times as some index appear
        updateList = M.fromList . map (, 0) . (s M.!) -- creates a list of all indeces that should be updated

crowdingDistance :: Solution a => Population a -> [Int] -> [Int]
crowdingDistance pop front = map fst                   -- get the indices
                           $ sortBy (compare `on` (negate.snd)) -- sort by the crowding distance
                           $ zip front (A.elems dists) -- zip the front and its corresponding distances
  where
    n     = length front 
    fits  = map (_getFitness . (pop !)) front
    nobjs = length $ head fits 
    dists = runSTArray $ do
              distMut <- newArray (0, n-1) 0.0   -- mutable array of distances
              forM_ [0 .. nobjs-1] $ \j -> do    -- for each objective j
                let ixs = sortObjective j fits   -- sort front by objective
                    window = zip3 ixs (tail ixs) (tail . tail $ ixs) -- window with previous and next index
                forM_ window $ \((prev, mp), (i, mi), (next, mn)) -> do -- for each index inside a window
                    di <- readArray distMut i             -- get the current distance
                    writeArray distMut i (di + (mn - mp)) -- write the updated distance
                writeArray distMut (fst (head ixs)) (1/0) -- the first and last indices should contain infinity
                writeArray distMut (fst (last ixs)) (1/0)
              return distMut

    -- sort front by objectives
    sortObjective j fits = sortBy (compare `on` snd) 
                         $ zip [0..length fits-1] 
                         $ map (!!j) fits

  

sortVec :: (Ord a, Eq a) => Vector a -> Vector a
sortVec = V.fromList . sort . V.toList
{-# INLINE sortVec #-}

-- | Selection algorithms
--
-- * Tournament: tournament selection, samples n solutions and returns the best
-- * RouletteWheel: roulette wheel, calculates the probability of choosing an individual proportional to its fitness and sample an individual using this probability distribution
data Selection = Tournament Int 
               | CrowdingTournament Int
               | RouletteWheel 
               | FitShare Double Int -- tournament with fit share
               deriving (Show, Read)

-- Fit-share = fit_i * (1 + m/n_pop), m = sum [ sh(i,j) | j <- [0 .. npop-1], i/=j ]

-- | selects a random individual from the population using the `Selection` strategy.
-- TODO: some strategies sample a bunch of solutions in a single pass (SUS)
-- this would require a modification on how selection is applied.
select :: (Ord a, Solution a) => Selection -> Population a -> Rnd a
select _ pop | V.null pop = error "empty population in selection"
select (Tournament n) pop = do
  ixs <- replicateM n (randomInt (0, V.length pop - 1))
  pure $ minimumBy (compare `on` id) $ map (pop V.!) ixs
-- TODO: rewrite this ugly thing
select (FitShare sigma n) pop = do
  ixs <- replicateM n (randomInt (0, V.length pop - 1))
  let nPop = V.length pop
      distMtx = array ((0,0), (nPop-1, nPop-1))
                      [( (i,j), if i > j then distMtx A.! (j,i) else _distance (pop V.! i) (pop V.! j)) 
                      | i <- [0 .. nPop-1]
                      , j <- [0 .. nPop-1]
                      ]
      sumDist = array (0, nPop-1) [ (i, sum [distMtx A.! (i,j) | j <- [0..nPop-1], i/=j] / fromIntegral (nPop - 1)) | i <- [0..nPop-1]]
      minIx = minimumBy (compare `on` (\ix -> (head . _getFitness) (pop V.! ix) + sigma * (1 - sumDist A.! ix))) ixs
  pure $ pop V.! minIx
select (CrowdingTournament n) pop = do
  ixs <- replicateM n (randomInt (0, V.length pop - 1))
  let pop'  = V.fromList (map (pop V.!) ixs)
      (i:_) = crowdingDistance pop' $ head $ fastNondominatedSort pop'
  pure $ pop' V.! i
select RouletteWheel pop = do
  let fits     = V.map (head . _getFitness) pop -- defaults to the first objective 
      tot      = V.sum fits
      normFits = V.map (/tot) fits
      cumsum   = V.scanl1 (+) normFits
  p <- randomDbl
  let mix = V.findIndex (>= p) cumsum
  case mix of 
    Nothing -> pure $ pop `V.unsafeIndex` 0
    Just ix -> pure $ pop `V.unsafeIndex` ix

-- | Data family for Crossover and Mutation
--
-- as each algorithm/problem `t` has a different set of 
-- possible operators, the user must provide a data type
-- for Crossover and Mutation.
--
-- See test/Specs.hs for an example.
class (Show (Crossover t), Read (Crossover t), Show (Mutation t), Read (Mutation t)) => EvoClass t where 
  data Crossover t :: *
  data Mutation t :: *

-- | Predicate for population filter 
--
-- * Feasible: selects only the feasible solutions
-- * Infeasible: selects only the infeasible solutions
-- * All: selects the entire population
data Predicate = Feasible 
               | Infeasible
               | AllOf
               deriving (Show, Read)

filterPop :: Solution a => Predicate -> Population a -> Population a
filterPop Feasible   = V.filter _isFeasible
filterPop Infeasible = V.filter (not . _isFeasible)
filterPop AllOf      = id

-- | DSL of an evolutionary process
--
-- The evolutionary process is composed of a reproduction strategy between 
-- two evolutionary cycles applied to a filtered population.
data Evolution t = Reproduce Reproduction [EvoCycle t]
--  [(Predicate, EvoCycle t)]
--data Evolution t = Reproduce Reproduction Predicate (EvoCycle t) Predicate (EvoCycle t)
--  deriving (Show, Read)
deriving instance EvoClass t => Show (Evolution t)
deriving instance EvoClass t => Read (Evolution t)

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
--data EvoCycle t = Loop (Gen t)
                
data EvoCycle t = Op t :> EvoCycle t
                | Done
                | Parent
data Op t = Cross (Crossover t) Int Double Selection
          | Mutate (Mutation t) Double
          | With Predicate

deriving instance EvoClass t => Show (EvoCycle t)
deriving instance EvoClass t => Read (EvoCycle t)
deriving instance EvoClass t => Show (Op t)
deriving instance EvoClass t => Read (Op t)

infixr 5 :>

randomDbl :: StateT StdGen IO Double
randomDbl = state (randomR (0.0, 1.0))

randomInt :: (Int, Int) -> StateT StdGen IO Int
randomInt rng = state (randomR rng)

-- | Evaluates an evolution cycle.
-- Each step involves the immutable population as a context
-- and an individual as focus.
-- By the end of the cycle, a single individual will be created.
evalCycle :: (Solution a, NFData a)
          => EvoCycle a
          -> Population a
          -> a
          -> ReaderT (Interpreter a) (StateT StdGen IO) a
evalCycle Done pop p   = pure p
evalCycle Parent pop p = pure p
evalCycle (With pred :> evo) pop p = evalCycle evo (filterPop pred pop) p

evalCycle (Cross cx nParents pc sel :> evo) pop !p = do
  prob    <- lift randomDbl
  cxf     <- asks _cx
  --choose  <- asks _select
  eval    <- asks _evaluate 
  parents <- lift $ replicateM nParents (select sel pop)
  child   <- if prob < pc
              then lift (cxf cx parents >>= eval)
              else pure $ head parents
  evalCycle evo pop child

evalCycle (Mutate mut pm :> evo) pop !p = do
  prob  <- lift randomDbl
  mutf  <- asks _mut
  eval  <- asks _evaluate
  child <- if prob < pm
              then lift (mutf mut p >>= eval)
              else pure p
  evalCycle evo pop child

-- | Evaluates the evolution process in parallel
-- The evolution process generates two branches with each
-- branch generating a population. After that, it apples the
-- reproduction strategy to generate the next population.
-- Each individual of the new populations are generated in parallel 
-- using the evolution cycle. 
evalEvo :: (Solution a, NFData a)
        => Evolution a 
        -> Population a 
        -> [StdGen]
        -> ReaderT (Interpreter a) (StateT StdGen IO) (Population a, [StdGen])
evalEvo (Reproduce rep []) _ _ = error "empty evolution cycle"
evalEvo (Reproduce rep evos) pop gs = do
  config       <- ask
  (pops, gs')  <- runEvo evos gs
  pop' <- lift $ reproduce rep pops 
  return (pop', gs')
    where
      runEvo [] gs1               = pure ([], gs1)
      runEvo (evo:ps) gs1  = do
        config <- ask
        (pop', gs2) <- runPar evo config gs1 pop--(filterPop pred pop)
        (pops, gs3) <- runEvo ps gs2
        pure (pop':pops, gs3)

      runPar evo cfg g pop' = splitResponse
                            $ traverseConcurrently (ParN 0) (runCycle evo cfg pop') 
                            $ zip (V.toList pop') g

      splitResponse = fmap (go ([], [])) . liftIO 
        where
          go (accP, accG) []          = (V.fromList accP, accG)
          go (accP, accG) ((p,g):pgs) = go (p:accP, g:accG) pgs

      runCycle evo conf pop' (ix, g) = force <$> runStateT (runReaderT (evalCycle evo pop' ix) conf) g

-- | Generates the evolutionary process to be evaluated using `runEvolution`
genEvolution :: (Solution a, NFData a)
             => Int 
             -> Maybe Integer
             -> Int 
             -> (Population a -> IO ()) 
             -> Evolution a 
             -> ReaderT (Interpreter a) (StateT StdGen IO) ([Double], a, Population a)
genEvolution nGens maxTime nPop logger evo = do
  curTime   <- liftIO getPOSIXTime
  createSol <- asks _create
  eval      <- asks _evaluate 
  pop0      <- lift $ V.replicateM nPop (createSol >>= eval)
  g         <- lift get
  liftIO $ logger pop0
  go nGens curTime pop0 ([avgFit pop0], V.minimum pop0) $ splitGensWithIndex nPop g
    where
      maxTime' = case maxTime of
                      Just t  -> fromInteger $ t - 1
                      Nothing -> fromInteger 100000000
      go 0 curTime pop (!avgs, !best) gs = return (reverse avgs, best, pop)
      go n curTime pop (!avgs, !best) gs = do t <- liftIO getPOSIXTime
                                              liftIO $ print (length pop)
                                              if t - curTime < maxTime'
                                                then do (pop', gs') <- evalEvo evo pop gs
                                                        liftIO $ logger pop'
                                                        let avgs' = force $ avgFit pop' : avgs
                                                            best' = getBest best pop'
                                                        go (n-1) curTime pop' (avgs', best') gs'
                                                else go 0 curTime pop (avgs, best) gs

-- | Runs the evolutionary process 
runEvolution :: (Solution a, NFData a)
             => Int                        -- ^ number of generations
             -> Maybe Integer              -- ^ runtime limit
             -> Int                        -- ^ population size
             -> (Population a -> IO ())    -- ^ a logger function to run at every generation 
             -> Evolution a                -- ^ Evolutionary process 
             -> StdGen                     -- ^ random number generator
             -> Interpreter a              -- ^ Interpreter of evolutionary process 
             -> IO ([Double], a, Population a)           -- ^ returns the average fitness of every generation and the final champion
runEvolution nGens maxTime nPop logger evo g = flip evalStateT g . runReaderT (genEvolution nGens maxTime nPop logger evo)


splitGensWithIndex :: Int -> StdGen -> [StdGen]
splitGensWithIndex n = map fst . take n . iterate (\(g1,g2) -> split g2) . split 
{-# INLINE splitGensWithIndex #-}

-- average of the first fitness!!!
avgFit :: Solution a => Vector a -> Double
avgFit pop = getSum tot / fromIntegral (V.length pop)
  where
    tot = foldMap (Sum . head . _getFitness) pop 
{-# INLINE avgFit #-}

getBest :: Solution a => a -> Vector a -> a
getBest best pop
  | V.null feasible && _isFeasible best = best
  | V.null feasible                     = min best (V.minimum pop)
  | _isFeasible best                    = min best (V.minimum feasible)
  | otherwise                           = V.minimum feasible
  where
    feasible = V.filter _isFeasible pop
{-# INLINE getBest #-}
