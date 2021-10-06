module Control.Evolution
    ( 
    ) where

type Rnd a = a
type Population a = [a]
  {-
class Ord a => Solution a where
  _evaluate :: a -> Rnd a

class (Show c, Read c) => Crossover c where
  _cx :: Solution a => c -> [a] -> Rnd a
    
class (Show m, Read m) => Mutation m where
  _mutate :: Solution a => m -> a -> Rnd a

class (Show r, Read r) => Reproduction r where
  _reproduce :: Solution a => r -> Population a -> Population a -> Rnd (Population a)

class (Show s, Read s) => Selection s where
  _select :: Solution a => s -> Population a -> Rnd [a]

class SolutionBuilder sb where
  _createSolution :: Solution a => sb -> Rnd a
-}

data Reproduction = Generational | KeepBest | ReplaceWorst | Probabilistic Selection
data Selection = Tournament Int | RouletteWheel | SUS
data Crossover = OnePoint | NPoints Int
data Mutation = SwapTree | InsertVar
data Predicate = Feasible | Infeasible

-- | DSL of an evolutionary process
data Evolution = Reproduce Reproduction Evolution Evolution
               | Cross Crossover Selection Evolution
               | Mutate Mutation Evolution
               | Filter Predicate Evolution
               | End
