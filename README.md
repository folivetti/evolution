# evolution: evolutionary algorithms DSL with concurrency support

The `evolution` package supports the creation of evolutionary and bio-inspired algorithms
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

**Warning:** the main objective of this package is to create a minimal
DSL capable of describing (almost) every bio-inspired search algorithm. As such,
every new version can potentially break compatibility with the previous one.

## Example - Simple GA:

Let's suppose we are working with a binary representation and we want
to support a one-point crossover, single point mutation, multi-point mutation.

We must first create a data type with the chromossome and a fitness field and
add support to `Eq, Ord` classes by comparing the fitness:

```haskell
data Bin = Bin { _chromo :: [Bool]
               , _fit :: Double
               }
               
instance Eq Bin where
  t1 == t2 = _fit t1 == _fit t2
instance Ord Bin where
  t1 <= t2 = _fit t1 <= _fit t2
```

After that, we instantiate the `Solution` class to indicate
how to retrieve the fitness of the individual and check
whether it is feasible or not.

```haskell
instance Solution Bin where
  _getFitness = _fit 
  _isFeasible = const 0.0 -- always feasible
```

Next, we instantiate the `EvoClass` to create
the DSL for the crossover and mutation operators:

```haskell
instance EvoClass Bin where 
  data Crossover Bin = OnePoint deriving (Show, Read)
  data Mutation  Bin = SinglePoint | MultiPoints deriving (Show, Read)
```

Finally, we implement the required functions and create an `Interpreter`:

```haskell
cx OnePoint [p1,p2] = do
  let c1 = _chromo p1
      c2 = _chromo p2
  ix <- randomIndex (length c1)
  let c = take ix c1 <> drop ix c2
  pure $ Bin c 0.0 -- the individual will be evaluated later
  
mut SinglePoint (Bin c f) = do
  ix <- randomIndex (length c)
  let c' = swap ix c
  pure $ Bin c' 0.0
mut MultiPoints (Bin c f) = do
  c' <- traverse swapRnd c  
  pure $ Bin c' 0.0
    
randomIndividual n = do
    c <- take n <$> state randoms
    pure $ Bin c 0.0
    
-- how many True in the chromossome
fitness (Bin c _) = pure $ Bin c (length $ filter id c)

interpret :: Interpreter Test
interpret = Funs cx mut randomIndividual fitness
```

We can create a customized logger to store the information
at every generation.

```haskell
logger :: Population a -> IO ()
logger pop  = do
  let best  = getBestFitness pop
      worst = getBestFitness worst
      avg   = getAvgFitness
  print $ "The best fitness is: " <> show best
  print $ "The worst fitness is: " <> show worst
  print $ "The avg fitness is: " <> show avg
```

Now you can define your evolutionary algorithm using the DSL and
run it using `runEvolution`

```haskell
evo = Reproduce Generational 
        [With AllOf 
           :> Cross OnePoint 2 0.3 (Tournament 2) 
           :> Mutate SinglePoint 0.7 
           :> Done
        ]

-- run the evolution with 100 individuals for 500 generations
-- and random seed `g`
runEvolution 500 100 logger evo g interpret
```

We can, for example, create a an evolutionary algorithm that 
creates two child population: one with crossover and mutation
and another with only mutation, and then we reproduce using 
tournament selection.

```haskell
evo = Reproduce (Probabilistic (Tournament 2))
        [ With AllOf :> Cross OnePoint 2 0.3 (Tournament 2) :> Mutate SinglePoint 0.7 :> Done
        , With AllOf :> Mutate MultiPoints 0.9 :> Done
        ]
```

We can even parse the evolution process from a file. If we have a file "ga.evo" with the content

```haskell
Reproduce Generational 
    [With AllOf 
       :> Cross OnePoint 2 0.3 (Tournament 2) 
       :> Mutate SinglePoint 0.7 
       :> Done
    ]
```

We can automatically parse as:

```haskell
evo <- read <$> readFile "ga.evo"
```

The evolution DSL is described as a reproduction operator
followed by a list of evolution cycles.

```haskell
Reproduce <reproduction operator> [<evolution cycle>]
```

The evolution cycle can be either `Parent`, when you just want
to pass the current population, `<operator> :> <evolution cycle>`, 
when you want to chain two operators, and `Done`, marking the end of
the cycle.

The operator can be 

```haskell
Cross <crossover operator> <n-parents> <probability> <selection operator>
Mutate <mutation operator> <probability>
With <predicate>
```

The `With` operator is used to filter the population and can be `AllOf` (no filter),
`Feasible`, `Infeasible`.
