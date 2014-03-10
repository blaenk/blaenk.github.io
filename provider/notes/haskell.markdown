---
title: Haskell
published: March 5, 2014
excerpt: An exceedingly elegant functional programming language
comments: off
toc: left
---

I originally chose [Erlang] as the first functional programming language to attempt learning. However, the combination of different programming paradigm, obscure Prolog-like syntax, and unfamiliar concurrency paradigm made it particularly difficult for me to see the big picture of functional programming.

[Erlang]: /notes/erlang

I'm actually familiar with Haskell now. This very site is written in Haskell, and I have worked on a few other Haskell projects. That said, Haskell is also a programming language theory (PLT) playground. To that end, GHC contains various Haskell language extensions which I'm not entirely familiar with. This page of notes will therefore not necessarily cover the more basic aspects and instead will cover language extensions, popular libraries, idiomatic patterns, modules, parallelism and concurrency, and so on.

*[PLT]: Programming Language Theory
*[GHC]: Glasgow Haskell Compiler

* toc

# Parallelism

Amdahl's law places an upper bound on potential speedup through more processors. The expected speedup $S$ can be described as a function of the number of processors $N$ and the percentage of runtime that can be parallelized $P$. The implications are that the potential speedup increasingly becomes negligible with the increase in processors, but more importantly that most programs have a theoretical maximum amount of parallelism.

$$S(N) = \frac {1} {(1 - P) + \frac P N}$$

<img src="/images/notes/haskell/amdahls-law.png" class="center">

## ThreadScope

The [ThreadScope] tool helps visualize parallel execution, particularly for profiling. To use ThreadScope, the program should be compiled with the `-eventlog` GHC option and then run with the `+RTS -l` option to generate the eventlog. ThreadScope is then run on this event log:

[ThreadScope]: http://hackage.haskell.org/package/threadscope

``` bash
$ ghc -O2 program.hs -threaded -rtsopts -eventlog
$ ./program +RTS -N2 -l
$ threadscope program.eventlog
```

## Eval Monad

The `Eval` monad from `Control.Parallel.Strategies` expresses parallelism naturally. The `rpar` combinator expresses that the argument can be evaluated in parallel, and `rseq` forces sequential evaluation. Evaluation is to weak head normal form (WHNF), i.e. the outermost type constructor is evaluated. Notice that the monad is completely pure, with no need for the `IO` monad.

*[WHNF]: Weak Head Normal Form

``` haskell
data Eval a
instance Monad Eval

runEval :: Eval a -> a

rpar :: a -> Eval a
rseq :: a -> Eval a
```

For example, the following evaluates two thunks in parallel then waits for both to be evaluated before returning:

``` haskell
runEval $ do
  a <- rpar $ f x
  b <- rpar $ f y
  rseq a
  rseq b
  return (a, b)
```

Consider the following parallelized `map` implementation:

``` haskell
parMap :: (a -> b) -> [a] -> Eval [b]
parMap f [] = return []
parMap f (a:as) = do
  b  <- rpar (f a)
  bs <- parMap f as
  return (b:bs)

runEval $ parMap f xs
```

## Sparks

An argument to `rpar` is called a _spark_. The runtime collects sparks in a pool and distributes them to available processors using a technique known as _work stealing_. These sparks are to be _converted_, i.e. evaluated in parallel, though this may fail in a variety of ways. The `+RTS -s` option provides information about the sparks and what became of them during the execution of the program in the following form:

```
SPARKS: 100 (100 converted, 0 overflowed, 0 dud, 0 GC'd, 0 fizzled)
```

Term       Definition
-----      -----------
converted  successfully evaluated in parallel
overflowed didn't fit in the spark pool
dud        already evaluated, ignored
GC'd       unused, garbage collected
fizzled    evaluated some place else

## Deepseq

The `Control.Deepseq` module contains various utilities for forcing the evaluation of a thunk. It defines the `NFData`, i.e. _normal-form data_, type class. This type class has only one method, `rnf`, i.e. _reduce to normal-form_, which defines how the particular type may be evaluated to normal form, returning `()` afterward:

``` haskell
class NFData a where
  rnf :: a -> ()
  rnf a = a `seq` ()
```

The module also defines a `deepseq` function which performs a _deep_ evaluation to normal form, similar to `seq` which performs _shallow_ evaluation to WHNF. There's also a more convenient `force` function which evaluates the argument to normal form and then returns the result.

``` haskell
deepseq :: NFData a => a -> b -> b
deepseq a b = rnf a `seq` b

force :: NFData a => a -> a
force x = x `deepseq` x
```

## Evaluation Strategies

Evaluation strategies decouple algorithms from parallelism, allowing for parallelizing in different ways by substituting a different strategy. A `Strategy` is a function in `Eval` that returns its argument.

``` haskell
type Strategy a = a -> Eval a
```

A strategy would take a data structure as input which is then traversed and evaluated with parallelism and returns the original value. For example a strategy for evaluating a pair tuple could look like:

``` haskell
parPair :: Strategy (a, b)
parPair (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a', b')
```

This strategy could then be used either directly or using the `using` combinator, which reads as _use this expression by evaluating it with this strategy_:

``` haskell
using :: a -> Strategy a -> a
x `using` s = runEval (s x)

runEval . parPair $ (fib 35, fib 36)
(fib 35, fib 36) `using` parPair
```

### Parameterized Strategies

The `parPair` always evaluates the pair components in parallel and always to WHNF. A parameterized strategy could take as arguments strategies to apply to the type's components. The following `evalPair` function is so called because it no longer assume parallelism, instead delegating that decision to the passed in strategy.

``` haskell
evalPair :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')
```

It's then possible to define a `parPair` function in terms of `evalPair` that evaluates the pair's components in parallel. However, the `rpar` strategy only evaluates to WHNF, restricting the evaluation strategy. We could instead use `rdeepseq` --- a strategy to evaluate to normal form --- by wrapping `rdeepseq` with `rpar`. The `rparWith` combinator allows the wrapping of strategies in this manner:

``` haskell
rdeepseq :: NFData a => Strategy a
rdeepseq x = rseq (force x)

rparWith :: Strategy a -> Strategy a

parPair :: Strategy a -> Strategy b -> Strategy (a, b)
parPair sa sb = evalPair (rparWith sa) (rparWith sb)
```

This then allows us to use `parPair` to create a strategy that evaluates a pair tuple to normal form, which can then be used with the `using` combinator:

``` haskell
(fib 35, fib 36) `using` parPair rdeepseq rdeepseq
```

Sometimes it may be required to not evaluate certain type components, which can be accomplished using the `r0` strategy. For example, the following evaluates only the first components of a pair of pairs, i.e. `a` and `c`:

``` haskell
r0 :: Strategy a
r0 x = return x

evalPair (evalPair rpar r0) (evalPair rpar r0) :: Strategy ((a, b), (c, d))
```

The `parMap` function can be defined in terms of evaluation strategies. The `parList` function is a strategy that evaluates list elements in parallel. Defining `parList` can take the same approach as before: define a parameterized strategy on lists called `evalList` and then define a parameterized function `parList` that performs `evalList` in parallel. Both of these functions are already defined in `Control.Parallel.Strategies`:

``` haskell
evalList :: Strategy a -> Strategy [a]
evalList strat []     = return []
evalList strat (x:xs) = do
  x'  <- strat x
  xs' <- evalList strat xs
  return (x':xs')

parList :: Strategy a -> Strategy [a]
parList strat = evalList $ rparWith strat

parMap :: (a -> b) -> [a] -> [b]
parMap f xs = map f xs `using` parList rseq
```

