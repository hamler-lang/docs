# Recursions and Higher Order Functions

[ToC]

## Introduction

Resursion is an important technique in programming, especially in functional programming.

Simple examples:

```haskell
fact :: Integer -> Integer
fact 0 = 1
fact n = n * fact (n - 1)

> fact 10
3628800
> fact 5
120

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

> fib 10
89
> fib 5
8
```

## Recursions on more complicated datatypes

Definition of the datatype list is recursive. So,when we define a function for such datatypes, it comes naturally to define the function recursively.

```haskell
length' :: forall a . [a] -> Integer
length'  []    = 0
length' [x|xs] = 1 + length xs

> length' "hamler"
6
```

## Map, filter and fold

`map`, `filter` and `foldr` are three commonly used functions to manipulate a list. `map` applys `f` on all `a`s in a list of `a`. `filter` just filters the list according to `p`. `foldr` destructs the list by replacing every `:` with a operator/or function.

Here are the definitions.

```haskell
map' :: forall a b. (a -> b) -> [a] -> [b]
map' f  []      = []
map' f [x|xs] = [f x | map' f xs]

> map' (\x -> x +1 ) [1..10]
[2,3,4,5,6,7,8,9,10,11]

filter' :: forall a. (a -> Boolean) -> [a] -> [a]
filter' p []     = []
filter' p [x|xs] = if p x
                   then [x | filter p xs]
                   else filter p xs

> filter' (\x -> x > 5) [1..10]
[6,7,8,9,10]

foldl' :: forall a b. (b -> a -> b) -> b -> [a] -> b
foldl' f k []     = k
foldl' f k [x|xs] = foldl' f (f k x) xs

> foldl' (+) 0 [1..10]
55
```

## List Comprehensions

There is an alternative way to define map and filter, which is to use list comprehension.

```haskell
map f xs    = [f x | x <- xs]
filter p xs = [x | x <- xs, p x]
```

With list comprehension we can also do things like:

```haskell
> [x + y | x <- [1..2], y<- [1..3]]
[2,3,4,3,4,5]

-- .. is syntax sugar for range
> [1..10]
[1,2,3,4,5,6,7,8,9,10]

> ['a' .. 'z']
"abcdefghijklmnopqrstuvwxyz"
```

## Higher Order Functions

Functions like map, filter and foldr are also called higher order functions, becuase they take a function as their argument. A higher order function takes a function as its argument or/and returns a function as it's result.

Here are some more examples of such functions.

```haskell
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

compose :: forall a b. (b -> c) -> (a -> b) -> a -> c
compose g f x = g (f x)
```
