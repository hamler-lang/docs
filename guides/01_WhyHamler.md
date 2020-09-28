# Why Hamler?

[ToC]

For almost a decade, we have been developing software systems based on Erlang/OTP, especially our main product [EMQ X](https://github.com/emqx/emqx) - the scalable open-source MQTT broker. So, we have always believed that Erlang is a masterpiece of engineering. With amazing concurrency, distribution and fault tolerance, it is one of the few general-purpose language platforms able to properly handle concurrency and soft realtime.

However, from all the experience writing Erlang, we believe that the following features can help Erlang programmer better adapt to the coming wave of 5G, IoT and edge-programming and attract more people for using BEAM.

- Compile-time type checking and type reference
- ADTs, Function Composition, Type Classes
- More friendly syntax for prosperous communities
- Functor, Applicative and Monad...:)

Now all the features are avaliable in the Hamler programming language.

## What's Hamler

`Hamler`  A Haskell-style functional programming language running on Erlang VM.

It is a strongly-typed language with compile-time typechecking and built-in support for concurrency and distribution. This makes it perfect for building the next generation of scalable, reliable, realtime applications, especially for 5G, IoT and edge computing.

Cool, let's quit the bragging and kick off.

## Prerequisites

- Basic Programming Skills

- It will be good to have some experience with Haskell or Erlang (but this is not essential)

## Haskell Style

First of all, Hamler is purely functional. It has really similar syntax to Haskell, so if you are familiar with Haskell it should not be a problem. However, if you are not, the guide should be able to walk through the basic syntax and make you more comfortable with programming functionally.

This is an example of implementing merge sort in Hamler. It is normal that you don't understand what is going on, the purpose of the example is to just let you get a gist of what will the code look like.

```haskell
merge :: forall a. Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge [x|xs] [y|ys] = if x <= y
                      then  [x |merge xs [y|ys]]
                      else  [y |merge [x|xs] ys]

mergesort :: forall a. Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = splitAt (length xs / 2) xs
               in merge (mergesort as) (mergesort bs)
```

## Type Checking

Hamler is strongly typed with compile type checking. So at compile time we can ensure that our program is type-safe, and this can help programmers to avoid silly mistakes like adding a string and an integer.

## Erlang and Concurrency

Erlang is famous for its concurrency. Concurrent programming can be used to improve performance, gain scalability and fault-tolerance. **BEAM** is the virtual machine at the core of the Erlang Open Telecom Platform (OTP) which enables it to happen. By compiling Hamler to CoreErlang, we can essentially take advantage of Erlang VM.

```haskell
import Prelude
import Data.List as L

start :: IO ()
start = do
  pid0 <- getSelf
  pids <- seqio [spawn $ loop (State pid0) | x <- [1..1000]]
  seqio [send j (Next i) | (i,j) <- (zip pids [last pids|L.init pids]) ]
  send (head pids) (Trans "great hamler! " 0)
  return ()

data Message = Next Pid
             | Trans String Integer

data State = State Pid

handleMsg :: State ->  Message -> IO State
handleMsg (State pid) (Next p) = return (State p)
handleMsg (State pid) (Trans str 1111) = return (State pid)
handleMsg (State pid) (Trans str i) =
  do send pid (Trans str (i+1))
     pid0 <- getSelf
     println (show pid0 <> " -> " <> show pid <> ": " <> str <> show i)
     return (State pid)

loop :: State -> IO ()
loop s = receive v -> handleMsg s v >>= loop
```
