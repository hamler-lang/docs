# Foreign Function Interface

[Toc]

## Use Erlang Code From Hamler 

Since Hamler compiles to CoreErlang, it makes sense that there should be some feature that allows you to call Erlang code from Hamler. This is a brief introduction to using Erlang code from Hamler. The FFI is a powerful feature, so you have to know what you are doing. One thing to keep in mind that though you can type foreign functions, Hamler has no way to check whether you have given the right type signature, so be careful and remember to keep impure code wrapped in IO.

## Foreign Import

There are lot of examples in the [lib](https://github.com/hamler-lang/hamler/tree/master/lib) directory, from which you can easily discover that Erlang code and Hamler code are in the same directory with same filename, when defining a foreign function.

For example, in `Data.Map` we defined datatype `Map`, so first define `Map` as a foreign imported datatype and give it its kind.

```haskell
foreign import data Map :: Type -> Type -> Type
```

Then we can define some basic functions in erlang and import them to hamler.
In Map.erl we have:

```erlang
singleton(K, V) -> #{K => V}.
```

This is a function we defined to create a new map with one key to one value.

In hamler we can easily import this with:

```haskell
foreign import singleton :: forall k v. k -> v -> Map k v
```

The nice thing is that we can give `singleton` a type; however there is no way that Hamler can check this against the actual code you've written in Erlang. So when you are doing something not pure, remember to wrap the output with IO.

```haskell
foreign import readFile :: String -> IO String
```

## FFI Functions

We have also provided some nice functions allowing you to "directly" use an Erlang library:

```haskell
--if we want to use sin function from math module in Erlang
sin :: Float -> Float
sin = ffi1 :math :sin

-- foreign import ffi1 :: forall a b. Atom -> Atom -> a -> b
```

Where `ffi1` is a function takes in two atoms to locate the function in the Erlang library, and the number 1 means this function needs  argument. There are more in the lib.

