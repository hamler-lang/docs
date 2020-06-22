# Foreign Function Interface

[Toc]

## Use Erlang Code From Hamler

FFI is a powerful feature, so you have to know what you are doing.

There are lot of examples in the [lib](https://github.com/hamler-lang/hamler/tree/master/lib) directory, from which you can easily discover that Erlang code and Hamler code are in the same directory with same filename, when defining a foreign function.

For example, in `Data.Eq` we defined `Eq` instances for different types. These are done with Erlang Code.

In Eq.erl we have:

```erlang
eqCharImpl(C1, C2) -> C1 =:= C2.
```

This is a function we defined to compare two `Char`s.

In Eq we can easily import this with:

```haskell
instance Eq Char where
  eq = eqCharImpl

foreign import eqCharImpl :: Char -> Char -> Boolean
```

The nice thing is that we can give `eqCharImpl` a type, however there is no way that hamler can check this against the actual code you've written in Erlang. So when you are doing something not pure, remember to wrap the output with IO.

```haskell
foreign import readFile :: String -> IO String
```

We have also provide some nice functions allow you to "directly" use Erlang library:

```haskell
--if we want to use sin function from math module in Erlang
sin :: Float -> Float
sin = ffi1 :math :sin

-- foreign import ffi1 :: forall a b. Atom -> Atom -> a -> b
```

Where `ffi1` is a function takes in two atoms to locate the function in Erlang lib, and the number 1 means this function needs 1 argument.

