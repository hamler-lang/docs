# Main differences between Hamler and Purecript

Hamler added syntaxes which are more familiar to Erlang programmers.

### Binary

Hamler support Binary

```haskell
getA :: Binary -> Maybe Integer
getA << a:24:Big-Integer , b:4:Binary-Little , c:3:Binary >> = Just a
getA _                                                       = Nothing
```

### Map

Map in Hamler is more similar to Erlang

```haskell
getID :: Map String Integer -> Maybe Integer
getID #{ "Wang":= x, "Thomas" := y, "Leeming" := z } = Just x
getID _                                              = Nothing
```

### Atom

We have atoms and allow pattern match on atom. It is now one of the primitive type in Hamler.

```
:hello
:world
:hello_World123
```

### String

In PureScript, `String` is one of the primitive type. However, in Hamler String is `[Char]` a list of  `Char`s.

### Tuple

Tuple can be represented with `(a,b)`. Ant they can be pattern matched and are one of the primitive type.

### List

We have use List instead of Array.

```
[1..10]                                  --[1,2,3,4,5,6,7,8,9,10]
[(x,y)| x <- [1..10], y <- [1..10]]]     --[(1,1),(1,2)...
[a,b,c,d]
[a|[b,c,d]]
[x|xs]
[1|[2|[3|[4|[5,6,7]]]]]
```



## Other syntax changes

While declaring class and instances we use Haskell's syntax.

```haskell
 class Applicative m => Monad m

 instance Monad List where
```

