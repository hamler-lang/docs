# Differences between Hamler and Haskell



**(Since the compiler is adapted from purescript's, we have inherited some differences between Purescript and Haskell)**



## Module Imports / Exports

Type classes in modules must be specifically imported using the class  keyword. 

```haskell
module B where

import A (class Fab)
```

[Ref](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md)



## Types

We use explicit `forall`.

```Haskell
id :: forall a. a -> a
```

### Float

There is no `Double` in Hamler, just `Float`

### List 

```haskell
{- hamler     <–>     haskell 
   []                   [] 
  [1,2,3]            [1,2,3] 
  [1|[2|[3|[]]]]     1:2:3:[]

Only in hamler [1,2,3|[2,3|[1,2]]]  --[1,2,3,2,3,1,2]
-}
```

### Records

```haskell
 data Person = Person { name :: String , age :: Integer } 
 p = Person {name = "alice", age = 30} --create a new record
 n = p.name        -- access record field
 a = p.age        
 np = p {age = 31} --new record
```



## Deriving

Same with PureScript, Hamler doesn't have `deriving` functionality when `declaring` data types. 

For example, the following code will not work

```Haskell
 data Foo = Foo Int String deriving (Eq, Ord)
```

[Ref](https://github.com/purescript/documentation/blob/master/language/Differences-from-Haskell.md)



## Orphan Instances

We do not allow orphan instance



## Operator

### No `,` operator

### No `$`  operator

We prefer not to add (>>) for now, because the key word >> is reserved for binary syntax. We choose not to put a lot of operators foe now. If there are needs, defining your own operator is allowed.

### Define Operators

In you can define a operator like this in Haskell, but this is not allowed.

```haskell
 f $ x = f x  -- Not Okay
 
 -- This is the way to do it
 apply f x = f
 x infixr 0 apply as $
```



## More

### No `undefined`



