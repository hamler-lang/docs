# Basic Types, Functions and Operators

[ToC]

## Simple Types

Hamler is strongly typed, and has a powerful static type system. Let's start with some simple examples.

**Boolean**

```haskell
true :: Boolean
false :: Boolean
```

**Numbers**

Hamler has Integer and Float, and since they have different types so they can't be mixed.

```haskell
--Integer
1 :: Integer

--Float
0.1 :: Float
```

**Atoms**

Atom is probably more familiar to Erlang user. It is  a literal, a constant with a name starting with `:` .

```
:hello
:world
```

**Strings**

In Hamler `String` is just a list of `Char`

```haskell
"Hello World" :: String  -- ['H','e','l','l','o',',','W','o','r','l','d']
```

**Binaries**

This is the very unique datatype that exists in Erlang. `Binary` contains the same information as `ByteString` and if you are not very familiar with binaries, this [link](https://erlang.org/doc/man/binary.html) should be helpful for some intuition.

```haskell
<<1,2,3:8,4:16,5,"abcdefg">> :: Binary
```

## Operators

| Operator | Meaning                |      | Operator | Meaning               |
| -------- | ---------------------- | ---- | -------- | --------------------- |
| +        | Numeric addition       |      | ==       | Equality check        |
| -        | Numeric subtraction    |      | <        | Less than             |
| *        | Numeric multiplication |      | <=       | Less than or equal    |
| /        | Numeric division (div) |      | >        | Greater than          |
| %        | Remainder              |      | >=       | Greater than or equal |
| &&       | Boolean AND            |      | \|\|     | Boolean OR            |

## Functions

When we define a new function, we can give it a type signature. For example `double` is a function takes an `Integer` and gives an `Integer` doubled as output.

```haskell
double :: Integer -> Integer
double x = x * 2
```

**Lambda Expression**

There are also lambda expressions in Hamler, here is an example of how we rewrite double.

```haskell
double' :: Integer -> Integer
double' = \x -> 2 * x
```

It becomes really handy when we need to make an anonymous function.

**Currying**

```haskell
--Curry
--This is uncurried (+)
add :: (Integer, Integer) -> Integer
add (x, y) = x + y

--This is curried (+)
plus :: Integer -> Integer -> Integer
plus x y = x + y
```

**Partial Application**

```haskell
-- plus :: Integer -> (Integer -> Integer) This is one of the example of higher order functions
>:t plus 2
plus 2:: Integer -> Integer
>let plusTwo = plus2
>plusTwo 3
5
```

## Quantified Types

They are also known as **polymorhphic types**.

```haskell
> :type id
id :: forall a. a -> a
```

The key word `forall`indicates that `id` is univerally quantified, meaning that `id` can be applied to any type.

```haskell
> id 1
1
```

A more complicated example is `flip`. `flip` is also a [higher-order function](05_HigherOrderFunctionsAndRecursions.md), which will be explained in a later chapter.

```haskell
> :type flip
forall a b c. (a -> b -> c) - > b -> a -> c
```

## Notes On Indentations

Like all ML Language Family, Hamler is indentation sensitive. Any declaration in the same block should have the same level of indentation. In the case of a declaration spans more than one line, the other lines have to be intended past the first line.

```haskell
flip x f = f
x                   -- NOT OKAY, Hamler will see x as a seperate declaration

flip f x = f
    x               -- OKAY, but not recommended
```

**`Let` and `Where` Bindings**

Keywords such as Let and Where introduces a new block, where further indentation is needed.

```haskell
distance x y = sqrt z
  where
    z = x' + y'
    x' = x * x
    y' = y * y
```

## Type Synonym

Type synonym can be used to simplify a long type name to make code more readable.

```haskell
>:i String
type String = [Char]
```

Or you can define you own synonym name or a record.

## Records

```haskell
type Name = String

type Person =
  { firstName  :: Name
  , secondName :: Name
  }

{-
This is syntax sugared
"type Person = Record (FirstName  :: Name , SecondName :: Name)"
-}
```

Fields can be accessed by `.`

```haskell
leader :: Person
leader = {firstName = "John", lastName = "Portsman"}

>leader.firstName
"John"
```

This is how we update a record.

```haskell
newLeader :: Person
newLeader = leader{firstName = "James"}

>newLeader.firstName
"James"
```

```haskell
a = { name = "yang"
    , position =
         { streetNumber = 232
         , location = { x = 12.223
                      , y = 45.9 
                      }
         }
    }

b = a{position.location.x = 10.003}


> a.position.location.x
12.223
> b.position.location.x
10.003

```
