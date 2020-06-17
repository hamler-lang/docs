# Hamler Cheatsheet

Hamler is a haskell-style functional programming language running on Erlang VM.

![hamler-logo](https://www.hamler-lang.org/images/hamler-logo.png)

[ToC]

## Hello world

```haskell
module Main where

import Prelude

main :: IO ()
main = println "Hello, world!"
```

## Hamler REPL

```shell
hamler repl
> import Data.Map as Map
> Map.empty
#{}
>
```

## Comments

```haskell
-- A single line comment
{- Multi-line comments -}
```

## Values, Types and Variables

```haskell
-- Types, Values
true :: Boolean
false :: Boolean
2 :: Integer
1.0 :: Float
'a' :: Char
"hello" :: String

-- Variables Binding
i = 1
(a, b) = (1, 2)
```

## Basic Types

| Type              | Values        | Description                   |
| ----------------- | ------------- | ----------------------------- |
| Atom              | :ok, :error   | Erlang Atom type              |
| Boolean(Bool)     | true \| false | Boolean type                  |
| Char              | 'c', 'x'      | UTF-8 character                |
| String            | "hello"       | List of UTF-8 character        |
| Integer(Int)      | 1, 2, -10     | Integer type                  |
| Float(Double)     | 3.14          | Float type                    |
| List              |               |                               |
| Tuple             | (1, true)     |                               |
| Map               | #{"k" => "v"} | Erlang Map                    |
| Record            |               |                               |
| Binary            | <<1,2,3>>     | Erlang Binary/Bitstring       |
| Pid               |               | Erlang Pid                    |
| Port              |               | Erlang Port                   |
| Reference(Ref)    |               | Erlang Reference              |

### Booleans

```haskell
true || false
```

### Integers and Floats

Two types of numeric literals: Integers and Floats.

```haskell
-- Integer
1, 2, -10

-- binary, octal, and hex literals
0x1, 0X1, 0x2a, 0X2A
0o1, 0O1, 0o52, 0O52
0b10, 0B10

-- floats
1.0, 1e10
2.3
2.3e-3
0.0023
```

### Atoms

In hamler, atoms are started with ':', and mapping to Erlang atoms.

```hamler
:atom, :ok, :error
```

### Chars

UTF-8 unicode characters.

```haskell
'a', 'b', 'の'
```

### Strings

In hamler, string is a list of UTF-8 unicode characters.

```haskell
"Hello, World!"
"你好，世界"
"ハロー、ワールド"

-- Escape Codes
"\r\n ..."

-- ++ to concat strings
"Hello " ++ " World"

-- TODO
printf "foo %s" "bar"
```

### Tuples

A tuple is a sequence of values of different types. In hamler. the maximum length of the tuple is 7.

```haskell
(1, "a", true)
(1, "a")

-- fst, snd
fst (1, 'a') :: Integer -- 1
snd (1, 'a') :: Char    -- 'a'
```

### Lists

A list is sequence of values of the same type:

```erlang
{-- List --}
[] -- empty list
[1,2,3] -- Integer list

[1|[2,3]] -- Cons
[1|[2|[3|[]]]] -- Cons

[x|_] = [1,2,3] -- List pattern
[_|xs] = [1,2,3] -- List pattern
[x|xs] -- Cons
```

### Maps

Erlang style maps are imported to hamler language:

```haskell
-- New map, values and keys must have the same type.
m = #{:foo => "foo", :bar => "bar"}

-- Pattern matching
#{:foo := a, :bar := b} = m
a :: String -- "foo"
b :: String -- "bar"

-- get, put
import Data.Map as Map
m1 = Map.put :key "val"
Map.get :foo m :: String -- "foo"
Map.get :key m1 :: String --"val"

-- keys, values
keys   = Map.keys   m -- [String]
values = Map.values m -- [String]
```

### Records

```haskell
-- declare a Person record
type Person = {name :: String, age :: Integer}

-- create a Person record
p = {name = "John", age = 12}

-- update a Person record
p1 = p {name = "Miles", age = 20}

-- accessors
name = p1.name :: String
age = p1.age   :: Integer
```

## Binaries

Binaries are imported from Erlang, which are raw byte strings.

```erlang
-- Construct Binary
<<127,0,0,1>>
<<"ABC">>
<<1:16,2:4,3:4>>

-- Binary Pattern Match
<<x:3,y:5,z:8>> = <<1,0>>
<<bigI:16:Big-Unsigned-Integer>> = <<1,2>>
<<litI:16:Little-Signed-Integer>> = <<1,2>>
```

### Ports

TODO: Erlang port identifier identifies an Erlang port.

### PIDs

TODO: Erlang process identifier, pid, identifies a process.

### References

TODO: Erlang reference

## User-defined Types

Hamler supports algebraic data type (ADT):

```haskell
-- type synonym
type Name = String
"Miles" :: Name
"Miles" :: String

newtype UInt8 = UInt8 Integer
1 :: Integer
UInt8 1 :: UInt8

-- sum datatype
data Color = Red | Green | Blue
Blue :: Color

-- product datatype
data Pair = Pair Integer Integer
Pair 3 4 :: Pair

-- record product datatype
data Person = Person {
  name :: String
  age :: Integer
  address :: String
}
Person {name = "Miles", age = 50, address = "NY"} :: Person

-- generic datatype (maybe for example)
data Maybe a = Just a | None
data Result val err = Ok val | Error err

-- recursive datatype
data Tree = Leaf Integer | Node Tree Tree
```

## Bindings

### let

```haskell
let n = 1 + 2
let (a, b) = (1, 2)
```

### let .. in ..

```haskell
z = let x = 3
        y = 2 * x
    in  x * y
```

### where

```haskell
z = x * y
    where
      x = 3
      y = 5
```

## Functions

A function is a mapping from values of one type to values of another type.

### Function Definition and Application

```haskell
add :: Integer -> Integer -> Integer
add x y = x + y

add 1 2
```

### Polymorphic Functions

```haskell
length :: forall a. [a] -> Integer

-- example
nats25 :: [Integer]
nats25 = [0..25]

letters :: [Char]
letters = ['a'..'z']

n1 = length nats25   -- 26
n2 == length letters -- 26

zip :: forall a b. [a] -> [b] -> [(a,b)]
ordL = zip nats letters

-- [(0,'a'),(1,'b'),(2,'c'),(3,'d'),(4,'e'),(5,'f'),(6,'g'),(7,'h'),(8,'i'),(9,'j'),(10,'k'),(11,'l'),(12,'m'),(13,'n'),(14,'o'),(15,'p'),(16,'q'),(17,'r'),(18,'s'),(19,'t'),(20,'u'),(21,'v'),(22,'w'),(23,'x'),(24,'y'),(25,'z')]
```

### Currying

```haskell
-- uncurried
plus :: (Integer, Integer) -> Integer
plus (x, y) = x + y

-- sum is the curried version of plus
sum :: Integer -> Integer -> Integer
sum x y = x + y
```

### Partial application

```haskell
sum 1  2      :: Integer
sum 1 (2 + 3) :: Integer

add2 = sum 1  :: Integer -> Integer -- partially applied
x    = add2 3 :: Integer        -- x = 5
```

### High-Order Functions

```haskell
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x
```

### Recursive Function

```haskell
fact n = if n == 0 then 1 else n * fact (n - 1)

length []       = 0
length [x|xs] = length xs + 1
```

### Lambda (Anonymous Function)

```haskell
multBy :: Integer -> Integer -> Integer
multBy n = \m -> m * n

mean :: Integer -> Integer -> Integer
mean = \x y -> (x + y) `div` 2  -- f = (\x -> \y -> (x + y) `div` 2)
```

### Pattern Matching

```haskell
(x, y) = (1, 2)

-- function declartion via pattern matching
allEmpty [] = True
allEmpty _ = False

-- pattern matching stops when it finds the first match
```

### Guarded Equations

```haskell
abs n | n > 0     = n
      | otherwise = -n
```

## Expressions

### if .. then .. else

```haskell
-- Every `then` must have a corresponding `else`
abs x = if x > 0 then x else -x

-- Indentations
sign x =
  if x > 0
    then print "pos"
    else if x < 0
      then print "neg"
      else print "zero"
```

### case .. of

```haskell
rgb = Red

f = case rgb of
      Red   -> "red"
      Green -> "green"
      Blue  -> "blue"
-- f has value "red"

g = case rgb of Green -> "red"; _ -> "not red"
-- g has valur "not red"

```

### List comprehensions

A list comprehension consists of four types of elements: *generators*, *guards*, *local bindings*, and *targets*.

```haskell
-- examples
[x*2 | x <- [1,2,3]]   -- [2,4,6]

[x * x | x <- [1..10]] -- [1,4,9,16,25,36,49,64,81,100]

-- multiple generators
[(x,y) | x <- [1,2,3], y <- [4,5]]

-- dependent generators
[(x,y) | x <- [1..3], y <- [x..3]]

-- Conditions
even i = 0 == i % 2
[x | x <- [1..10], even x]
```

### Enumerations, Range

```haskell
[1..10]
--[1,2,3,4,5,6,7,8,9,10]

[0, 5..100]
-- [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100]

['a'..'z']
--"abcdefghijklmnopqrstuvwxyz"
```

## Operators

### Arithmetic Operators

| Operator | Name             | Example |
| -------- | ---------------- | ------- |
| +        | Add              |         |
| -        | Subtract         |         |
| *        | Multiply         |         |
| /        | Divide           |         |
| %        | Remain           |         |
| div      | Integer Division | div 7 3 |
| rem      | Remain           | rem 7 3 |
|          |                  |         |

### Logical Operators/Functions

| Operator | Name |
| -------- | ---- |
| &&       | And  |
| \|\|     | Or   |
| not      | Not  |
|          |      |

### Relational Operators

| Operator | Name        |
| -------- | ----------- |
| ==       | Equal       |
| /=       | Not Equal   |
| <        | Less        |
| >        | Great       |
| <=       | Less Equal  |
| >=       | Great Equal |

### Bit Operators

| BitOp | Name            |
| ----- | --------------- |
| band  | Bit and         |
| bor   | Bit or          |
| bnot  | Bit not         |
| bxor  | Bit xor         |
| bsl   | Bit shift left  |
| bsr   | Bit shift right |

## Modules

A module is a compilation unit which exports types, functions, type classes and other modules.

### Module Declaration and Export

The name of a module must start with a capital letter.

```haskell
-- Declare a module and export all the types and functions
module MyMod where

-- Declare a module and export some types or functions
module MyMod (Maybe(..), add) where

data Maybe a = Just a | Nothing

add :: Integer -> Integer -> Integer
add x y = x + y
```

### Main

```haskell
-- Main
module Main where

import Prelude

main = println "Hello World"
```

### Import

```haskell
import Data.List
import Data.Map (keys, values)

nth 1 [1..10]            -- 1
keys #{"key" => "val"}   -- ["key"]
values #{"key" => "val"} -- ["val"]

-- Qualified Imports
import Data.Set as Set
import Data.Map as Map

Map.get "foo" #{"foo" => "bar"}
```

## Type classes

TODO:...

## Functor, Applicative and Monad

TODO:...

## Reserved Words

```haskell
as case class data do else false forall foreign hiding import if in infix infixl infixr instance kind let module newtype of then true type where
```

