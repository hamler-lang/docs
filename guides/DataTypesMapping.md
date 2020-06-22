# Data Types Mapping

[ToC]

## Overview

The Hamler Data Types are mapping to Erlang Data types at compile-time. The following table shows the overview of the mappings:

| Hamler Data Type     | Erlang Data Type        | Mapping Description               |
| -------------------- | ----------------------- | --------------------------------- |
| Atom(Symbol in Ruby) | atom()                  |                                   |
| Bool                 | boolean()               | true -> true <br />false -> false |
| Char                 | char()                  |                                   |
| Integer(Int)         | integer()               | Integer type                      |
| Float(Double)        | float()                 | Float type                        |
| String               | string()                |                                   |
| Tuple                | tuple()                 |                                   |
| List                 | list()                  |                                   |
| Map                  | map()                   |                                   |
| Record               | map()                   |                                   |
| Binary               | binary() \| bitstring() |                                   |
| Port                 | port()                  | Erlang Port                       |
| Pid                  | pid()                   | Erlang Pid                        |
| Reference(Ref)       | reference()             | Erlang Reference                  |

## Atoms Mapping

**Atom** is a constant with name, which is also known as **Symbol** in other languages, such as Ruby.

**Atoms** in Hamler are imported from Erlang, defined as a primitive type, but have a different syntax from Erlang.

**Atoms** in Hamler start with a colon `:`

```hamler
:ok
:error
:the_1st_Atom
atom("node@127.0.0.1")
```
**Atoms** in Erlang:

```erlang
ok
error
the_1st_Atom
'node@127.0.0.1'
```

## Booleans Mapping

The syntax of **Booleans** in Hamler is the same as Erlang. The difference is that **Boolean** is a primitive type in Hamler, while `true` and `false` are atoms in Erlang.

**Booleans** in Hamler:

```hamler
true
false
```

**Booleans** in Erlang:

```hamler
true
false
```

## Chars Mapping

**Chars** in Hamler are also UTF-8 Unicode characters but with different syntax from Erlang.

**Chars** in Hamer are enclosed in single quotes, which are similar to most programming languages.

```hamler
'a'
'b'
'の'
```

**Chars** in Erlang are started with '$':

```hamler
$a
$b
$の
```

## Integers Mapping

**Integers** in Hamler are the same as Erlang, with unlimited size. Hamler defines `Int` as an alias for `Integer`.

Binary, octal, and hex integers in Hamler have a different syntax than Erlang.

**Integers** in Hamler:

```hamler
1, 2, -10
-- binary
0b101, 0B101
-- octal
0o1, 0O1, 0o52, 0O752
-- hex
0x1, 0X1, 0x2a, 0X2A, 0xff
```

**Integers** in Erlang:

```erlang
1, 2, -10
-- binary
2#101, 2#10
-- octal
8#1, 8#52, 8#752
-- hex
16#01, 16#2a, 16#2A, 16#ff
```

## Floats Mapping

**Floats** in Hamler are the same as Erlang, which is also known as **Doubles** in other languages.

**Floats** in Hamler and Erlang:

```erlang
1.0, 1.0e10
2.3
2.3e-3
0.0023
```

## Strings Mapping

**Strings** in both Hamler and Erlang are a list of UTF-8 characters.

**Strings** in Hamler and Erlang:

```erlang
"Hello, World!"
"你好，世界"
"ハロー、ワールド"
```

## Tuples Mapping

**Tuples** are defined as primitive data types in Hamler and compiled to Erlang tuples.

The differences from Erlang are as below:

- **Tuples** in Hamler are enclosed within parentheses, while **Tuples** in Erlang are enclosed in braces.

- The maximum length of **Tuples** in Hamler is 7, while no limit in Erlang.

**Tuples** in Hamler:

```hamler
(1, "a", true)
(1, "a")
(:error, "Reason")

-- fst, snd
fst (1, 'a') :: Integer -- 1
snd (1, 'a') :: Char    -- 'a'
```

**Tuples** in Erlang:

```erlang
{1, "a", true}
{1, "a"}

{error, "Reason"}
```

## Lists Mapping

**Lists** in Hamler are the same as Erlang, which is a list of UTF-8 Unicode characters.

**Lists** in Hamler:

```hamler
{-- List --}
[] -- empty list
[1,2,3] -- Integer list

[1|[2,3]] -- Cons
[1|[2|[3|[]]]] -- Cons

[x|_] = [1,2,3]  -- List pattern
[_|xs] = [1,2,3] -- List pattern
[x|xs] -- Cons
```

**Lists** in Erlang:
```erlang
%% List
[]      %% empty list
[1,2,3] %% Integer list

[1|[2,3]]      %% Cons
[1|[2|[3|[]]]] %% Cons

[H|_] = [1,2,3] %% List pattern
[_|T] = [1,2,3] %% List pattern
[H|T]           %% Cons
```

## Enum, Range

Hamler provides an Enum or Range syntax to help construct lists.

```hamler
[1..10] --[1,2,3,4,5,6,7,8,9,10]

[0, 5..100] -- [0,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100]

['a'..'z'] --"abcdefghijklmnopqrstuvwxyz"
```

## Maps Mapping

The syntax of **Maps** in Hamler is the same as Erlang. The differences are listed below:

- The keys of a Map should have the same type in Hamler;
- The values of a Map should have the same type in Hamler;
- The syntax `#{k := v}` is only used for map pattern matching.

**Maps** in Hamler:

```hamler
-- New map, values and keys must have the same type.
m = #{:foo => "foo", :bar => "bar"}

-- Pattern matching
#{:foo := a, :bar := b} = m
a :: String -- "foo"
b :: String -- "bar"
```

**Maps** in Erlang:
```erlang
%% Create a map
M = #{foo => "foo", bar => "bar"}

%% Pattern matching
#{foo := A, bar := B} = M.
A  %% "foo"
B  %% "bar"
```

## Records Mapping

**Records** in Hamler are mapping to Erlang **Maps** at compile-time.

**Records** in Hamler:

```hamler
type Person = {name :: String, age :: Integer}
john = {name = "John", age = 12}
```

Compiled to Erlang's Maps:
```
%% john
#{age => 12,name => "John"}
```

## Binaries Mapping

**Binaries** in Hamler are imported from Erlang. The **Bit Syntax Expressions** in Erlang are defined as below:

```erlang
<<>>
<<E1,...,En>>
          
Ei ::= Value 
     | Value : Size 
     | Value / TypeSpecifierList 
     | Value : Size / TypeSpecifierList
```

The **Bit Syntax** in Hamler is a bit different from Erlang, that '/' is replaced with ':':

```hamler
<<>>
<<E1,...,En>>
Ei ::= Value 
    |  Value : Size 
    |  Value : TypeSpecifierList 
    |  Value : Size : TypeSpecifierList
```

**Binaries** in Hamler:

```hamler
<<127,0,0,1>>
<<"ABC">>
<<1:16,2:4,3:4>>

-- Binary Pattern Match
<<x:3,y:5,z:8>> = <<1,0>>
<<bigI:16:Big-Unsigned-Integer>> = <<1,2>>
<<litI:16:Little-Signed-Integer>> = <<1,2>>
```

**Binaries** in Erlang:
```erlang
<<127,0,0,1>>
<<"ABC">>
<<1:16,2:4,3:4>>

-- Binary Pattern Match
<<X:3,Y:5,Z:8>> = <<1,0>>
<<BigI:16/big-unsigned-integer>> = <<1,2>>
<<LitI:16/little-signed-integer>> = <<1,2>>
```

## Ports Mapping

The **Port** data type in Hamler is foreign imported from Erlang.

## Pids Mapping

The **Pid** data type in Hamler is foreign imported from Erlang. **Pid** in Erlang is a unique identifier of the Erlang lightweight process.

## References Mapping

The **Reference** data type in Hamler is a foreign imported data type from Erlang.

Create a **Reference** in Hamler:
```hamler
import Data.Ref
makeRef
```

## User-defined data types Mapping

The constructors of Sum/Product algebraic data types are mapping to tuples in Erlang at compile-time.

Hamler:
```hamler
-- Sum datatype
data Color = Red | Green | Blue
Red   -- {'Red'}
Green -- {'Green'}
Blue  -- {'Blue'}

-- Product datatype
data Pair = Pair Integer Integer
Pair 3 4 -- {'Pair',3,4}

-- Maybe
data Maybe a = Just a | Nothing
Nothing -- {'Nothing'}
Just 5  -- {'Just',5}
```

Erlang:
```erlang
{'Red'}   %% Red
{'Green'} %% Green
{'Blue'}  %% Blue

{'Pair',3,4} %% Pair 3 4

{'Nothing'} %% Nothing
{'Just',5}  %% Just 5
```

