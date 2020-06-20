# Data Types Mapping

[ToC]

## Overview

The Hamler Data Types are mapping to Erlang Data types at compile-time as following tables show:

| Hamler Data Type  | Erlang Data Type               | Mapping Description               |
| ----------------- | ------------------------------ | --------------------------------- |
| Atom(Symbol)      | atom()                         |                                   |
| Bool              | boolean()                      | true -> true <br />false -> false |
| Char              | char()                         |                                   |
| Integer(Int)      | integer()                      | Integer type                      |
| Float(Double)     | float()                        | Float type                        |
| String            | string()                       |                                   |
| Tuple             | tuple()                        |                                   |
| List              | list()                         |                                   |
| Map               |                                |                                   |
| Record            |                                |                                   |
| Binary            | binary() \| bitstring()        |                                   |
| Port              | port()                         | Erlang Port                       |
| Pid               | pid()                          | Erlang Pid                        |
| Reference(Ref)    | reference()                    | Erlang Reference                  |

## Atoms Mapping

**Atom** is a constant with name, which is also known as **Symbol** in other languages, such as Ruby.

Hamler's **Atom** data type is imported from Erlang, but a different syntax is used.

Atoms in Hamler start with a colon `:`.

Hamler:
```hamler
:ok
:error
:the_1st_Atom
atom("node@127.0.0.1")
```
Erlang:
```erlang
ok
error
the_1st_Atom
'node@127.0.0.1'
```

## Booleans Mapping

The syntax of **Booleans** in Hamler are the same to Erlang. The difference is that **Boolean** is a primitive type in Hamler, while `ture` and `false` are atoms in Erlang.

Hamler:
```hamler
true
false
```

Erlang:
```hamler
true
false
```

## Chars Mapping

**Chars** in hamler are also utf-8 unicode characters but with different syntax from erlang.

**Chars** in hamer are enclosed in single quotes, which are similar to most programming languages.

```hamler
'a'
'b'
'の'
```

**Chars** in erlang is started with '$':
```hamler
$a
$b
$の
```

## Integers Mapping

**Integers** in hamler are the same to erlang, with unlimited size. Hamler defines `Int` as an alias for `Integer`.

Binary, octal and hex integers in hamler have a different syntax than erlang.

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

Erlang:
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

**Floats** in hamler are the same to erlang, which are also defined as **Doubles** in other languages.

**Floats** in Hamler and Erlang:
```erlang
1.0, 1.0e10
2.3
2.3e-3
0.0023
```

## Strings Mapping

**Strings** in both Hamler and Erlang are list of UTF-8 characters.

**Strings** in Hamler and Erlang:
```erlang
"Hello, World!"
"你好，世界"
"ハロー、ワールド"
```

## Tuples Mapping

**Tuples** are defined as primitive data types in Hamler, and compiled to Erlang tuples.

The differences from Erlang are as below:

1. **Tuples** in Hamler are enclosed within parentheses, while **Tuples** in Erlang are enclosed in braces.
2. The maximum length of **Tuples** in hamler is 7, while no limit in erlang.

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

**Lists** in Hamler are the same to Erlang.

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

The syntax of **Maps** in Hamler is same to Erlang. The differences are listed below:

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

**Records** in Hamler are compiled to Erlang maps.

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
Ei = Value |
     Value:Size |
     Value/TypeSpecifierList |
     Value:Size/TypeSpecifierList
```

The **Bit Syntax** in Hamler is a bit different from Erlang, that '/' is replaced with ':':

```hamler
<<>>
<<E1,...,En>>
Ei = Value |
     Value:Size |
     Value:TypeSpecifierList |
     Value:Size:TypeSpecifierList
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

## Port Mapping

Intro:...

Hamler:
```hamler
```

Erlang:
```erlang
```

## Pid Mapping

Intro:...

Hamler:
```hamler
```

Erlang:
```erlang
```

## Reference(Ref) Mapping

Intro:...

Hamler:
```hamler
```

Erlang:
```erlang
```

## User-defined data Mapping

### Sum datatypes

```hamler
data Color = Red | Green | Blue
```

### Product datatypes

```
data Pair = Pair Integer Integer
```

### Record product datatypes

