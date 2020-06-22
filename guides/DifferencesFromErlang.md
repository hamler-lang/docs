# Differences from Erlang

## Overview

Hamler has a very different syntax from Erlang, although the Hamler source code compiled into CoreErlang. Erlang's syntax comes primarily from Prolog, but Hamler's comes from Haskell and Standard ML.

## Variables

Though variable names in both Hamler and Erlang are composed of letters, digits, and underscores, variables in Hamler begin with a lowercase letter, while Erlang variables begin with a capital letter.

Hamler:
```hamler
a, b, _a, _
the_1st_var
```

Erlang:
```erlang
A, B, _A, _
The_1st_var
```

## Delimiters

Unlike Erlang, Hamler language do not require `,`, `;`, and `.` delimiters.

## Comments

Single line comments in Hamler start with `--`:
```hamler
-- A single line comment
```

Comments in Erlang start with `%`:
```erlang
%% erlang comment
```

## Functions

### Function Defination and Application

Hamler:
```hamler
add x y = x + y
add 3 4 -- return 7

factorial 0 = 1
factorial n = n * factorial (n - 1)

factorial 10 -- return 3628800
```

Erlang:
```erlang
add(X, Y) -> X + Y.
add(3, 4). %% return 7

factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

factorial(10). %% return 3628800
```

### Anonymous Functions

Hamler:
```hamler
add = \a b -> a + b
curried_add = \a -> \b -> a + b

add 2 3
curried_add 2 3
```

Erlang:
```erlang
Add = fun(X, Y) -> X + Y end.
CurriedAdd = fun(X) -> fun(Y) -> X + Y end end.

Add(2,3).
(CurriedAdd(2))(3).
```

### Guards in Function

Hamler:
```hamler
f :: Integer -> String
f n | n > 0 = "Positive Integer"
    | n < 0 = "Negative Integer"
    | otherwise = "Zero"
```

Erlang:
```erlang
f(N) when N > 0 -> "Positive Integer";
f(N) when N < 0 -> "Negative Integer";
f(_) -> "Zero".
```

## Data Types

### Atoms
### Binaries
### Chars
### Integers
### Tuples

## List Comprehensions

List comprehensions in Erlang use `||` as a separator between expression and generators, but `|' is used in Hamler.

Hamler:
```hamler
[x*2 | x <- [1,2,3]]   -- [2,4,6]

-- multiple generators
[(x,y) | x <- [1,2,3], y <- [4,5]]

-- dependent generators
[(x,y) | x <- [1..3], y <- [x..3]]

-- Conditions
even i = 0 == i % 2
[x | x <- [1..10], even x]
```

Erlang:
```erlang
[X*2 || X <- [1,2,3]]. %% [2,4,6]

-- multiple generators
[{X, Y} || X <- [1,2,3], Y <- [4,5]].

-- dependent generators
[{X, Y} || X <- [1,2,3], Y <- lists:seq(X,3)].

-- Conditions
even(I) -> 0 == (I rem 2).
[X || X <- lists:seq(1, 10), even(X)].
```

## Expressions

### case .. of

The `case` expression is the same as Haskell.

Hamler:
```hamler
data RGB = Red | Green | Blue
color = Green
case color of
  Red -> "Red"
  Green -> "Green"
  Blue -> "Blue"
```

`case` expression in Erlang ends with an `end` keyword.

Erlang:
```erlang
Color = green.
case Color of
  red -> "Red";
  green -> "Green";
  blue -> "Blue"
end.
```

### if .. then .. else

Hamler:
```hamler
-- Every `then` must have a corresponding `else`
max x y = if x > y then x else y
```

Erlang:
```erlang
max(X, Y) -> if X > Y -> X; true -> Y end.
```

### let and where bindings

There are no `let` and `where` bindings in Erlang

Hamler:
```
let n = 1 + 2

z = let x = 3
        y = 2 * x
    in  x * y

-- or
z = x * y
    where
      x = 3
      y = 5
```

## Operators

### Arithmetic Operators

| Erlang   | Hamler           | Description      |
| -------- | ---------------- | ---------------- |
| rem      | %                | Remain           |
| div      | Not available    | Integer Division |


### Logical Operators

| Erlang   | Hamler           | Description      |
| -------- | ---------------- | ---------------- |
| and      | Not available    |                  |
| andalso  | &&               | And also         |
| or       | Not available    |                  |
| orelse   | \|\|             | Or else          |

### Relational Operators

| Erlang   | Hamler           | Description      |
| -------- | ---------------- | ---------------- |
| =:=      | Not available    | Exactly equal    |
| =/=      | Not available    | Exactly not equal|
| =<       | <=               | Less equal       |

## Modules

The module system of Hamler is the same as Haskell, which is more advanced than Erlang.

