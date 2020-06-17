# Data Types Mapping

[ToC]

## Overview

The Hamler Data Types are mapping to Erlang Data types at compile-time as following tables show:

| Hamler Data Type  | Erlang Data Type               | Mapping Description               |
| ----------------- | ------------------------------ | --------------------------------- |
| Atom(Symbol)      | atom()                         |                                   |
| Bool              | boolean()                      | True -> true <br />False -> false |
| Char              | char() ??                      |                                   |
| Integer(Int)      | integer()                      | Integer type                      |
| Float(Double)     | float()                        | Float type                        |
| String            | "hello"                        | String is a list of character     |
| Tuple             | tuple()                        |                                   |
| List              | list()                         |                                   |
| Enum, Range       |                                |                                   |
| Map               |                                |                                   |
| Record            |                                |                                   |
| Binary/Bitstrings | binary() \| bitstring()        |                                   |
| Map(Dict)         | map()                          |                                   |
| Record            |                                |                                   |
| Fun               | fun()                          | Function                          |
| Port              | port()                         | Erlang Port                       |
| Pid               | pid()                          | Erlang Pid                        |
| Ref               | reference()                    | Erlang Reference                  |

## Atom Mapping

TODO:...

## Bool Mapping

TODO:...

## Char Mapping

TODO:...

## Integer(Int) Mapping

## Float(Double) Mapping

## String Mapping

## Tuple Mapping

examples: (:error, "Reason") -> {error, "Reason"}

## List Mapping

## Enum, Range

Enum mapping

## Map Mapping

## Record Mapping

Record is compiled to an Erlang Map

{x=1, hello="world"}

## Binary Mapping

## Fun Mapping

## Port Mapping

## Pid Mapping

## Reference(Ref) Mapping

## User-defined data Mapping

```
data Color = Red | Green | Blue
```
