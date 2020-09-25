# Quick Start

[ToC]

## Installation

**Homebrew(macOS)**

```shell
$ brew tap hamler-lang/hamler
$ brew install hamler
```

**Install from source code(macOS)**

1. Install Erlang

   ```shell
   $ brew install erlang@23
   ```

2. Install Stack

   Stack tutoriall https://docs.haskellstack.org/en/stable/install_and_upgrade/

3. Clone from the Git repo

   ```shell
   $ git clone https://github.com/hamler-lang/hamler.git
   ```

4. Install Hamler

   ```shell
   $ cd hamler
   $ make
   $ make install
   $ sudo mkdir -p /usr/local/lib/hamler/bin
   $ sudo cp repl/replsrv /usr/local/lib/hamler/bin/
   $ sudo cp -rv lib /usr/local/lib/hamler/
   $ sudo cp -rv ebin /usr/local/lib/hamler/
   ```

## Hamler Interpreter

```shell
$ hamler repl
> -- List, range and enums
> [1,2,3]
> [1..10]
> ['a'..'z']

> -- erlang style maps
> import Data.Map as Map
> -- New map
> m = #{"foo" => "bar", "bar" => "foo"}
> -- Match Map
> #{"foo" := a, "bar" := b} = m
> -- get, put
> Map.get "foo" m -- a = "bar"
> Map.get "bar" m -- b = "foo"
> m1 = Map.put "key" "val"
> -- keys, values
> keys = Map.keys m
> values = Map.values m
```

## Create A Project

```shell
$ mkdir demo-project
$ cd demo-project
$ hamler init
$ make
$ make run
```

## Module structure

A module is simply a bunch of related functions, types and type classes. This makes a program a collection of modules. This helps organize your code and makes reusing some of the code easier.

### Module header

**Module declaration**

This how we declare a new module and specify which of the functions or types are exported.

```haskell
module Hello (greet, farewell) where
{- the module name can be a word or words separated by '.';
  in this case it is just "Hello" -}

greet :: String -> String
greet n = "Hello " ++ n

farewell :: String -> String
farewell n = "Bye " ++ n
```

**Module import**

The syntax for import in Hamler is `import <module name>`. This has to be done before defining any functions. One module can import as many as modules you wish, but there could be ambiguity when there are two things with the same name.

```haskell
import Data.List       --Modules are imported using their full names
import Data.Maybe (isJust, isNothing)   -- We can choose which functions to import
import Data.Funtion as F
-- We can deal with ambiguity by adding an alias. This means we need to add "F." before every function that is exposed from Data.Function to specify that it is from this module
import Prelude hiding (fst)  -- The Prelude is imported by default. By hiding `fst`, we can define our own version.
```
## Hello, Hamler!

```haskell
module Main where

import Prelude

main = print "Hello, World!"
```
