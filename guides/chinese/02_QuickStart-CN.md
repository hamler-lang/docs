
# 快速开始

[ToC]

---

## 安装

**Homebrew(macOS)**。

```shell
$ brew tap hamler-lang/hamler
$ brew install hamler
```

**从源代码安装（macOS）**。

1. 安装Erlang

   ```shell
   $brew install erlang@22
   ```

2. 安装stack

   Stack tutoriall https://docs.haskellstack.org/en/stable/install_and_upgrade/

3. 从github克隆

   ```shell
   $ git clone https://github.com/hamler-lang/hamler.git
   ```

4. 安装hamler

   ```shell
   $ cd hamler
   $ make
   $ make install
   $ sudo mkdir -p /usr/local/lib/hamler/bin
   $ sudo cp repl/replsrv /usr/local/lib/hamler/bin/
   $ sudo cp -rv lib /usr/local/lib/hamler/
   $ sudo cp -rv ebin /usr/local/lib/hamler/
   ```



---

## Hamler解析器

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



---

## 创建一个项目

```shell
$ mkdir demo-project
$ cd demo-project
$ hamler init
$ make
$ make run
```



---

## 模块结构

一个模块只是一堆相关的函数、类型和类型类定义的集合。这使得一个程序成为一些模块的组合。这有助于组织你的代码，并使一些代码的重用更容易。

### 模块头

**模块声明**

这就是我们如何声明一个新的模块，并指定哪些函数或类型被导出。

```haskell
module Hello (greet, farewell) where
{-the module name can be a word or words seperated by '.',
  in this case i it is just "Hello"-}

greet :: String -> String
greet n = "Hello " ++ n

farewell :: String -> String
farewell n = "Bye " ++ n
```

**模块导入**

Hamler中导入的语法是`import <module name>`。这必须在定义任何函数之前完成。如果你愿意的话，一个模块可以导入任意数量个模块，但是当有两个模块的名字相同时，可能会有歧义。

```haskell
import Data.List       --Modules are imported using their full names
import Data.Maybe (isJust, isNothing)   -- We can choose which functions to import
import Data.Funtion as F     
-- We can deal with ambiguity by adding an alias. This means we need to add "F." before every functions that are exposed from Data.Function to specify that it is from this module
import Prelude hiding (fst)  -- The Prelude is imported by default. By hiding `fst`, we can define our own version.
```



---

## Hello, Hamler !

```Haskell
module Main where

import System.IO

main = print "Hello, World!"
```
