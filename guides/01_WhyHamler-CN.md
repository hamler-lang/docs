# 为什么要迎接Hamler?

[ToC]

近十年来，我们一直在开发基于Erlang/OTP的软件系统，尤其是我们的主打产品[EMQ X](https://github.com/emqx/emqx)--可扩展的开源MQTT代理。因此我们一直坚信Erlang是工程界的杰作。它具有惊人的并发性、原生支持分布式和容错性，是少数能够正确处理并发和软实时的通用语言平台之一。

但是从所有编写Erlang程序的经验来看，我们认为以下特性可以帮助Erlang程序员更好地适应即将到来的5G、物联网和边缘计算的浪潮，吸引更多的人使用BEAM的生态。

- 编译时类型检查和类型引用
- ADTs，函数复合，类型类。
- 可以促进社区繁荣的更加友好的语法
- 函子、应用函子和单子...)

现在所有的特性都已被Hamler编程语言变成了现实。

---

## 什么是Hamler

`Hamler` 是一种运行在Erlang虚拟机上的具有Haskell风格的函数式编程语言。

它是一种强类型语言，具有编译时类型检查和对并发与分布式的原生支持。这使得它非常适合构建下一代可扩展的、可靠的、实时的应用，特别是用于5G、物联网和边缘计算。

它太酷了，我们也别只顾吹牛了，行动起来吧。



---

## ＃＃先决条件

- 基本编程技能

- 最好是有一些Haskell或Erlang的使用经验（但这不是必需的）。



---

## Haskell风格

首先，Hamler是纯函数式的。它的语法和Haskell真的很相似，所以如果你熟悉Haskell语法，那么上手Hamler将是一件比较容易的事情。但是如果你不熟悉，跟着指南过一遍也足以让你熟悉基本的语法，更加适应函数式编程的风格。

这是一个在Hamler中实现merge sort的例子。你不明白是怎么回事是正常的，这个例子的目的只是让你对Hamler代码有一个整体感知。

```haskell
merge :: forall a. Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge [x|xs] [y|ys] = if x <= y
                      then  [x |merge xs [y|ys]]
                      else  [y |merge [x|xs] ys]

mergesort :: forall a. Ord a => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs = let (as, bs) = splitAt (length xs / 2) xs
               in merge (mergesort as) (mergesort bs)
```



---

##类型检查

Hamler是强类型化的，具有编译类型检查功能。所以在编译时，我们可以确保我们的程序是类型安全的，这可以帮助程序员避免愚蠢的错误，比如将一个字符串和一个整数相加。



---

## Erlang和并发

Erlang以其并发性著称。并发编程可以用来提高性能，获得可扩展性和容错性。这一切都依托于**BEAM**， 它是Erlang开放电信平台(OTP)核心的虚拟机。通过将Hamler编译成CoreErlang，我们可以很容易就利用Erlang虚拟机的强大功能。

```haskell
t :: IO ()
t = do
  pid0 <- selfPid
  pid100 <- seqio [spawn loop (State pid0) | x <- [1..1000]]
  seqio [send j (Next i) | (i,j) <- (zip pid100 [last pid100|init pid100]) ]
  send (head pid100) (Trans "great hamler! " 0)
  return ()

data Message = Next Pid
             | Trans String Integer

data State = State Pid

dealMessage :: State ->  Message -> IO State
dealMessage (State pid) (Next p) = return (State p)
dealMessage (State pid) (Trans str 11111) = return (State pid)
dealMessage (State pid) (Trans str i) =
  do send pid (Trans str (i+1))
     pid0 <- selfPid
     println (show pid0 <> " -> " <> show pid <> ": " <> str <> show i)
     return (State pid)

loop :: State -> IO ()
loop s = do
  x <- receive
  s1 <- dealMessage s x
  loop s1

```

