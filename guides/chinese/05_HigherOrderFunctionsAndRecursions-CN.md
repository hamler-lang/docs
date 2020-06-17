# 递归和高阶函数

[ToC]

---

## 介绍

递归是编程中的重要技术，尤其是在函数式编程中。

简单的例子。

```haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
```



---

## 对更复杂的数据类型进行递归。

列表的定义是递归的。所以，当我们为这样的数据类型定义一个函数时，很自然地就会递归地定义这个函数。

```haskell
length :: forall a . [a] -> Integer
length  []    = 0
length (x:xs) = 1 + length xs
```



---

## Map、过滤和折叠

`map`、`filter`和`foldr`是三个常用的操作列表的函数。`map`对`a`列表中的所有`a`应用`f`。`filter`只是根据`p`过滤列表。`foldr`通过将每个`:`替换为操作符或函数来破坏列表。

以下是定义。

```haskell
map :: forall a b. (a -> b) -> [a] -> [b]
map f  []      = []
map f (x:xs) = f x : xs

filter :: forall a. (a -> Boolean) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x then (x : filter p xs)
                         else filter p xs

foldr :: forall a b. (a -> b -> b) -> b -> [a] -> b --simplified defination see typeclass for more info
foldr f k []     = k
foldr f k (x:xs) = f x (foldr f k xs)
```

下面是一些使用上的例子。

```haskell
>map (+1) [1,2,3,4,5]
[2,3,4,5,6]

>filter (> 0) [-3,-2,-1,0,1,2,3]
[1,2,3]

>foldr (+) 0 [1,2,3,4,5]
15
```



---

## 列表解析

还有另一种定义Map和过滤器的方法，就是使用列表理解。

```haskell
map f xs    = [f x | x <- xs]
filter p xs = [x | x <- xs, p x]
```

有了列表理解，我们还可以做这样的事情。

```haskell
> [x + y | x <- [1..2], y<- [1..3]]
[2,4,5]

-- .. is syntax sugar for range
> [1..10]
[1,2,3,4,5,6,7,8,9,10]

```



---

## 高阶函数

像map、filter和foldr这样的函数也被称为高阶函数，因为它们以一个函数作为参数。高阶函数将一个函数作为参数或/和返回一个函数作为结果。

这里还有一些这样的函数的例子。

```haskell
apply :: forall a b. (a -> b) -> a -> b
apply f x = f x

compose :: forall a b. (b -> c) -> (a -> b) -> a -> c
compose g f x = g (f x)
```
