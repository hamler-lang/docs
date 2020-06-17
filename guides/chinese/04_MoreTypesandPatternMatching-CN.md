# 类型和模式匹配进阶

[ToC]

---

## 代数数据类型

使用代数数据类型，我们是说某个数据类型可以是众多事物中的一个，用所谓的构造函数来区分和识别。

例如，"Maybe a "是指如果某个东西的类型是Maybe a，那么它可以是一个具有 "a "类型并由构造函数 "Just "封装的值，也可以是一个空值 "Nothing"。

```haskell
data Maybe a = Just a
             | Nothing
```

另一个例子是 "List"，从它的定义中我们可以看出它有一个递归结构。所以我们可以在列表中拥有任何数量的元素，但它们必须具有相同的类型。

```haskell
data List a = Cons a (List a)
            | Empty
{-
data [a] = a : [a]
         | []
-}
```



---

## Map

Map和Erlang的`Map`有着相同的功能。`Map k v`是Map的类型。

我们可以按照如下方式构建Map。

```Haskell
m1 :: Map String Integer
m1 = #{"Hello" => 5, "World" => 17}  

> lookup m1 "Hello"
Just  5

> insert "!" 0 m1
#{"Hello" => 5, "World" => 17, "!" => 0}

```



---

## Newtypes

"newtype "用于区分具有相同类型的值但单位/含义不同的两种类型。

例如：

```haskell
newtype Email = Email String

m1 :: Map Email Integer
m1 = empty
--This is forces we can only pass a String with a contrutor Email.
--So insert "abc" 123 m1 will fail
```



---

##简单的模式匹配



```haskell
fib 0 = 1
fib 1 = 1
fib x = fib (x - 1) + fib (x - 2)
```



---

## Guards



```haskell
max x y | x > y     = x
        | otherwise = y
```



---

##列表模式



```Haskell
isEmpty :: forall a.[a] -> Boolean
isEmpty  []      = true
isEmpty (x : xs) = false
```



---

## 记录模式



```haskell
showPerson :: { firstName :: Name, lastName :: Name } -> Name
showPerson { firstName: x, lastName: y } = y <> ", " <> x

> showPerson { firstName: "Phil", lastName: "Freeman" }
"Freeman, Phil"

> showPerson { firstName: "Phil", lastName: "Freeman", location: "Los Angeles" }
"Freeman, Phil"
```



---

## Map模式

我们还可以在`Map`上进行模式匹配，这和`Record`非常相似，只是在语法上做了一些改变。例如，"getID "让我们从Map中获取Wang的ID，其中至少要有Wang、Thomas 和黎Leeming 作为键。

```haskell
getID :: Map String Integer -> Maybe Integer
getID #{ "Wang":= x, "Thomas" := y, "Leeming" := z } = Just x
getID _                                              = Nothing

```



---

## 二进制模式

在二进制类型上的匹配就像在Erlang中的行为一样。在下面的例子中，我们试图从传递给getA的二进制中得到一个24位的整数。

```haskell
getA :: Binary -> Just Integer
getA << (a):24:Big-Integer | (b):4:Binary-Little | (c):32:Binary >> = Just a
getA _                                                               = Nothing
```

"大 "和 "小 "指的是我们所需要的部分的endianess。"整数 "或 "二进制 "是我们提取段后要赋予的类型。段的位数取决于我们需要的段的大小和我们分配的类型。如果我们分配的类型是一个 "整数"，那么我们得到的 "比特大小 "的数量是完全相同的，这需要被8整除。 如果我们想要的是一个 "二进制"，那么它将需要8倍的比特大小。



---

## Case表达式

当不需要绑定中间结果的时候，通过`case`，我们还可以在一些计算后的值上进行模式匹配，。

```haskell
plus (x, y) = x + y

sumUpTo :: Integer -> (Integer, Integer) -> Boolean
sumUpTo x p = case plus p of
                x -> true
                _ -> false
```

