


# 基本类型、函数和运算符

[ToC]

---

## 简单类型

Hamler是强类型的，并且拥有强大的静态类型系统。我们先来看一些简单的例子。

**布尔类型**

```Haskell
true :: Boolean
false :: Boolean
```

**数值类型**

Hamler有Integer和Float，由于它们的类型不同所以不能混用。

```Haskell
--Integer
1 :: Int

--Float
0.1 :: Float
```

**原子类型**

Atom对于Erlang用户来说可能更熟悉。它是一个字面量，一个名字以`:`开头的常量。

```
:hello
:world
```

**字符串**

在Hamler中，`String`只是一个`Char`的列表。

```Haskell
"Hello World" :: String  -- ['H','e','l','l','o',',','W','o','r','l','d']
```

**二进制类型**。

这是Erlang中存在的非常独特的数据类型，对于Haskell用户来说，`Binary`包含的信息和`ByteString`是一样的，如果你对二进制类型不是很熟悉，这个[链接](https://erlang.org/doc/man/binary.html)应该对形成一些直观的认识有帮助。

```haskell
<<1,2,3:8,4:16,5,"abcdefg">> :: Binary
```



---

## 运行符

| Operator | Meaning                |      | Operator | Meaning               |
| -------- | ---------------------- | ---- | -------- | --------------------- |
| +        | Numeric addition       |      | ==       | Equality check        |
| -        | Numeric subtraction    |      | <        | Less than             |
| *        | Numeric multiplication |      | <=       | Less than or equal    |
| /        | Numeric division (div) |      | >        | Greater than          |
| %        | Remainder              |      | >=       | Greater than or equal |
| &&       | Boolean AND            |      | \|\|     | Boolean OR            |




---

## 函数

当我们定义一个新函数时，我们可以给它一个类型签名。例如 `double`是一个函数，它接收一个`Integer`，并给出一个`Integer`的两倍作为输出。

```haskell
double :: Integer -> Integer
double x = x * 2
```

**Lambda表达式**

在Hamler中也有lambda表达式，下面是我们重写double的例子。

```haskell
double' :: Integer -> Integer
double' = \x -> 2 * x
```

当我们需要编写一个匿名函数时，它就变得非常方便。

**柯里化**

```haskell
--Curry
--This is uncurried (+)
add :: (Integer, Integer) -> Integer
add (x, y) = x + y

--This is curried (+)
plus :: Integer -> Integer -> Integer
plus x y = x + y
```

**Partial Application**

```Haskell
-- plus :: Integer -> (Integer -> Integer) This is one of the example of higher order functions
>:t plus 2
plus 2:: Integer -> Integer
>let plusTwo = plus2
>plusTwo 3
5
```



---

## Quantified Types

它们也被称为**多态类型**。

```Haskell
> :type id
id :: forall a. a -> a
```

关键字 "forall "表示 "id "是统一quantified，这意味着 "id "可以应用于任何类型。

```Haskell
> id 1
1
```

一个更复杂的例子是`flip`，`flip`也是一个[高阶函数](05_HigherOrderFunctionsAndRecursions.md)。`flip`也是一个[高阶函数](05_HigherOrderFunctionsAndRecursions.md)，这将在后面的章节中解释。

```Haskell
> :type flip
forall a b c. (a -> b -> c) - > b -> a -> c。
```



---

## 缩进说明

像所有的ML语言家族一样，Hamler对缩进敏感。在同一区块中的任何声明都应该有相同程度的缩进。如果一个声明跨越了多行，其他行必须在第一行之后进行缩进。

```Haskell
flip x f = f
x                   -- NOT OKAY, Hamler will see x as a seperate declaration

flip f x = f
    x               -- OKAY, but not recommended
```

**"Let "和 "Where "区块**。

Let和Where等关键词创建了一个新的区块，这里需要进一步缩进。

```haskell
distance x y = sqrt z
  where
    z = x' + y'
    x' = x * x
    y' = y * y
```



---

## 类型别名

类型别名可以用来简化一个长的类型名，使代码更易读。

```Haskell
>:i String
type String = [Char]
```

或者你可以自己定义别名名称或记录。



---

## 记录

```Haskell
type Name = String

type Person =
  { firstName  :: Name
  , secondName :: Name
  }

{-
This is syntax sugared
"type Person = Record (FirstName  :: Name , SecondName :: Name)"
-}
```

字段可以通过`.`访问。

```haskell
leader :: Person
leader = {firstName : "John", lastName : "Portsman"}

>leader.firstName
"John"
```

这就是我们更新记录的方法。

```haskell
newLeader :: Person
newLeader = Leader {firstName : "James"}

>newLeader.lastName
"Portsman"
```
