


# 类型类

[ToC]

---

## 介绍

一个类型类定义了一些由其操作相关的类型。这是说类型类通常是由归属于它的操作方法来定义的，这些操作方法把这些类型进行了分组。

例如，我们可以把所有可以转换为 "字符串 "的类型放在同一个类型类中，称为 "Show"。

我们可以通过以下方式来引入"show "这个类型类的定义如下。

```Haskell
class Show a where
  show :: a -> String
```

然后我们可以给出这个类型类的一个实例。

```haskell
instance Show String where
  show s = s

instance Show Boolean where
  show true = "true"
  show false = "false"
```



---

## 函子

我们已经看到了如何为`[]`定义map，我们也可以对其他类型进行映射。如果这些类型能够被 "映射"，并且能够同时满足 "Functor "法则，我们就称它们为 "Functor"。

 函子法则：                                  `map id = id | map (compose g f) = map g . map f `。

```haskell
class Functor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance Functor Maybe where
  map f (Just x) = Just (f x)
  map f  Nothing = Nothing

instance Functor [] where
  map f []     = []
  map f (x:xs) = f x : map f xs
```



---

## 其他常见类型类

```haskell
class Eq a where
  eq :: a -> a -> Boolean

-- data Ordering = LT | GT | EQ
class Eq a => Ord a where
  compare :: a -> a -> Ordering

class Foldable f where
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b

class Semigroup a where
  append :: a -> a -> a
```
