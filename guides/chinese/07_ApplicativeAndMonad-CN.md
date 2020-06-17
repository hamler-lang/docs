
#Applicative 函子和单子

[ToC]

在这一章中，我们要看两个非常重要的类型类，应用型 Functor 和 Monad。

**Applicative**

我们先来看看Applicative类型类是如何定义的。从定义中，我们可以看到，Applicatives他们是Functors，还有两个操作方法，就是pure和apply。`pure`包装一些值，使之成为一个applicative函子。`apply`比pure更加复杂一点。

我们就看看它的类型，有印象吗？是的，它看起来像map，只是我们的函数被包裹在一个应用型函子中。`apply`做的是从漏斗中提取函数，并将其映射到`f a`。

```haskell
class Functor f => Applicative f where
  pure  :: forall a. a -> f a
  apply :: forall a b. f (a -> b) -> f a -> f b

infixl 4 apply as <*>
```

以下是 "Applicatve "的一些实例。

```haskell
实例 应用性的 Maybe，其中
  纯粹=公正
  Nothing <*> mb = Nothing
  (Just f) <*> mb = map f mb

实例 应用型 [] 其中
  pure = (:[])
  fs <*> xs = [f x | x <- xs, f <- fs]
```

让我们仔细看看实例`Applicative[]`，我们可以看到列表中的每一个`f`都会被应用到列表中的所有元素。 所以，随着`(+)<$> [1,2] <*> [3,4,5]`，我们将对`(+)`进行非确定性计算。

**单子**



```haskell
class Applicative m => Monad m where
  bind :: forall a b. m a -> (a -> m b) -> m b
  return :: forall a. a -> m a
```



**"类型提升(lifting)"**

```Haskell
liftA1 :: forall a b f. Applicative f => (a -> b) -> f a -> f b
liftA1 f a = pure f <*> a

liftA2 :: forall a b c f. Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = pure f <*> a <*> b

liftM1 :: forall a b m. Monad m => (a -> b) -> m a -> m b
liftM1 f ma = do
  a <- ma
  return (f a)

liftM2 :: forall a b c m. Monad m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f ma mb = do
  a <- ma
  b <- mb
  return (f a b)
```