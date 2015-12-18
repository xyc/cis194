# Monad

Readings
- [The Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)



```haskell
import Control.Monad
```

## Motivation
> Monads are just monoids on the category of endofunctors.

A monad is handy whenever a programmer wants to sequence actions. The details of the monad says exactly how the actions should be sequenced. A monad may also store some information that can be read from and written to while performing actions.

We’ve already learned about the **IO monad**, which sequences its actions quite naturally, performing them in order, and gives actions access to read and write anything, anywhere. We’ll also see the **Maybe** and **[] (pronounced “list”)** monads, which don’t give any access to reading and writing, but do interesting things with sequencing. And, for homework, you’ll use the **Rand monad**, which doesn’t much care about sequencing, but it does allow actions to read from and update a random generator.


#### Example
We would like to write a function that zips two binary trees together by applying a function to the values at each node. However, the function should fail if the structure of the two trees are different. Note that by fail, we mean return Nothing. Here is a first try at writing this function:

```haskell
data Tree a = Node (Tree a) a (Tree a)
            | Empty
              deriving (Show)

zipTree1 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree1 _ (Node _ _ _) Empty = Nothing
zipTree1 _ Empty (Node _ _ _) = Nothing
zipTree1 _ Empty Empty        = Just Empty
zipTree1 f (Node l1 x r1) (Node l2 y r2) =
    case zipTree1 f l1 l2 of
      Nothing -> Nothing
      Just l  -> case zipTree1 f r1 r2 of
                   Nothing -> Nothing
                   Just r  -> Just $ Node l (f x y) r
```

```haskell
bindMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
bindMaybe mx f = case mx of
                   Nothing -> Nothing
                   Just x  -> f x
```

Using this helper, we can refactor the code to be much more elegant.
```haskell
zipTree2 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree2 _ (Node _ _ _) Empty = Nothing
zipTree2 _ Empty (Node _ _ _) = Nothing
zipTree2 _ Empty Empty        = Just Empty
zipTree2 f (Node l1 x r1) (Node l2 y r2) =
    bindMaybe (zipTree2 f l1 l2) $ \l ->
        bindMaybe (zipTree2 f r1 r2) $ \r ->
            Just (Node l (f x y) r)

-- bind operator 
zipTree3 :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree3 _ (Node _ _ _) Empty = Nothing
zipTree3 _ Empty (Node _ _ _) = Nothing
zipTree3 _ Empty Empty        = Just Empty
zipTree3 f (Node l1 x r1) (Node l2 y r2) =
    zipTree3 f l1 l2 >>= \l ->
        zipTree3 f r1 r2 >>= \r ->
            return (Node l (f x y) r)         

-- do notation
zipTree :: (a -> b -> c) -> Tree a -> Tree b -> Maybe (Tree c)
zipTree _ (Node _ _ _) Empty = Nothing
zipTree _ Empty (Node _ _ _) = Nothing
zipTree _ Empty Empty        = Just Empty
zipTree f (Node l1 x r1) (Node l2 y r2) = do
    l <- zipTree f l1 l2
    r <- zipTree f r1 r2
    return $ Node l (f x y) r            
```

Monad type class
```haskell
class Monad m where
  return :: a -> m a

   -- pronounced "bind"
  (>>=) :: m a -> (a -> m b) -> m b

  (>>)  :: m a -> m b -> m b
  m1 >> m2 = m1 >>= \_ -> m2
```                   
