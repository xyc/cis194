module JoinList where

-- import Data.Monoid

import Sized

import Scrabble

import Buffer
import Editor

{-
implementing a lightweight,
tree-like structure, both for holding the data and caching the
metadata. This data structure is referred to as a join-list.

The intent of this data structure is to directly represent append
operations as data constructors. This has the advantage of making
append an O(1) operation: sticking two JoinLists together simply
involves applying the Append data constructor.

The m parameter will be used to track monoidal annotations to the
structure. The idea is that the annotation at the root of a JoinList
will always be equal to the combination of all the annotations on
the Single nodes (according to whatever notion of “combining” is
defined for the monoid in question). Empty nodes do not explicitly
store an annotation, but we consider them to have an annotation of
mempty (that is, the identity element for the given monoid).

The point of doing this is that all the subcomputations needed to
compute the product of all the annotations in the join-list are cached.
If we now change one of the annotations, say, the annotation on ’y’,
we need only recompute the annotations on nodes above it in the
tree. In particular, in this example we don’t need to descend into the
subtree containing ’e’ and ’a’, since we have cached the fact that
their product is 6. This means that for balanced join-lists, it takes
only O(log n) time to rebuild the annotations after making an edit.
-}


{-
m parameter: track monoidal annotations
-}
data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

{-
gets the annotation at the root of a JoinList
m, a are both type variables
Monoid m class constraint: m is an monoid
-}
tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) = m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
-- (+++) x Empty = x
-- (+++) Empty x = x -- don't need this
(+++) x y = Append (mappend (tag x) (tag y)) x y -- mappend monoidal annotations

{-
The first annotation to try out is one for fast indexing
into a JoinList. The idea is to cache the size (number of data elements)
of each subtree. This can then be used at each step to determine
if the desired index is in the left or the right branch.

We have provided the Sized module that defines the Size type,
which is simply a newtype wrapper around an Int. In order to make
Sizes more accessible, we have also defined the Sized type class
which provides a method for obtaining a Size from a value
-}

{-
indexJ finds the JoinList element at the specified index. If the
index is out of bounds, the function returns Nothing. By an index
in a JoinList we mean the index in the list that it represents.

this is essentially a binary tree like data structure:
Single: leaf
Append: non-leaf node
Empty: null
-}

{-
Extract the cached size
-}
jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize Empty = 0
jlSize (Single b a) = 1
jlSize (Append b jl1 jl2) = getSize (size b)
-- jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i Empty = Nothing
indexJ i jl | i < 0 = Nothing
-- Single JoinList size should be 1
-- size b :: Size
indexJ i (Single b a) | getSize (size b) < i = Nothing
indexJ i (Single b a) = Just a
-- left Append JoinList size
-- right Append JoinList size
indexJ i (Append b jl1 jl2) | getSize (size b) < i = Nothing
indexJ i (Append b jl1 jl2) = let leftTreeSize = jlSize jl1 in
  if i < leftTreeSize then
    indexJ i jl1
  else
    indexJ (i - leftTreeSize) jl2

{-
For any index i and join-list jl, it should be the case that
(indexJ i jl) == (jlToList jl !!? i)
The point, of course, is that indexJ can be more efficient (O(log n) versus
O(n), assuming a balanced join-list), because it gets to use the size
annotations to throw away whole parts of the tree at once, whereas
the list indexing operation has to walk over every element.
-}
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1) -- using pattern matching to iterate over list

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2


{-
The dropJ function drops the first n elements from a JoinList.
drops the first n leafs (jlToList's result list size is leaf numbers)
jlToList is in-order traversal? no-order
jlToList (dropJ n jl) == drop n (jlToList jl).
-}
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl | n <= 0 = jl
           | n >= jlSize jl = Empty
-- n > 0 && n < jl's size
dropJ _ Empty = Empty
dropJ _ (Single _ _) = Empty
-- dropJ i jl@(Single b a) | jlSize jl < i = Empty
-- dropJ i jl@(Single b a) = jl
dropJ n (Append b jl1 jl2) = let leftTreeSize = jlSize jl1 in
  if n < leftTreeSize then
    dropJ n jl1 +++ jl2 -- drop partial left subtree and concat with right (the new count is updated because of +++)
  else
    dropJ (n - leftTreeSize) jl2 -- drop entire left subtree

{-
The takeJ function returns the first n elements of a JoinList,
dropping all other elements.

jlToList (takeJ n jl) == take n (jlToList jl)
-}
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n jl | n <= 0 = Empty
           | n >= jlSize jl = jl
takeJ _ Empty = Empty
takeJ n jl@(Single _ _) = jl
takeJ n (Append b jl1 jl2) = let leftTreeSize = jlSize jl1 in
  if n < leftTreeSize then
    takeJ n jl1
  else
    jl1 +++ takeJ (n - leftTreeSize) jl2

{-
String to a JoinList (with String data and score annotation):
  converted to a leaf node

test: scoreLine "yay " +++ scoreLine "haskell!"
-}
scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

type JoinListBuffer = JoinList (Score, Size) String

instance Buffer JoinListBuffer where
  -- | Convert a buffer to a String.
  -- toString :: JoinListBuffer -> String
  toString = unlines . jlToList

  -- | Create a buffer from a String.
  -- fromString :: String -> JoinListBuffer
  fromString s = foldl (+++) Empty map (\s -> Single (scoreString s, Size 1) s) . lines

  -- | Extract the nth line (0-indexed) from a buffer.  Return Nothing
  -- for out-of-bounds indices.
  -- line :: Int -> b -> Maybe String
  line = indexJ

  -- | @replaceLine n ln buf@ returns a modified version of @buf@,
  --   with the @n@th line replaced by @ln@.  If the index is
  --   out-of-bounds, the buffer should be returned unmodified.
  replaceLine n ln buf = takeJ n buf +++ fromString ln +++ dropJ (n + 1) buf

  -- | Compute the number of lines in the buffer.
  numLines = jlSize

  -- | Compute the value of the buffer, i.e. the amount someone would
  --   be paid for publishing the contents of the buffer.
  value = \(Score i)->i . fst . tag

main = runEditor editor jlBuffer
  where jlBuffer = fromString $ unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ] :: JoinListBuffer
