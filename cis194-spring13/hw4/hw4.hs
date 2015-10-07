import Data.List

{-
Reimplement each of the following functions in a more idiomatic
Haskell style. Use wholemeal programming practices, breaking each
function into a pipeline of incremental transformations to an entire
data structure. Name your functions fun1’ and fun2’ respectively.

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
| even x = (x - 2) * fun1 xs
| otherwise = fun1 xs

2. fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n ‘div‘ 2)
| otherwise = fun2 (3 * n + 1)



iterate :: (a -> a) -> a -> [a]
base Prelude, base Data.List
iterate f x returns an infinite list of repeated applications of f to x: > iterate f x == [x, f x, f (f x), ...]

takeWhile :: (a -> Bool) -> [a] -> [a]
base Prelude, base Data.List
takeWhile, applied to a predicate p and a list xs, returns the longest prefix (possibly empty) of xs of elements that satisfy p: > takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2] > takeWhile (< 9) [1,2,3] == [1,2,3] > takeWhile (< 0) [1,2,3] == []
-}

fun1' :: [Integer] -> Integer
fun1' [] = 1
fun1' (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

{-
first filter out even elements
foreach x -> x - 2
product of the array
-}
fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even
-- filter even is the predicate
-- product = foldl (*) 1

fun2' :: Integer -> Integer
fun2' 1 = 0
fun2' n | even n = n + fun2' (n `div` 2)
        | otherwise = fun2' (3 * n + 1)

{-
100 + 50 + fun2 (76) ... + fun2(1) -> 0
hailstone sequence
[100, 50, 76, ..., 1, 4, 2, 1, ...]
takeWhile ->
[100, 50, 76, ..., 1]
filter even ->
[(odd elements), ..., 1]
sum ->
(the sum)
-}
fun2 :: Integer -> Integer
fun2 = sum . filter even . takeWhile (/= 1) . iterate (\x -> if even x then x `div` 2 else 3 * x + 1)

{-
Recall the definition of a binary tree data structure. The height of http://en.wikipedia.org/wiki/
Binary_tree a binary tree is the length of a path from the root to the deepest
node. For example, the height of a tree with a single node is 0; the
height of a tree with three nodes, whose root has two children, is 1;
and so on. A binary tree is balanced if the height of its left and right
subtrees differ by no more than 1, and its left and right subtrees are
also balanced.
You should use the following data structure to represent binary
trees. Note that each node stores an extra Integer representing the
height at that node.
data Tree a = Leaf
| Node Integer (Tree a) a (Tree a)
deriving (Show, Eq)
For this exercise, write a function

foldTree :: [a] -> Tree a
foldTree = ...
which generates a balanced binary tree from a list of values using
foldr.
For example, one sample output might be the following, also visualized
at right:

foldTree "ABCDEFGHIJ" ==
Node 3
(Node 2
(Node 0 Leaf ’F’ Leaf)
’I’
(Node 1 (Node 0 Leaf ’B’ Leaf) ’C’ Leaf))
’J’
(Node 2
(Node 1 (Node 0 Leaf ’A’ Leaf) ’G’ Leaf)
’H’
(Node 1 (Node 0 Leaf ’D’ Leaf) ’E’ Leaf))
Your solution might not place the nodes in the same exact order,
but it should result in balanced trees, with each subtree having a
correct computed height.
-}

data Tree a = Leaf | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- generate balanced bst is like fold
-- (Node <height> Leaf <value> Leaf)
foldTree :: [a] -> Tree a
foldTree [] = Leaf
foldTree [x] = Node 0 Leaf x Leaf
foldTree xs = Node height leftSubtree rootVal rightSubtree
  where
    -- bindings (like let?)
    n = length xs
    (ys, zs) = splitAt (n `div` 2) xs
    leftSubtree = foldTree ys
    rightSubtree = foldTree $ tail zs -- cut the root out
    -- wrong: this is not the height, but the total number of nodes in subtree
    -- height = toInteger n `div` 2 -- left subtree is n/2, right is n/2 (n is odd) or n/2 - 1 (n is even), int is bounded (machine), integer is unbounded
    -- compute height bottom-up
    getHeight Leaf = 0
    -- getHeight (Node h Leaf _ Leaf) = 0 -- should be 1? if this was excuted on "A", but it was matched in the 2nd function `foldTree [x] = Node 0 Leaf x Leaf`
    getHeight (Node h _ _ _) = h
    height = max (getHeight leftSubtree) (getHeight rightSubtree) + 1
    rootVal = head zs

-- https://wiki.haskell.org/How_to_work_on_lists
-- splitAt example:
-- let (ys,zs) = splitAt n xs   in   ys ++ [new_element] ++ zs
-- let xs = ...
-- let n = ...
-- let new_element = ...
-- splitAt n xs -> tuple
-- let (ys,zs) = splitAt n xs   in   ys ++ [new_element] ++ zs
-- -> ys ++ [new_elment] ++ zs
-- splitAt length n / 2


{-
1. Implement a function
xor :: [Bool] -> Bool
which returns True if and only if there are an odd number of True
values contained in the input list. It does not matter how many
False values the input list contains. For example,
xor [False, True, False] == True
xor [False, True, False, False, True] == False
xor [False, True, True, True, True] == False
xor [True, True, True, True, True] == True
xor [False, False] == False
xor [False] == False
Your solution must be implemented using a fold.
-}

{-
[True, False] two-element list xor is just a special case

False `xor` False == False
False `xor` True == True
True `xor` False = True
True `xor` True == False

(odd number of True values)
False ++ [False] -> False (even + 0 = even)
False ++ [True] -> True (even + 1 = odd)
True ++ [False] -> True (odd + 0 = odd)
True ++ [True] -> False (odd + 1 = even)

[] -> False (0 Trues)

Data.Boolean xor
  \x y -> (x || y) && not (x && y)
-}
xor :: [Bool] -> Bool
xor = foldl (\x y -> (x || y) && not (x && y)) False

{-
2. Implement map as a fold. That is, complete the definition
map’ :: (a -> b) -> [a] -> [b]
map’ f = foldr ...
in such a way that map’ behaves identically to the standard map

map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs

  acc <- f x : acc
accumulator:
  what's map f []?

Test:
*Main> map' (+1) [1,2,3,4]
[2,3,4,5]
*Main> map (+1) [1,2,3,4]
[2,3,4,5]
-}
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

{-
. (Optional) Implement foldl using foldr. That is, complete the
definition
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr ...
in such a way that myFoldl behaves identically to the standard
foldl function.
Hint: Study how the application of foldr and foldl work out:
foldr f z [x1, x2, ..., xn] == x1 ‘f‘ (x2 ‘f‘ ... (xn ‘f‘ z)...)
foldl f z [x1, x2, ..., xn] == (...((z ‘f‘ x1) ‘f‘ x2) ‘f‘...) ‘f‘ xn
-}

{-
https://wiki.haskell.org/Foldl_as_foldr
http://stackoverflow.com/questions/6172004/writing-foldl-using-foldr
-}
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs =

{-
Read about the Sieve of Sundaram. Implement the algorithm us- http://en.wikipedia.org/wiki/Sieve_
of_Sundaram ing function composition. Given an integer n, your function should
generate all the odd prime numbers up to 2n + 2.

Start with a list of the integers from 1 to n. From this list, remove all numbers of the form i + j + 2ij where:
i,j\in\mathbb{N},\ 1 \le i \le j
i + j + 2ij \le n

The remaining numbers are doubled and incremented by one, giving a list of the odd prime numbers (i.e., all primes except 2) below 2n + 2.


sieveSundaram :: Integer -> [Integer]
sieveSundaram = ...
To give you some help, below is a function to compute the Cartesian
product of two lists. This is similar to zip, but it produces all
possible pairs instead of matching up the list elements. For example,
cartProd [1,2] [’a’,’b’] == [(1,’a’),(1,’b’),(2,’a’),(2,’b’)]
It’s written using a list comprehension, which we haven’t talked about
in class (but feel free to research them).
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
-}
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = [2 * n + 1 | n <- filteredNumbers]
                  --map (\n -> 2 * n + 1) filteredNumbers
  where filteredNumbers = [1..n] \\ [i + j + 2 * i * j | i <- [1..n], j <- [i..n], i + j + 2 * i * j <= n]

-- (\\) is set difference, the first set minus the second set
-- (\\)                    :: (Eq a) => [a] -> [a] -> [a]
-- (\\)                    =  foldl (flip delete)
