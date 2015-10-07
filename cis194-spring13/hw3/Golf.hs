
module Golf where

import Data.List
import qualified Data.Map as Map

{-
skips :: [a] -> [[a]]
The output of skips is a list of lists. The first list in the output should
be the same as the input list. The second list in the output should
contain every second element from the input list. . . and the nth list in
the output should contain every nth element from the input list.
For example:
skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips [1] == [[1]]
skips [True,False] == [[True,False], [False]]
skips [] == []
Note that the output should be the same length as the input.
-}

skips :: [a] -> [[a]]
-- select every
-- select every second
-- n = length ys
-- http://stackoverflow.com/questions/11531328/defining-variables-inside-a-function-haskell
-- foo a = b where b = a * 2
-- foo a = let b = a * 2 in b
{-
ys = "ABCD"
-- append an index to each element of the list
-- the index preceding the element will decide whether the element will be filtered
[(x,y) <- zip [1..n] ys]
> [(1, 'A'), (2, 'B'),  (3, 'C'), (4, 'D')]
[y | (x,y) <- zip [1..n] ys]
> ['A', 'B', 'C', 'D']
[[y | (x,y) <- zip [1..n] ys, x `mod` i == 0] | i <- [1]]
> [['A', 'B', 'C', 'D']]
n = length ys
[[y | (x,y) <- zip [1..n] ys, x `mod` i == 0] | i <- [1..n]]
> [['A', 'B', 'C', 'D'], ['B', 'D'], ['C'], ['D']]
-}
skips ys =
  [[ y | (x,y) <- zip [1..n] ys, x `mod` i == 0] | i <- [1..n]] where n = length ys

{-
A local maximum of a list is an element of the list which is strictly
greater than both the elements immediately before and after it. For
example, in the list [2,3,4,1,5], the only local maximum is 4, since
it is greater than the elements immediately before and after it (3 and
1). 5 is not a local maximum since there is no element that comes
after it.
Write a function
localMaxima :: [Integer] -> [Integer]
which finds all the local maxima in the input list and returns them in
order. For example:
localMaxima [2,9,5,6,1] == [9,6]
localMaxima [2,3,4,1,5] == [4]
localMaxima [1,2,3,4,5] == []
localMaxima [1,2]
-}

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [ y | (x, y, z) <- zip3 xs (drop 1 xs) (drop 2 xs), x < y && y > z ]

{-
For this task, write a function
histogram :: [Integer] -> String
which takes as input a list of Integers between 0 and 9 (inclusive),
and outputs a vertical histogram showing how many of each number
were in the input list. You may assume that the input list does not
contain any numbers less than zero or greater than 9 (that is, it does
not matter what your function does if the input does contain such
numbers). Your output must exactly match the output shown in the
examples below.

Important note: If you type something like histogram [3,5] at
the ghci prompt, you should see something like this:
" * * \n==========\n0123456789\n"
This is a textual representation of the String output, including \n
escape sequences to indicate newline characters. To actually visualize
the histogram as in the examples above, use putStr, for example,
putStr (histogram [3,5]).

histogram [1,4,5,4,6,6,3,4,2,4,9]
-}

histogram :: [Integer] -> String
histogram xs = unlines ([[getHistogramChar i j | i <- [0..9]] | j <- [1..n]] ++ ["=========="] ++ ["0123456789"])
  where
    ss = group (sort xs)  -- [[1],[2],[3],[4,4,4,4],[5],[6,6],[9]]
    n = maximum [length y| y <- ss]
    m = Map.fromList [(head y, length y) | y <- ss]
    getHistogramChar i j = case Map.lookup i m of
                              Nothing -> ' '
                              Just count -> case j + count > n of
                                {-
                                j+count==n (the blank space on the boundary)
                                -}
                                True -> '*'
                                False -> ' '

-- let xs = [1,4,5,4,6,6,3,4,2,4,9]
-- maximum [length y| y <- group (sort xs)]

-- test
-- putStr( histogram [1,4,5,4,6,6,3,4,2,4,9])
