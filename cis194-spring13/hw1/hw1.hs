module HW1 where

{-
Double the value of every second digit beginning from the right.
That is, the last digit is unchanged; the second-to-last digit is doubled;
the third-to-last digit is unchanged; and so on. For example,
[1,3,8,6] becomes [2,3,16,6].

Add the digits of the doubled values and the undoubled digits
from the original number. For example, [2,3,16,6] becomes
2+3+1+6+6 = 18

Calculate the remainder when the sum is divided by 10. For the
above example, the remainder would be 8.
-}

{-
Example: toDigits 1234 == [1,2,3,4]
Example: toDigitsRev 1234 == [4,3,2,1]
Example: toDigits 0 == []
Example: toDigits (-17) == []
-}

toDigits :: Integer -> [Integer]
toDigits n = reverse (toDigitsRev n)


{-
toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n = (n `mod` 10) : toDigitsRev (n `div` 10)
-}

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
            | n <= 0    = []
            | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

{-
Remember that doubleEveryOther should double every other number
beginning from the right, that is, the second-to-last, fourth-to-last,
. . . numbers are doubled.

Example: doubleEveryOther [8,7,6,5] == [16,7,12,5]
Example: doubleEveryOther [1,2,3] == [1,4,3]

http://stackoverflow.com/questions/17383169/haskell-double-every-2nd-element-in-list
http://stackoverflow.com/questions/19867491/double-every-other-element-of-list-from-right-in-haskell
http://stackoverflow.com/questions/20459241/haskell-how-to-iterate-list-elements-in-reverse-order-in-an-elegant-way

-- zipWith takes a function and two lists here: (function) [things...] [things...]
so doubleEveryOther [1...n] == zipWith ($) [(*2), id, (*2), id, ...] [1..n]
  since $ is infix application for functions
doubleEveryOther = zipWith ($) (cycle [(*2),id])
-}

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther (x : []) = [x]
doubleEveryOther (x : y : zs) = x * 2 : y : doubleEveryOther zs

-- TODO: this only works for even sized xs now
-- cycle [id, (*2)] -> id, (*2), id, (*2), ...
-- zipWith .. -> ($, id), ($, (*2)), ...
doubleEveryOther xs = reverse (zipWith ($) (cycle [id, (*2)]) (reverse xs))


{-
The output of doubleEveryOther has a mix of one-digit
and two-digit numbers. Define the function
sumDigits :: [Integer] -> Integer
to calculate the sum of all digits.
Example: sumDigits [16,7,12,5] = 1 + 6 + 7 + 1 + 2 + 5 = 22
-}
sumDigit :: Integer -> Integer
sumDigit n = sum (toDigits n)

sumDigits :: [Integer] -> Integer
sumDigits d = sum (map (sumDigit) d)

{-
sumDigits [] = 0
sumDigits [n] = sum (toDigits n)
sumDigits (n:ns) = n + sumDigits ns
-}

{-
that indicates whether an Integer could be a valid credit card number.
This will use all functions defined in the previous exercises.
Example: validate 4012888888881881 = True
Example: validate 4012888888881882 = False
-}
validate :: Integer -> Bool
validate n = (sumDigits (doubleEveryOther (toDigits n))) `mod` 10 == 0

{-
http://rosettacode.org/wiki/Towers_of_Hanoi#Haskell

http://mathworld.wolfram.com/TowerofHanoi.html
  The problem is isomorphic to finding a Hamiltonian path on an n-hypercube (Gardner 1957, 1959)

To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,
1. move n − 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n − 1 discs from c to b using a as temporary storage

hanoi 2 "a" "b" "c"
-}
type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n - 1) a c b ++ [(a, b)] ++ hanoi (n - 1) c b a

{-
What if there are four pegs instead of three?
That is, the goal is still to move a stack of discs from the first peg to
the last peg, without ever placing a larger disc on top of a smaller
one, but now there are two extra pegs that can be used as “temporary”
storage instead of only one. Write a function similar to hanoi
which solves this problem in as few moves as possible.
It should be possible to do it in far fewer moves than with three
pegs. For example, with three pegs it takes 215 − 1 = 32767 moves
to transfer 15 discs. With four pegs it can be done in 129 moves. (See
Exercise 1.17 in Graham, Knuth, and Patashnik, Concrete Mathematics,
second ed., Addison-Wesley, 1994.)
-}

-- TODO:
