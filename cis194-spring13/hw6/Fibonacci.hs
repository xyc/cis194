{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

{-
https://wiki.haskell.org/The_Fibonacci_sequence

Fibonacci numbers
The Fibonacci numbers Fn are defined as the sequence of integers,
beginning with 0 and 1, where every integer in the sequence is the
sum of the previous two. That is,
F0 = 0
F1 = 1
Fn = Fn−1 + Fn−2 (n ≥ 2)
For example, the first fifteen Fibonacci numbers are
0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, . . .
It’s quite likely that you’ve heard of the Fibonacci numbers before.
The reason they’re so famous probably has something to do with the
simplicity of their definition combined with the astounding variety of
ways that they show up in various areas of mathematics as well as art
and nature.1
-}

{-
Translate the above definition of Fibonacci numbers directly into a
recursive function definition of type
fib :: Integer -> Integer
so that fib n computes the nth Fibonacci number Fn.
Now use fib to define the infinite list of all Fibonacci numbers,
fibs1 :: [Integer]
(Hint: You can write the list of all positive integers as [0..].)
Try evaluating fibs1 at the ghci prompt. You will probably get
bored watching it after the first 30 or so Fibonacci numbers, because
fib is ridiculously slow. Although it is a good way to define the Fibonacci
numbers, it is not a very good way to compute them—in order to compute Fn
it essentially ends up adding 1 to itself Fn times! For
example, shown at right is the tree of recursive calls made by evaluating fib 5.
-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

-- map fib [1..10]
{-As you can see, it does a lot of repeated work. In the end, fib
has running time O(Fn), which (it turns out) is equivalent to O(ϕ^n), where ϕ = (sqrt 5 + 1)/2
is the “golden ratio”. That’s right, the running time
is exponential in n. What’s more, all this work is also repeated from
each element of the list fibs1 to the next. Surely we can do better.-}

{-
test:
take 10 fibs1
-}
fibs1 :: [Integer]
fibs1 = map fib [0..]

{-
When I said “we” in the previous sentence I actually meant “you”.
Your task for this exercise is to come up with more efficient implementation.
Specifically, define the infinite list
fibs2 :: [Integer]
so that it has the same elements as fibs1, but computing the first n
elements of fibs2 requires only O(n) addition operations. Be sure to
use standard recursion pattern(s) from the Prelude as appropriate.
-}

{-
lazy evaluation (fib is dynamic programming)

zipWith :: (a->b->c) -> [a]->[b]->[c]
zipWith _f []     _bs    = []
zipWith _f _as    []     = []
zipWith f  (a:as) (b:bs) = f a b : zipWith f as bs
-}
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- iterate takes a function, a value and returns an infinite list where the value gets applied by the function
-- iterate (\(a, b) -> (b, a + b)) (0, 1) == [(0, 1), (1, 1), (1, 2), (2, 3), (3, 5)..]
-- iterate :: (a -> a) -> a -> [a]
-- iterate f x =  x : iterate f (f x)
-- look how iterate is similar to repeat
-- repeat = iterate id
fibs2' :: [Integer]
fibs2' = map fst $ iterate (\(a, b) -> (b, a + b)) (0, 1)

{-
We can be more explicit about infinite lists by defining a type Stream
representing lists that must be infinite. (The usual list type represents
lists that may be infinite but may also have some finite length.)
In particular, streams are like lists but with only a “cons” constructor—
whereas the list type has two constructors, [] (the empty list) and (:) (cons),
there is no such thing as an empty stream. So a stream is
simply defined as an element followed by a stream.

Exercise 3
• Define a data type of polymorphic streams, Stream.
• Write a function to convert a Stream to an infinite list,
streamToList :: Stream a -> [a]
• To test your Stream functions in the succeeding exercises, it will be
useful to have an instance of Show for Streams. However, if you put
deriving Show after your definition of Stream, as one usually does,
the resulting instance will try to print an entire Stream—which,
of course, will never finish. Instead, you should make your own
instance of Show for Stream

instance Show a => Show (Stream a) where
show ...
which works by showing only some prefix of a stream (say, the
first 20 elements).

-}

-- polymorphic stream
-- stream is infinite, with only 1 "cons" constructor
data Stream a = StreamCons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (StreamCons x s) = x : streamToList s

instance Show a => Show (Stream a) where
  -- showing 20 elements of stream
  show s = show $ take 20 $ streamToList s

{-
Let’s create some simple tools for working with Streams.
• Write a function
streamRepeat :: a -> Stream a
which generates a stream containing infinitely many copies of the
given element.
• Write a function
streamMap :: (a -> b) -> Stream a -> Stream b
which applies a function to every element of a Stream.
• Write a function
streamFromSeed :: (a -> a) -> a -> Stream a
which generates a Stream from a “seed” of type a, which is the
first element of the stream, and an “unfolding rule” of type a -> a
which specifies how to transform the seed into a new seed, to be
used for generating the rest of the stream.
-}

-- streamRepeat x = StreamCons x (need a stream)
streamRepeat :: a -> Stream a
streamRepeat x = StreamCons x (streamRepeat x)

-- streamMap applies f to each element in the stream
streamMap :: (a -> b) -> Stream a -> Stream b
--streamMap f = \s -> (...)
streamMap f (StreamCons x s) = StreamCons (f x) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = StreamCons x (streamFromSeed f (f x)) -- f is the unfolding rule, x is the seed, (f x) is the new seed

{-
Now that we have some tools for working with streams, let’s create
a few:
• Define the stream
nats :: Stream Integer
which contains the infinite list of natural numbers 0, 1, 2, . . .

-}
nats :: Stream Integer
nats = streamFromSeed succ 0
-- nats = streamFromSeed (+1) 0

{-
• Define the stream
ruler :: Stream Integer
which corresponds to the ruler function
0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0, 4, . . .
where the nth element in the stream (assuming the first element corresponds to n = 1)
 is the largest power of 2 which evenly divides n

1, 2, 3, 4, 5, ... n
1, 2, 1, 4, 1, ... power that evenly divides n (try from 0, 1, 2, 4, ...)
0, 1, 0, 2, 0, ... base
-}
ruler :: Stream Integer
ruler = streamMap r (streamFromSeed succ 1)
-- number of zeroes after the right most 1
-- right shift until encounter 1
-- for example: 0b1001000 -> 3
-- odd number's last bit is 1, so r x = 0
-- for even number, count the right shift times
  where r x | odd x = 0
            | otherwise = r (x `div` 2) + 1

{-
This section is optional but very cool, so if you have time I hope you
will try it. We will use streams of Integers to compute the Fibonacci
numbers in an astounding way.
The essential idea is to work with generating functions of the form
a0 + a1x + a2x2 + · · · + anxn + . . .
where x is just a “formal parameter” (that is, we will never actually
substitute any values for x; we just use it as a placeholder) and all the
coefficients ai are integers. We will store the coefficients a0, a1, a2, . . .
in a Stream Integer.
-}

{-
Exercise 6 (Optional)
First, define
x :: Stream Integer
by noting that x = 0 + 1x + 0x^2 + 0x^3 + . . . .
-}

x :: Stream Integer
-- 0 : 1 : repeat 0
x = StreamCons 0 $ StreamCons 1 $ streamRepeat 0

{-
Define an instance of the Num type class for Stream Integer
Here’s what should go in your Num instance:
– You should implement the fromInteger function. Note that
n = n + 0x + 0x^2 + 0x^3 + . . . .
– You should implement negate: to negate a generating function,
negate all its coefficients.
– You should implement (+), which works like you would expect:
(a0 + a1x + a2x^2 + . . .) + (b0 + b1x + b2x^2 + . . .) = (a0 + b0) +
(a1 + b1)x + (a2 + b2)x^2 + . . .
– Multiplication is a bit trickier. Suppose A = a0 + xA0 and
B = b0 + xB0 are two generating functions we wish to multiply.
We reason as follows:
AB = (a0 + xA')B
= a0B + xA'B
= a0(b0 + xB') + xA'B
= a0b0 + x(a0B' + A'B)
That is, the first element of the product AB is the product of
the first elements, a0b0; the remainder of the coefficient stream
(the part after the x) is formed by multiplying every element in
B'
(that is, the tail of B) by a0, and to this adding the result of
multiplying A'
(the tail of A) by B.
-}

-- Numeric type class: https://hackage.haskell.org/package/base-4.8.1.0/docs/src/GHC.Num.html#Num
instance Num (Stream Integer) where
  fromInteger n = StreamCons n $ streamRepeat 0
  -- negate each coefficient
  negate = streamMap negate
  -- adding each elements in both streams
  (+) (StreamCons x0 xs) (StreamCons y0 ys) = StreamCons (x0 + y0) $ (+) xs ys
  (*) (StreamCons x0 xs) s2@(StreamCons y0 ys) = StreamCons (x0 * y0) $ streamMap (* x0) ys + xs * s2
  -- abs, signum not implemented

{-
If you have implemented the above correctly, you should be able
to evaluate things at the ghci prompt such as
*Main> x^4
*Main> (1 + x)^5
*Main> (x^2 + x + 3) * (x - 5)
*-}

{-
The penultimate step is to implement an instance of the Fractional
class for Stream Integer. Here the important method to define is
division, (/). I won’t bother deriving it (though it isn’t hard), but
it turns out that if A = a0 + xA0 and B = b0 + xB0
, then A/B = Q,
where Q is defined as
Q = (a0/b0) + x((1/b0)(A' − QB')).
-}
instance Fractional (Stream Integer) where
  (/) s1@(StreamCons x0 xs) s2@(StreamCons y0 ys) = q
    -- x0: a0, y0: b0, x1: A, s2: B, xs: A', ys: B', q: Q
    -- or ... (s1 - (s1 / s2) * ys)
    where q = StreamCons (x0 `div` y0) $ streamMap (`div` y0) (s1 - q * ys)

{-
Consider representing the Fibonacci numbers using a generating
function,
F(x) = F0 + F1x + F2x^2 + F3x^3 + . . .
Thus x = F(x) − xF(x) − x^2F(x), and solving for F(x) we find that

F(x) = x / (1 − x − x^2)

It turns out that it is possible to compute the nth Fibonacci number
with only O(log n) (arbitrary-precision) arithmetic operations. This
section explains one way to do it.

F =
"
1 1
1 0
"
Notice what happens when we take successive powers of F (see
http://en.wikipedia.org/wiki/Matrix_multiplication if you
forget how matrix multiplication works)
The point is that exponentiation can be implemented in logarithmic
time using a binary exponentiation algorithm. The idea is that to
compute x^n
, instead of iteratively doing n multiplications of x, we compute
even n | x^n = (x^(n `div` 2))^2
odd n  | x^n = x * (x^((n - 1) `div` 2))
The punchline is that Haskell’s exponentiation operator (^) already
uses this algorithm, so we don’t even have to code it ourselves!
-}

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)

{-
Create a type Matrix which represents 2 × 2 matrices of Integers.
• Make an instance of the Num type class for Matrix. In fact, you only
have to implement the (*) method, since that is the only one we
will use. (If you want to play around with matrix operations a bit
more, you can implement fromInteger, negate, and (+) as well.)
-}

{-
• We now get fast (logarithmic time) matrix exponentiation for free,
since (^) is implemented using a binary exponentiation algorithm
in terms of (*). Write a function
fib4 :: Integer -> Integer
which computes the nth Fibonacci number by raising F to the nth
power and projecting out Fn (you will also need a special case
for zero). Try computing the one millionth or even ten millionth
Fibonacci number
-}

-- see here: https://wiki.haskell.org/Prelude_extensions#Matrices
-- Data.Matrix: https://hackage.haskell.org/package/matrix-0.3.4.4/docs/Data-Matrix.html

newtype Matrix a = Matrix [[a]] deriving (Eq, Show)

-- note that !! takes an Int (not Integer)
get :: Int -> Int -> Matrix a -> a
get i j (Matrix m) = (m !! i) !! j

-- TODO:
