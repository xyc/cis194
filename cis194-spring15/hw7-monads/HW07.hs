{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------
{-
This function lifts a regular function into a monad and applies it to an argument in that monad.

Example: liftM (+1) (Just 5) == Just 6

-- x is a monadic value
-- expected type:
liftM f x :: m b
liftM f x = (x >>= (return . f))

liftM f = \x -> x >>= (return . f)

liftM f :: m a -> m b
x :: m a
f :: a -> b
return :: a -> m a
(>>=) :: m a -> (a -> m b) -> (m b)
-}
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f x = x >>= (return . f)

{-
takes in two indices and swaps the elements at those indices in
some Vector. This function should use the safe indexing operation
(!?) and not the unsafe one (!). If either of the indices are out of
bounds (ie (!?) returns Nothing), then the result should be Nothing.

(!?) :: Vector v a => v a -> Int -> Maybe a

-- Bulk updates
(//) :: Vector v a
     => v a         initial vector (of length m)
     -> [(Int, a)]  list of index/value pairs (of length n)
     -> v a function
<5,9,2,7> // [(2,1),(0,3),(2,8)] = <3,9,8,7>
V.fromList([5,9,2,7]) // [(2,1),(0,3),(2,8)]

Example: swapV 0 2 (V.fromList [1, 2, 3]) == Just (V.fromList [3, 2, 1])
Example: swapV 0 2 (V.fromList [1, 2]) == Nothing

v !? i :: Maybe a
v !? j :: Maybe a

liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
liftM3 :: Monad m => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r

vi :: a
vj :: a
\vi vj -> v // [(i, vj), (j, vi)] :: a -> a -> Vector a
\vi vj -> v // [(i, vj), (j, vi)]

liftM2 (\vi vj -> v // [(i, vj), (j, vi)]) :: Maybe a -> Maybe a -> Maybe (Vector a)
liftM2 (\vi vj -> v // [(i, vj), (j, vi)])
-}
swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i j v = liftM2 (\vi vj -> v // [(i, vj), (j, vi)]) (v !? i) (v !? j)

-- Exercise 2 -----------------------------------------

{-
Implement the function mapM that maps a monadic function
across a list

Example: mapM Just [1..10] == Just [1..10]

f :: (a -> m b)
xs :: [a]

http://programmers.stackexchange.com/questions/226599/mapping-a-list-of-optional-values-to-an-optional-list-of-values

TODO:
-- sequence takes every monadiac value out of the list
sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) = do
  a  <- ma
  as <- sequence mas
  return (a:as)

  -- ma >>= \a ->
  --   sequence mas >>= \as ->
  --     return (a:as)

-- or
sequence :: Monad m => [m a] -> m [a]
sequence ms = foldr k (return []) ms
            where
              k m m' = do { x <- m; xs <- m'; return (x:xs) }
-}
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

{-
use mapM to define a function that takes in a list of indices
and a Vector and returns a list of the elements at those indices in the
Maybe monad. If any of the indices don’t exist, the function should
return Nothing. Again, use the safe indexing operator (!?).

Example: getElts [1,3] (V.fromList [0..9]) == Just [1, 3]
-}
getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (\i -> v !? i) is

-- Exercise 3 -----------------------------------------
{-
In the same way that IO actions are not actual computations, but
rather descriptions of computations, functions in the Rnd monad are
descriptions of randomized computations. To evaluate a randomized
function, we can use:
evalRandIO :: Rnd a -> IO a

To get a random value, use of the following functions:
getRandom :: Random a => Rnd a
getRandomR :: Random a => (a, a) -> Rnd a
-}

{-
Use the randomness monad to produce a random element
of a Vector. This function should only return Nothing if the
Vector has length 0

evalRandIO $ getRandomR (0,4)

let v = V.fromList ([1,2,3,4,5])
evalRandIO $ randomElt v
-}
type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
-- randomElt v = do
--   let n = V.length v
--   randomIndex <- getRandomR (0, n - 1)
--   return $ if n > 0 then
--               v !? randomIndex
--            else
--               Nothing
randomElt v
  | n == 0 = return Nothing
  | otherwise = (v!?) <$> getRandomR (0, n - 1) -- fmap
    where n = V.length v

-- Exercise 4 -----------------------------------------

{-
These functions should produce vectors of the specified length that
are populated with random elements. The elements of randomVec n
can have any value of type a whereas the ones from randomVecR n (lo, hi)
should all be within the specified range. Try to make use of functions
from the Control.Monad module.

replicate :: Int -> a -> Vector a
generate :: Int -> (Int -> a) -> Vector a

getRandom :: Rnd a

-- V.generate n
Int -> (a -> m b) -> Vector a

Test:
evalRandIO $ randomVec 5
-}
randomVec :: Random a => Int -> Rnd (Vector a)
-- randomVec n =
-- randomVec n = do
--   let vM = V.generate n (const getRandom) -- \i -> getRandom
--   sequence vM
randomVec n = V.replicateM n getRandom

{-
evalRandIO $ randomVecR 5 (0,3)
-}
randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
-- randomVecR n range = sequence $ V.generate n (const (getRandomR range))
randomVecR n range = V.replicateM n (getRandomR range)

-- Exercise 5 -----------------------------------------

{-
The Fisher-Yates algorithm shuffles the elements of an
array such that each element is equally likely to end up in each position.
The algorithm is defined as follows: for i = n − 1 down to 1,
choose a random j ∈ {0, 1, . . . , i} and swap the elements at positions
i and j. In order to save yourself some hassle, you can use the unsafe
indexing operator (!), but only use it if you are sure that it won’t
fail!
-}

swapVUnsafe :: Int -> Int -> Vector a -> Vector a
swapVUnsafe i j v = (\vi vj -> v // [(i, vj), (j, vi)]) (v ! i) (v ! j)

{-
  evalRandIO $ shuffleIndex  (V.fromList ([1,2,3,4,5])) 4
-}
shuffleIndex :: Vector a -> Int -> Rnd (Vector a)
shuffleIndex v i = do
  j <- getRandomR (0, i)
  return $ swapVUnsafe i j v

{-
shuffleIndex :: Vector a -> Int -> Rnd (Vector a)

shuffleIndex v :: Int -> Rnd (Vector a)

shuffleIndex v (n - 1) :: Rnd (Vector a)
shuffleIndex v (n - 2) :: Rnd (Vector a)
...
shuffleIndex v 1 :: Rnd (Vector a)

-}

{-
  (>>=) :: m a -> (a -> m b) -> m b
  (>>=) :: Rnd (Vector a) -> (Vector a -> Rnd (Vector a)) -> Rnd (Vector a)
  shuffleIndex v n :: Rnd (Vector a)
  shuffleR (n - 1) :: Vector a -> Rnd (Vector a)
-}
-- recursive shuffle
-- shuffleR :: Int -> Vector a -> Rnd (Vector a)
-- shuffleR 0 v = return v
-- shuffleR n v = shuffleIndex v n >>= shuffleR (n - 1)

{-
shuffleIndex v (n - 1) >>= ((shuffleIndex v (n - 2)) >>= .... >>= (shuffleIndex v 1 >>= return v)...

Test:
  evalRandIO $ shuffle (V.fromList([1,2,3,4,5]))
-}
shuffle :: Vector a -> Rnd (Vector a)
shuffle v'' = shuffleR (V.length v'' - 1) v''
  -- shuffleR :: Int -> Vector a -> Rnd (Vector a)
  where shuffleR n v | n == 0 = return v
                     | otherwise = do
                        j <- getRandomR (0, n)
                        let v' = swapVUnsafe n j v
                        -- v' <- shuffleIndex v n
                        shuffleR (n - 1) v'

-- Exercise 6 -----------------------------------------

{-

- The first argument is the Vector to be partitioned
- and the second argument is the index of the pivot
- The output is a 3-tuple containing a Vector of the elements less than the value of the pivot, the value of the pivot itself, and a Vector of the elements greater than or equal to the pivot in that order.

partitionAt (V.fromList [5, 2, 8, 3, 6, 1]) 3 == (V.fromList [2, 1], 3, V.fromList [5, 8, 6])
-}
partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v pivotIndex = (V.filter (< pivot) v, pivot, V.filter (> pivot) v)
  where pivot = v ! pivotIndex

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

{-
Monad Comprehensions?
qsort $ V.fromList([5,4,3,2,1])
-}
qsort :: Ord a => Vector a -> Vector a
qsort v | V.length v == 0 = V.empty
        | otherwise = V.concat [qsort left, V.singleton pivot, qsort right]
          where (left, pivot, right) = partitionAt v 0


-- Exercise 8 -----------------------------------------

{-
sorting V.fromList (reverse [1..10000]), the integers from 1 to 10,000 in
reverse order, using both implementations:

> let v = V.fromList $ reverse [1..10000]
> qsort v
> evalRandIO $ qsortR v
-}
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v | n == 0 = return v
         | otherwise = do
            pivotIndex <- getRandomR (0, n - 1)
            let (left, pivot, right) = partitionAt v pivotIndex
            -- left :: Vector a
            -- right :: Vector a
            left' <- qsortR left
            right' <- qsortR right
            return $ V.concat [left', V.singleton pivot, right']
              where n = V.length v

-- Exercise 9 -----------------------------------------

-- Selection
{-
(quick select)
The purpose of this algorithm is to select the element with rank i in an unsorted array.

For example,
- select 0 v selects the minimum element
- select (n - 1) v selects the maximum element,
- and select (n ‘div‘ 2) v selects the median.

let v = V.fromList ([1..5])
let n = V.length v
evalRandIO $ select 0 v  -- Just 1
evalRandIO $ select (n - 1) v -- Just 5
evalRandIO $ select (n `div` 2) v -- Just 3
-}
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select i v
  --rank that is asked for may be outside the bounds of the Vector
  | i < 0 || i > n - 1 = return Nothing
  | otherwise = do
    pivotIndex <- getRandomR (0, n - 1)
    let (left, pivot, right) = partitionAt v pivotIndex
    let nLeft = V.length left
    -- If i < |L|, then you know that the element you are looking for is in L, so you should recurse on L
    if i < nLeft then select i left
    -- If i = |L|, then p must have rank i
    else if i == nLeft then return (Just pivot)
    -- Finally, if i > |L|, the element must be in R, so you should recurse on R searching for rank i − |L| − 1
    else select (i - nLeft - 1) right
      where n = V.length v

-- Exercise 10 ----------------------------------------
{-
get a deck that contains all of the cards grouped by suit (Spade, Heart, Club, Diamond),
 and arranged from Two to Ace

 V.fromList([Card Two Spade, Card Three Spade, ..., Card Ace Spade,
            Card Two Heart, Card Three Heart, ..., Card Ace Heart,
            Card Two Club, Card Three Club, ..., Card Ace Club,
            Card Two Diamond, Card Three Diamond, ..., Card Ace Diamond])

output is a Vector of Cards arranged in the order

data Label deriving Enum
toEnum :: Int -> a
fromEnum :: a -> Int
toEnum 0 :: Label

You should implement allCards as a Monad Conprehension. You
will probably find suits and labels in the Cards module useful.

Test:
  allCards
-}
allCards :: Deck
--order by label then by suit
--allCards = V.fromList [Card (toEnum label) suit | label <- [0..12], suit <- [Spade, Heart, Club, Diamond]]
allCards = V.fromList [Card (toEnum label) suit | suit <- [Spade, Heart, Club, Diamond], label <- [0..12]]

{-
This should return a new Deck that contains all of the cards from allCards, but in a random order

shuffle :: Vector a -> Rnd (Vector a)

Test:
  evalRandIO newDeck
-}
newDeck :: Rnd Deck
newDeck = shuffle allCards

-- Exercise 11 ----------------------------------------

{-
We also need some sort of uncons’ing operation for Decks. That is, a function that takes in a Deck and gives you the head
and tail (since we can’t pattern match on Vector like we can with lists).

This function takes in a Deck and returns the head and tail of the
Deck in the Maybe monad. If the Deck is empty, it should return
Nothing.

Test:
  nextCard allCards
-}
nextCard :: Deck -> Maybe (Card, Deck)
nextCard v | V.length v == 0 = Nothing
           | otherwise = Just (V.head v, V.tail v)

-- Exercise 12 ----------------------------------------
{-
This function should draw n cards from the given Deck where n is the first input to the function
Try to make use of nextCard and the Maybe monad in order to avoid pattern matching on Maybe.
If the Deck has fewer than n Cards remaining then this function should return Nothing.

Test:
  getCards 5 allCards
-}
getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards n v | V.length v < n = Nothing
             | n == 1 = Just ([V.head v], V.tail v)
             | otherwise = do
                -- use maybe monad (if one fails return nothing)
                (vHead, vTail) <- nextCard v
                (cards, d) <- getCards (n - 1) vTail
                return (vHead : cards, d)

-- Exercise 13 ----------------------------------------
{-
Test:
  main
-}
data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
