module Scrabble where

import Data.Char

data Score = Score Int deriving (Eq, Show)

instance Num Score where
  (+) (Score a) (Score b) = Score (a + b)

instance Monoid Score where
  mempty = Score 0
  mappend (Score a) (Score b) = Score (a + b)

{-
char-score mapping
-}
score :: Char -> Score
score c = getScore (lookup c scoreMapping)
  where
    -- each item -> list, then concat the lists
    -- [('a', 1), ('e', 1)...]
    scoreMapping = foldl (\x y -> x ++ [(ch, snd y) | ch <- fst y]) []
      [("aeilnorstuAEILNORSTU", 1), ("dgDG", 2), ("bcmpBCMP", 3), ("fhvwyFHVWY", 4), ("kK", 5), ("jxJX", 8), ("qzQZ", 10)]
    getScore (Just s) = Score s
    getScore Nothing = Score 0

{-
mapping to scores, then add it up
-}
scoreString :: String -> Score
scoreString = foldl (+) (Score 0) . map score
