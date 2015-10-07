{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
parseMessage "E 2 562 help help"
== LogMessage (Error 2) 562 "help help"
parseMessage "I 29 la la la"
== LogMessage Info 29 "la la la"
parseMessage "This is not in the right format"
== Unknown "This is not in the right format"

For example, to convert a String like "562" into an Int, you
can use the read function. Other functions which may (or may not)
be useful to you include lines, words, unwords, take, drop, and (.)

  lines :: String -> [String] Source

  lines breaks a string up into a list of strings at newline characters. The resulting strings do not contain newlines.

  words :: String -> [String] Source

  words breaks a string up into a list of words, which were delimited by white space.

  unlines :: [String] -> String Source

  unlines is an inverse operation to lines. It joins lines, after appending a terminating newline to each.

  unwords :: [String] -> String Source

  unwords is an inverse operation to words. It joins words with separating spaces.

  take :: Int -> [a] -> [a] Source

  take n, applied to a list xs, returns the prefix of xs of length n, or xs itself if n > length xs:

  drop :: Int -> [a] -> [a] Source

  drop n xs returns the suffix of xs after the first n elements, or [] if n > length xs:


read "3" :: Int
> 3
😂
-}

{-
errorFromChar :: Char -> MessageType
errorFromChar c = case c of
  'E' -> Error
  'I' -> Info
  'W' -> Warning
-}

{-
data MessageType = Info
                 | Warning
                 | Error Int
  deriving (Show, Eq)

type TimeStamp = Int

data LogMessage = LogMessage MessageType TimeStamp String
                | Unknown String
  deriving (Show, Eq)
-}
-- data ErrorChar = Char

parseMessage :: String -> LogMessage
parseMessage s = case words s of
  (x: y: zs) -> case x of
    "I" -> LogMessage Info (read y :: Int) (unwords zs)
    "W" -> LogMessage Warning (read y :: Int) (unwords zs)
    "E" -> case length zs of
      0 -> Unknown s
      _ -> LogMessage (Error (read y :: Int)) (read (head zs) :: Int) (unwords (drop 1 zs))
    _ -> Unknown s
  _ -> Unknown s

{-
parseMessage (x: y: zs) |
  x == "I" = LogMessage Info (read y :: Int) (unwords zs)
  ...
  otherwise = Unknown s
parseMessage _ = Unknown s
-}

-- testParse parse 10 "error.log"
parse :: String -> [LogMessage]
parse s = map parseMessage (lines s)


{-
A MessageTree should be sorted by timestamp: that is, the timestamp
of a LogMessage in any Node should be greater than all timestamps
of any LogMessage in the left subtree, and less than all timestamps
of any LogMessage in the right child.

data MessageTree = Leaf
                 | Node MessageTree LogMessage MessageTree
  deriving (Show, Eq)

BST
-}

-- TODO: thoughts: why not case ts of (<rootts), _

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m tree@(Node lt rootNode@(LogMessage _ rootts _) rt) = case m of
  (Unknown _) -> tree
  (LogMessage _ ts _) -> case (ts < rootts) of
    True -> Node (insert m lt) rootNode rt
    False -> Node lt rootNode (insert m rt)
--insert _ (Node _ (Unknown _) _) -- haskell finds out this non-exhausive
insert _ t = t

build :: [LogMessage] -> MessageTree
build [] = Leaf
build (m:ms) = insert m (build ms)  -- like foldr ? build = (foldr \y ys) -> insert y

{-
which takes a sorted MessageTree and produces a list of all the
LogMessages it contains, sorted by timestamp from smallest to biggest.

With these functions, we can now remove Unknown messages and
sort the well-formed messages using an expression such as:
inOrder (build tree)
-}
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node lt m rt) = (inOrder lt) ++ [m] ++ (inOrder rt)

{-
which takes an unsorted list of LogMessages, and returns a list of the
messages corresponding to any errors with a severity of 50 or greater,
sorted by timestamp. (Of course, you can use your functions from the
previous exercises to do the sorting.)

isWrong =
-}
wrongMessages :: [LogMessage] -> [String]
wrongMessages [] = []
wrongMessages (m: ms) = case m of
  LogMessage (Error severity) _ theerror -> case severity > 50 of
    True -> theerror : wrongMessages ms
    False -> wrongMessages ms
  _ -> wrongMessages ms


whatWentWrong :: [LogMessage] -> [String]
whatWentWrong ms = wrongMessages $ inOrder (build ms)
-- testWhatWentWrong parse whatWentWrong "sample.log"

{-
testing
http://stackoverflow.com/questions/15273158/how-do-i-use-a-let-within-a-do-block-in-ghci
use brackets to contain declarations
-}
