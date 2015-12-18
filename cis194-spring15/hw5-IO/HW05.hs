{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

{-
cabal update; cabal install aeson text
-}

module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map

import Parser

import Data.Char -- ord, chr
import Data.Word -- Word8
import Data.Bits -- xor

import qualified Data.List as List (sortBy)

-- Convert a Char to a Word8
c2w :: Char -> Word8
c2w = fromIntegral . ord

-- Convert a Word8 to a Char
w2c :: Word8 -> Char
w2c = chr . fromIntegral

-- Exercise 1 -----------------------------------------
{-
Prelude.readFile can only read file with system locale encoding
Prelude> :t Prelude.readFile
Prelude.readFile :: FilePath -> IO String
So use ByteString.readFile to read a ByteString
let secret = getSecret "dog-original.jpg" "dog.jpg"
-}
getSecret :: FilePath -> FilePath -> IO ByteString
getSecret origPath modPath = do
  bs1 <- BS.readFile origPath
  bs2 <- BS.readFile modPath
  -- to word8 list
  let l1 = BS.unpack bs1
  let l2 = BS.unpack bs2
  -- filter out bytes with value 0 (to get value from Word8 use toInteger)
  let secrets = filter ((/= 0) . toInteger) $ zipWith xor l1 l2
  return $ BS.pack secrets

-- Exercise 2 -----------------------------------------

-- l1 is key's word8 list
-- l2 is encrypted text's word8 list
decrypt :: [Word8] -> [Word8] -> [Word8]
decrypt _ [] = []
decrypt l1 l2 = zipWith xor l1 l2 ++ decrypt l1 (drop (length l1) l2)

-- Test:
-- key <- getSecret "dog-original.jpg" "dog.jpg"
-- let file = "victims.json"
-- decryptWithKey key file
decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key file = do
  let encFile = file ++ ".enc"
  encBS <- BS.readFile encFile
  let decryptedWord8List = decrypt (BS.unpack key) (BS.unpack encBS)

  -- write to output file
  BS.writeFile file (BS.pack decryptedWord8List)

  return ()

-- Exercise 3 -----------------------------------------

-- Test
-- Parse JSON file
-- TId: transaction ID (TId :: String)
-- parseFile "victims.json" :: IO (Maybe [TId])
-- parseFile "transactions.json" :: IO (Maybe [Transaction])
parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file = do
  -- readFile :: FilePath -> IO ByteString
  bs <- BS.readFile file
  -- JSON.parse
  -- decode :: FromJSON a => ByteString -> Maybe a
  return $ decode bs

-- filter maybeTransactions with maybeVictims
-- Extract from Maybe: pattern matching; or Data.Maybe.fromJust, Data.Maybe.fromMaybe?
-- find if transaction's id is in victim ids: Data.Set?
findBadTs :: Foldable t => Maybe (t TId) -> Maybe [Transaction] -> Maybe [Transaction]
findBadTs (Just victims) (Just transactions) = Just $ filter (\transaction -> tid transaction `elem` victims) transactions
findBadTs Nothing (Just tran) = Just tran
findBadTs _ Nothing = Nothing

-- Exercise 4 -----------------------------------------
-- (Just ts) <- getBadTs "victims.json" "transactions.json"
getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile = do
  maybeVictims <- parseFile victimFile :: IO (Maybe [TId])
  maybeTransactions <- parseFile transactionFile :: IO (Maybe [Transaction])
  return $ findBadTs maybeVictims maybeTransactions

-- Exercise 5 -----------------------------------------
{-
   let ts = [ Transaction { from   = "Haskell Curry"
                          , to     = "Simon Peyton Jones"
                          , amount = 10
                          , tid = "534a8de8-5a7e-4285-9801-8585734ed3dc" }]
                         in getFlow ts == Map.fromList [("Haskell Curry", -10), ("Simon Peyton Jones", 10)]
let ts = [Transaction "Haskell Curry" "Simon Peyton Jones" 10 "534a8de8-5a7e-4285-9801-8585734ed3dc"]
-}
compose :: [a -> a] -> a -> a
compose fs v = foldl (flip (.)) id fs $ v

{-
  function to insert or update the map with the transaction amount
-}
updateMapWithT :: Transaction -> Map String Integer -> Map String Integer
updateMapWithT t =
    -- insertWith updateFunction(oldValue -> newValue -> updatedValue), key, defaultValue m
    -- (Map String Integer -> Map String Integer) . (Map String Integer -> Map String Integer)
    Map.insertWith (+) (to t) (amount t) . Map.insertWith (+) (from t) (-(amount t))

{-
  Compose a series of functions to update map
  Test:
    let flow = getFlow ts
-}
getFlow :: [Transaction] -> Map String Integer
-- map takes a transaction, returns a function to update the map
getFlow ts = compose (map updateMapWithT ts) Map.empty

-- Exercise 6 -----------------------------------------

-- TODO: refactor: bindMaybe
wealthierPerson :: String -> String -> Map String Integer -> String
wealthierPerson name1 name2 m = case Map.lookup name1 m of
                                  Nothing -> name2
                                  (Just money1) -> case Map.lookup name2 m of
                                                     Nothing -> name1
                                                     (Just money2) -> if money1 > money2 then name1 else name2

-- wealthierPerson name1 name2 m = do
--   money1 <- Map.lookup name1 m
--   money2 <- Map.lookup name2 m
--   return $ if money1 > money2 then name1 else name2

{-
  get the maximum value's key
  http://stackoverflow.com/questions/19723523/sort-data-map-by-value-and-get-all-biggest-values

  let criminal = getCriminal flow
  ("Shaanan Cohney", 13089)
-}
getCriminal :: Map String Integer -> String
getCriminal m = Map.foldlWithKey (\prev cur curMoney -> (wealthierPerson prev cur m)) "Not found!" m

-- Exercise 7 -----------------------------------------

{-
  Separate the people into payers and payees; ie, people who ended up with extra money and people who ended up at a loss.

  Sort both groups in descending order. The payers who owe the most and the payees who are owed the most should come first. You will likely find the sortBy function in the Data.List module helpful for this stage.

  Iterate over the payers and payees in parallel. For each pair, make a new Transaction where the payer pays the payee the minimum between how much the payer owes and how much the payee is owed. Deduct this amount from both, remove anyone who has completely paid his/her debt or has been completely paid off, and repeat.

=========
  bipartite graph

  import qualified Data.List as List (sortBy) -- http://stackoverflow.com/questions/28017548/haskell-qualified-import-of-a-set-of-functions
    - compare :: a -> a -> Ordering
      - reverse sorted list or
      - sort descending https://groups.google.com/forum/#!topic/comp.lang.haskell/m655Tc4nVjQ
        - descending a b = compare b a
        - descending = flip compare
        - custom compare http://stackoverflow.com/questions/2349798/in-haskell-how-can-i-use-the-built-in-sortby-function-to-sort-a-list-of-pairst

  Test: undoTs m (repeat "")
-}

toListAndSort m = List.sortBy (\a b -> compare (snd b) (snd a)) $ Map.toList m
filterPayer = filter ((>0) . snd)
filterPayee = filter ((<0) . snd)
createTransactionFromPayerPayeeTId triplet = Transaction (fst payer) (fst payee) payment tId
                                        where (payer, payee, tId) = triplet
                                              payment = min (snd payer) ((abs . snd) payee)
-- Deduct this amount from both, remove anyone who has completely paid his/her debt or has been completely paid off
-- f takes the incMoney and returns Nothing (for Map.update to delete if the new amount is 0)
-- or Just newAmount (for Map.update to update)
updateMapWithUndoT :: Transaction -> Map String Integer -> Map String Integer
updateMapWithUndoT t = let f incMoney origMoney = if origMoney + incMoney == 0 then Nothing
                                                        else Just (origMoney + incMoney)
                              in Map.update (f (amount t)) (to t)
                                 . Map.update (f (negate $ amount t)) (from t)

-- filter out zero flow
filterFlow :: Map String Integer -> Map String Integer
filterFlow = Map.filter (/=0)

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs flow tIds = if null flow then [] -- empty map, don't need any undo transaction
                else transactions ++ undoTs updatedFlow nextTIds
                  -- sorted transactions List
                  -- where sortedTs = List.sortBy (\a b -> (flip compare) (snd a) (snd b)) $ Map.toList m
                  where sortedTs = toListAndSort flow
                        payerTs = filterPayer sortedTs
                        payeeTs = filterPayee sortedTs

                        transactions = map createTransactionFromPayerPayeeTId (zip3 payerTs payeeTs tIds)

                        nextTIds = drop (length transactions) tIds
                        updatedFlow = filterFlow $ compose (map updateMapWithUndoT transactions) flow

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
-- writeJSON = undefined
writeJSON file ts = do
  -- convert to JSON object and JSON.stringify
  let json = encode (toJSON ts)
  BS.writeFile file json
  -- return ()

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <-
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
