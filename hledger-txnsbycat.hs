#!/usr/bin/env stack
-- stack runghc --verbosity info --package hledger

-- This can be run directly by making it executable, or compiled with
-- ```
-- stack ghc -- hledger-txnsbycat.hs
-- ```
--
-- {-# OPTIONS_GHC -Wno-missing-signatures #-}

import Data.List (find)
import Data.Map (Map, fromListWith, keys, lookup)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.String (String)
import Data.Text (Text, intercalate, isPrefixOf, pack, unpack)
import Data.Text.IO (putStrLn)
import Data.Time.Calendar (showGregorian)
import Data.Tree (Tree, rootLabel, subForest)
import Hledger.Cli hiding (main)
import Text.Printf (printf)
import Prelude hiding (lookup)

------------------------------------------------------------------------------
cmdmode =
  hledgerCommandMode
    ( unlines
        [ "txnsbycat",
          "Generate a report of income and expense transactions grouped by category.",
          " ",
          "Takes standard hledger query flags."
        ]
    )
    []
    [generalflagsgroup1]
    []
    ([], Just $ argsFlag "[QUERY]")

------------------------------------------------------------------------------

main :: IO ()
main = do
  cliopts <- getHledgerCliOpts cmdmode
  withJournalDo cliopts $ \j -> do
    groupedtxnsstatement cliopts j

groupedtxnsstatement :: CliOpts -> Journal -> IO ()
groupedtxnsstatement cliopts j = Data.Text.IO.putStrLn $ showTree txnsByAccount accountTree
  where
    entries = (entriesReport $ reportspec_ cliopts) j
    minimalTxns = mapMaybe txnToMinimal entries
    txnsByAccount = fromListWith (++) (map (\t -> (mtAccount t, [t])) minimalTxns)
    accountTree = accountNameTreeFrom $ keys txnsByAccount

data MinimalTransaction = MinimalTransaction
  { mtDate :: Text,
    mtDescription :: Text,
    mtAccount :: Text,
    mtAmount :: MixedAmount
  }

showTree :: Map Text [MinimalTransaction] -> Tree AccountName -> Text
showTree txnsByAccount accountTree = fst $ doShowTree 0 accountTree
  where
    {- Returns a tuple:
     -   fst: The text output for the tree
     -   snd: The sum of the transactions for the subtree
     -}
    doShowTree :: Int -> Tree AccountName -> (Text, MixedAmount)
    doShowTree depth subtree =
      ( intercalate
          newline
          ( [prefixedAccountName]
              {- Don't include transactions at this level if there aren't any; otherwise we get a stray newline. -}
              ++ [transactionsInNodeText | not (null transactionsInNode)]
              {- Same, but for when there aren't any subtrees -}
              ++ [subtreesText | not (null (subForest subtree))]
              ++ [sumOfTransactionsText]
          ),
        sumAtThisLevel
      )
      where
        newline :: Text = pack "\n"
        {- The raw account name; can be used for display and lookups -}
        accountName :: Text = rootLabel subtree
        {- The account name prefixed with the indentation level -}
        prefixedAccountName :: Text = intercalate (pack "") [pack $ replicate (depth * 3) ' ', accountName]
        {- The text representing the transactions at this level of the tree -}
        transactionsInNodeText :: Text = intercalate newline $ map (pack . minimalToStr) (reverse transactionsInNode)
        {- The transactions in this level of the tree -}
        transactionsInNode :: [MinimalTransaction] = fromMaybe [] $ lookup accountName txnsByAccount
        {- The sum of the transactions at this level of the tree -}
        sumOfTxnsInNode :: MixedAmount = sum $ map mtAmount transactionsInNode
        {- The sum of the transactions in all subtrees of this node -}
        sumOfSubaccounts :: MixedAmount = sum $ map snd subtrees
        {- The (text, sum) pair for each subtree of this node -}
        subtrees :: [(Text, MixedAmount)] =
          map (doShowTree (depth + 1)) $ subForest subtree
        {- The text for all subtrees of this node -}
        subtreesText :: Text = intercalate newline (map fst subtrees)
        {- The text representation of the sum of all transactions in this node and all subtrees of this node -}
        sumOfTransactionsText :: Text = pack $ printf "%64s %10s" sumLabel (showMixedAmount sumAtThisLevel)
        {- The sum of all transactions in this node and all subtrees of this node -}
        sumAtThisLevel :: MixedAmount = sumOfSubaccounts + sumOfTxnsInNode
        {- The label to use when displaying the sum at this level -}
        sumLabel :: String =
          if accountName == pack "root"
            then "Total"
            else "Total for " ++ unpack accountName

minimalToStr :: MinimalTransaction -> String
minimalToStr t =
  printf
    {- Initial indentation is arbitrary to allow for a few levels of indent.
     - Ideally we'd calculate this based on the number of indentation levels
     - and the width of the screen. -}
    "            %s  %-40s %10s"
    (unpack $ mtDate t)
    (unpack $ mtDescription t)
    (showMixedAmount $ mtAmount t)

txnToMinimal :: Transaction -> Maybe MinimalTransaction
txnToMinimal t = do
  posting <- incomeOrExpensePosting (tpostings t)
  return $
    MinimalTransaction
      (Data.Text.pack $ showGregorian $ tdate t)
      (tdescription t)
      (paccount posting)
      (pamount posting)

{- Finds the posting in the transaction that corresponds to an Income or Expense account -}
incomeOrExpensePosting :: [Posting] -> Maybe Posting
incomeOrExpensePosting = find (\p -> isPrefixOf (pack "Expenses") (paccount p) || isPrefixOf (pack "Income") (paccount p))
