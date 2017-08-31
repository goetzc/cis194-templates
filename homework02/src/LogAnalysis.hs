{-# OPTIONS_GHC -Wall #-}

----------------------------------------------------------------------
-- |
--
-- CIS 194 Spring 2013: Homework 02
--
----------------------------------------------------------------------

module LogAnalysis where

import Log

----------------------------------------------------------------------
-- Exercise 1
----------------------------------------------------------------------

-- |
--
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"

parseMessage :: String -> LogMessage
parseMessage message = case words message of
    ("I":time:xs)   -> LogMessage Info (read time) (unwords xs)
    ("W":time:xs)   -> LogMessage Warning (read time) (unwords xs)
    ("E":n:time:xs) -> LogMessage (Error $ read n) (read time) (unwords xs)
    other           -> Unknown $ unwords other

parse :: String -> [LogMessage]
-- parse messages = ((map parseMessage) . lines) messages
parse messages = map parseMessage $ lines messages

----------------------------------------------------------------------
-- Exercise 2
----------------------------------------------------------------------

-- |
--
-- >>>
--

insert :: LogMessage -> MessageTree -> MessageTree
insert m Leaf = Node Leaf m Leaf
insert m@(LogMessage _ t _) (Node left m2@(LogMessage _ t2 _) right)
    | t < t2    = Node (insert m left) m2 right
    | otherwise = Node left            m2 (insert m right)
insert _ tree = tree

----------------------------------------------------------------------
-- Exercise 3
----------------------------------------------------------------------

-- |
--
-- >>>
--

build :: [LogMessage] -> MessageTree
-- build [] = Leaf
-- build (m:xs) = insert m (build xs)
build = foldr insert Leaf

----------------------------------------------------------------------
-- Exercise 4
----------------------------------------------------------------------

-- |
--
-- >>>
--

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left m right) = inOrder left ++ [m] ++ inOrder right

----------------------------------------------------------------------
-- Exercise 5
----------------------------------------------------------------------

-- |
--
-- >>>
--

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = extractMsg . sortByTimestamp . filter relevant

relevant :: LogMessage -> Bool
relevant (LogMessage (Error n) _ _)
    | n >= 50   = True
    | otherwise = False
relevant _ = False

sortByTimestamp :: [LogMessage] -> [LogMessage]
sortByTimestamp = inOrder . build

extractMsg :: [LogMessage] -> [String]
extractMsg (LogMessage _ _ s : xs) = s : extractMsg xs
extractMsg _ = []

----------------------------------------------------------------------
-- Exercise 6 (Optional)
----------------------------------------------------------------------

whoDidIt :: String
whoDidIt = undefined
