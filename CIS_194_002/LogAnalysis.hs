{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where
import Log

stringToInt :: String -> Int
stringToInt s = read s :: Int

separatedWordsToLogMessage :: [String] -> LogMessage
separatedWordsToLogMessage ("E" : level : timestamp : rest) = LogMessage (Error (read level :: Int)) (stringToInt timestamp) (unwords rest)
separatedWordsToLogMessage ("I" : timestamp : rest)         = LogMessage Info (read timestamp) (unwords rest)
separatedWordsToLogMessage ("W" : timestamp : rest)         = LogMessage Warning (read timestamp) (unwords rest)
separatedWordsToLogMessage s                                = Unknown (unwords s)

parseMessage :: String -> LogMessage
parseMessage = separatedWordsToLogMessage . words

parse :: String -> [LogMessage]
parse = (map parseMessage) . lines

insert :: LogMessage -> MessageTree -> MessageTree
insert lm@LogMessage{} Leaf = Node Leaf lm Leaf
insert newLogMessage@(LogMessage _ newTimestamp _) (Node left rootNode@(LogMessage _ rootTimestamp _) right)
    | newTimestamp > rootTimestamp = (Node left rootNode (insert newLogMessage right))
    | otherwise = (Node (insert newLogMessage left) rootNode right)
insert _ tree = tree

build :: [LogMessage] -> MessageTree
build ms = (foldr insert Leaf ms)

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf         = []
inOrder (Node l m r) = (inOrder l) ++ [m] ++ (inOrder r)

hasSeverityGreaterThan :: Int -> LogMessage -> Bool
hasSeverityGreaterThan requiredSeverity (LogMessage (Error s) _ _) = s > requiredSeverity
hasSeverityGreaterThan _ _                        = False

takeLogMessage :: LogMessage -> String
takeLogMessage (LogMessage _ _ m) = m
takeLogMessage _                  = ""

takeSeverity :: LogMessage -> Int
takeSeverity (LogMessage (Error s) _ _) = s
takeSeverity                          _ = 0

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = (map takeLogMessage) . inOrder . build . (filter (hasSeverityGreaterThan 49))