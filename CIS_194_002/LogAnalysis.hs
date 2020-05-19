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
