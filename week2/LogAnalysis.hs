{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1: parse a single line
-- TODO: refactor to not need helper + use case statements
parseMessage :: String -> LogMessage
parseMessage s = parseMessageHelper (words s)

parseMessageHelper :: [String] -> LogMessage
parseMessageHelper ("I":rest) = LogMessage Info (read (head rest)) (unwords (drop 1 rest))
parseMessageHelper ("W":rest) = LogMessage Warning (read (head rest)) (unwords (drop 1 rest))
parseMessageHelper ("E":rest) = LogMessage (Error (read (head rest))) (read (rest !! 1)) (unwords (drop 2 rest))
parseMessageHelper x = Unknown (unwords x)

parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- parseHelper :: [String] -> [LogMessage]
-- parseHelper strs = map parseMessage strs
-- parseMessageHelper s = case s of 
--     ("I":_) -> LogMessage Info 10 "hi"
--     _ -> Unknown "unrecognized char"