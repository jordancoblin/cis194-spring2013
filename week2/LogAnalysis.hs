{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1: parse a single line
parseMessage :: String -> LogMessage
parseMessage s = parseMessageHelper (words s)

parseMessageHelper :: [String] -> LogMessage
parseMessageHelper ("I":rest) = LogMessage Info (read (head rest)) (mconcat (drop 1 rest))
parseMessageHelper ("W":rest) = LogMessage Warning (read (head rest)) (mconcat (drop 1 rest))
parseMessageHelper ("E":rest) = LogMessage (Error (read (head rest))) (read (rest !! 1)) (mconcat (drop 2 rest))
parseMessageHelper _ = Unknown "unrecognized char"

-- parseMessageHelper s = case s of 
--     ("I":_) -> LogMessage Info 10 "hi"
--     _ -> Unknown "unrecognized char"