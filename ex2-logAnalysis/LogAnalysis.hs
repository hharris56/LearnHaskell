-- Hunter Harris
-- Log Analysis (hw2)
-- 8 28 2021

module LogAnalysis where

import Log

-- Parses an individual line from a log file
parseMessage :: String -> LogMessage
parseMessage msg = 
  let wordList = words msg in
  case wordList of
       ("I":ts:m)   -> LogMessage Info (read ts) (unwords m)
       ("W":ts:m)   -> LogMessage Warning (read ts) (unwords m)
       ("E":l:ts:m) -> LogMessage (Error (read l)) (read ts) (unwords m)
       _            -> Unknown (unwords wordList)

-- CHECK THIS OUT
-- This is a partial function definition, where parse uses the
-- composition operator (.) to put the result of lines into map.
-- This is more or less what is happeneing
--      parse LFS = map parseMessage (lines LFS)
-- But becuase parse literally is "map parseMessage . lines" we
-- dont even need to write in the variale since keyword 'parse'
-- will be replaced by its definition
parse :: String -> [LogMessage]
parse = map parseMessage . lines

-- Insert a log message into the message tree (binary search tree)
-- earlier Timestamps to left, later Timestamps to right
insert :: LogMessage -> MessageTree -> MessageTree
insert msg@LogMessage{} Leaf = (Node Leaf msg Leaf)
insert msg@(LogMessage _ ts1 _) (Node nl (LogMessage _ ts2 _) nr)
     | ts1 < ts2  = insert msg nl
     | otherwise  = insert msg nr 
insert _ tree = tree

-- build a message tree from a list of messages
build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

-- takes a message tree and generates a list of log messages in
-- the correct order
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node l lm r) = inOrder l ++ [lm] ++ inOrder r

-- accepts a minimum value and a log message
-- returns bool if log message level >= minimum
isSevere :: Int -> LogMessage -> Bool
isSevere minLvl (LogMessage (Error lvl) _ _) = lvl >= minLvl
isSevere _ _ = False

-- extracts message string from a LogMessage
getMesgs :: [LogMessage] -> [String]
getMesgs (LogMessage _ _ msg : msgs) = msg : getMesgs msgs
getMesgs _ = []

-- accepts a list of unsorted messages
-- returns a list of messages with severity >= 50 in order
whatWentWrong :: [LogMessage] -> [String]
--whatWentWrong logList = map getMesg . filter (isSevere 50) . inOrder $ build logList
--whatWentWrong = getMesgs . filter (isSevere 50) . inOrder . build
--TODO: Why is this the order? Why do the above ones compile but not work?
whatWentWrong = getMesgs . inOrder . build . filter (isSevere 50)
