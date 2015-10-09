--file LogAnalysis.hs
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

{-
1. Split the given string using the ' ' character as
   delimiter - use the function Prelude.words here.
2. Pattern match based on the first three elements of the splitted list.   
-}
{-
parseMessage :: String -> LogMessage
parseMessage inputString = let splitStrings = splitMessage ' ' inputString
						   in 
						   	case splitStrings of
						   		("I":timeStamp:remainderString) ->	LogMessage Info (read timeStamp) (foldr (++) "" remainderString)

--splitMessage :: Char -> String -> String
splitMessage delimiter inputString = splitMessageHelp delimiter inputString [] []
 where 
	splitMessageHelp delim [] acc ans = reverse ((reverse acc):ans)
	splitMessageHelp delim (delim1:restOfString) acc ans | (delim == delim1) = splitMessageHelp delim restOfString [] ((reverse acc):ans)
	splitMessageHelp delim givenString acc ans = splitMessageHelp delim (tail givenString) ((head givenString):acc) ans
-}
--lots of words and un words , does this perform well ?
--should we write our own function that splits the string into words based on the content ?
parseMessage :: String -> LogMessage
parseMessage inputString = let splitStrings = words inputString
 in
   case splitStrings of
    ("I":timeStamp:remainderString) -> LogMessage Info (read timeStamp) (unwords remainderString)
    ("W":timeStamp:remainderString) -> LogMessage Warning (read timeStamp) (unwords remainderString)
    ("E":errorNum:timeStamp:remainderString) -> LogMessage (Error (read errorNum)) (read timeStamp) (unwords remainderString)
    _ -> Unknown (unwords splitStrings)

parse :: String -> [LogMessage]    
parse inputFile = parseHelper (lines inputFile) []
 where
  parseHelper [] ans = reverse ans
  parseHelper (line:restOfLogs) ans = parseHelper restOfLogs ((parseMessage line):ans )

parse1 :: String -> [LogMessage]  
parse1 inputFile = map parseMessage (lines inputFile)

insert :: MessageTree 
          ->  LogMessage
          -> MessageTree
insert messgTree (Unknown _) = messgTree
insert Leaf lm@(LogMessage _ timeStamp _) = Node Leaf lm Leaf 
insert (Node leftMsgTree lm1@(LogMessage _ timeStamp1 _) rightMsgTree) lm@(LogMessage _ timeStamp _)  | (timeStamp >= timeStamp1)= Node leftMsgTree lm1 (insert rightMsgTree lm)
insert (Node leftMsgTree lm1@(LogMessage _ timeStamp1 _) rightMsgTree) lm@(LogMessage _ timeStamp _)  | (timeStamp < timeStamp1) = Node (insert leftMsgTree lm) lm1 rightMsgTree

--A very nice abstraction of the below is actually a foldl.
build lstLM = buildHelper lstLM Leaf
 where
 	buildHelper [] ans = ans
 	buildHelper (x:xs) ans = buildHelper xs (insert ans x)

--build1 :: [LogMessage] -> MessageTree
build1 lstLM = foldl insert Leaf lstLM 

--can we apply foldt over the MessageTree to get the above function ?
-- What can be done to get the inOrder written with a fold ?
inOrder :: MessageTree -> [LogMessage]	
inOrder Leaf = []
inOrder (Node leftMsgTree lm rightMsgTree) = (inOrder leftMsgTree) ++ [lm] ++ (inOrder rightMsgTree)

--whatWentWrong :: [LogMessage] -> [String]
whatWentWrong lstLogMesg = map (\lm@(LogMessage (Error _) _ remString) -> remString) $ 
						   filter filterErrorMessages $ inOrder . build1 $ lstLogMesg

filterErrorMessages logMessage = case logMessage of
	LogMessage (Error errVal) _ _ | (errVal >= 50) -> True
	_ -> False





 