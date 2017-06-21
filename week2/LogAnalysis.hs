module LogAnalysis where 
import Log 

{-EXERCISE ONE -}

{-parseMessage "E 2 562 help help"-}
{-== LogMessage (Error 2) 562 "help help"-}

{-parseMessage "I 29 la la la"-}
{-== LogMessage Info 29 "la la la"-}

{-parseMessage "This is not in the right format"-}
{-== Unknown "This is not in the right format"-}

parse:: String -> [LogMessage]
parse string = linesToLogMessage logs 
    where logs = lines string

linesToLogMessage:: [String] -> [LogMessage]
linesToLogMessage (line:rest) = parseString line : linesToLogMessage rest

parseString:: String -> LogMessage
parseString = (instanceLog . splittedWords)
    where splittedWords = words 

instanceLog:: [String] -> LogMessage
instanceLog (typeLog:rest) 
    | (==) typeLog "E" = errorLog rest
    | (==) typeLog "I" = infoLog rest
    | (==) typeLog "W" = warningLog rest
    | otherwise = Unknown (getMessage (typeLog:[]++rest))
 
errorLog:: [String] -> LogMessage
errorLog (errorNumber:timeStamp:rest) = 
   LogMessage (Error (toInt errorNumber)) (toInt timeStamp) (getMessage rest) 
 
infoLog:: [String] -> LogMessage
infoLog (timeStamp:rest) = 
   LogMessage Info (toInt timeStamp) (getMessage rest)

warningLog:: [String] -> LogMessage
warningLog (timeStamp:rest) = 
   LogMessage Warning (toInt timeStamp) (getMessage rest)

getMessage:: [String] -> String
getMessage [] = ""
getMessage (x:[]) = x
getMessage (x:xs) = x ++ " " ++ getMessage xs

toInt:: String -> Int
toInt str = read str

{-EXERCISE TWO-}

getTimeStamp:: LogMessage -> Int
getTimeStamp (LogMessage (Error number) time message) = time 
getTimeStamp (LogMessage Info time message) =  time 
getTimeStamp (LogMessage Warning time message) = time 


insert:: LogMessage -> MessageTree -> MessageTree
insert (Unknown error) tree = tree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage (Node left value right) 
     | (<) (getTimeStamp logMessage) (getTimeStamp value) = Node (insert logMessage left) value right
     | otherwise =  Node left value (insert logMessage right)

{-EXERCISE THREE-}
{-build:: [LogMessage] -> MessageTree-}
build messages  = building messages tree 
    where tree = Leaf
building [] tree = tree 
building (x:xs) tree = building xs (insert x tree)
