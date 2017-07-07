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
linesToLogMessage [] = []
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
build:: [LogMessage] -> MessageTree
build messages  = building messages tree 
    where tree = Leaf
building [] tree = tree 
building (x:xs) tree = building xs (insert x tree)

{-EXCERCISE FOUR -}
tree= ["I 6 Completed armadillo processing", "I 1 Nothing to report", "E 99 10 Flange failed!", "I 4 Everything normal","I 11 Initiating self-destruct sequence", "E 70 3 Way too many pickles", "E 65 8 Bad pickle-flange interaction detected", "W 5 Flange is due for a check-up", "I 7 Out for lunch, back in two time steps", "E 20 2 Too many pickles", "I 9 Back from lunch"]

inOrder:: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node messageTreeLeft logMessage messageTreeRight) = 
    inOrder messageTreeLeft ++ logMessage:[] ++ inOrder messageTreeRight

{-testing function inOrder-}
createOrderedTree:: [String] -> [LogMessage]
createOrderedTree = inOrder . build . linesToLogMessage
