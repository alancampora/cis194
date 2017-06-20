module LogAnalysis where 
import Log 

{-parseMessage "E 2 562 help help"-}
{-== LogMessage (Error 2) 562 "help help"-}

{-parseMessage "I 29 la la la"-}
{-== LogMessage Info 29 "la la la"-}

{-parseMessage "This is not in the right format"-}
{-== Unknown "This is not in the right format"-}

parseString = (instanceLog . splittedWords)
splittedWords = words 

instanceLog (typeLog:rest) 
    | (==) typeLog "E" = errorLog rest
    | (==) typeLog "I" = infoLog rest
    | (==) typeLog "W" = warningLog rest
    | otherwise = Unknown (getMessage (typeLog:[]++rest))
    
errorLog (errorNumber:timeStamp:rest) = 
   LogMessage (Error (toInt errorNumber)) (toInt timeStamp) (getMessage rest) 
 
infoLog (timeStamp:rest) = 
   LogMessage Info (toInt timeStamp) (getMessage rest)

warningLog (timeStamp:rest) = 
   LogMessage Warning (toInt timeStamp) (getMessage rest)

getMessage (x:[]) = x
getMessage (x:xs) = x ++ " " ++ getMessage xs

toInt str = read str
