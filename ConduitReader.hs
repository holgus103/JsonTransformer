module ConduitReader where 

import Conduit
import Operations

-- disperse :: Monad m => ConduitM Text Char m ()
-- disperse = do
--     yieldMany 

-- run = runConduit $ yieldMany testString .| processUknownStart .| sinkList

processUnknownStart :: Monad m => [Op] -> ConduitM Char Char m ()
processUnknownStart ops = do
    -- check first character
    val <- peekC 
    case val of 
        Nothing -> return ()
        -- if object beginning detected call processObject
        Just '{' -> processObject ops
        -- if array beginning detected call processArray
        Just '[' -> processArray
        -- if none of the above keep dropping chars 
        Just x ->  do {takeWhileC (\x -> x /= '[' && x /= '{'); processUnknownStart ops}

processUnknownValue :: Monad m => [Op] -> ConduitM Char Char m ()
processUnknownValue ops = do
    val <- peekC 
    case val of
        Nothing -> return ()
        Just '{' -> processObject ops
        Just '[' -> processArray
        Just ' ' -> do { takeWhileC (==' '); processUnknownValue ops}
        Just x -> getFieldValue

processArray :: Monad m => ConduitM Char Char m ()
processArray = do
    dropWhileC (/= '[')
    dropC 1


processObject :: Monad m => [Op] -> ConduitM Char Char m ()
processObject ops = do
    -- consume object start
    dropWhileC (== '{')
    yield '{'
    processField ops
    -- consume object end
    dropWhileC (== '}')
    yield '}'   

processField :: Monad m => [Op] -> ConduitM Char Char m ()
processField ops =  do
    fieldName <- getFieldName
    -- if all (\x -> case x of
    --             Removal [fieldName] -> False
    --             _ -> True
    --         ) ops then do { yieldMany fieldName; yield ':'; processUnknownValue ops;} else return ()
    yieldMany fieldName
    yield ':'
    processUnknownValue ops;
    val <- peekC
    case val of 
        Nothing -> return ()
        Just x -> if x == '}' then return ()
                  else do {yield ','; processField ops} 

getFieldName :: Monad m => ConduitM Char o m [Char]
getFieldName = do
    -- drop object opening and first quotation mark
    dropWhileC (\x-> x == '{' || x == '\"' || x == ' ')
    -- get field name
    y <- takeWhileC (/='\"') .| sinkList 
    -- drop field name end 
    dropWhileC (\x -> x =='\"' || x == ':' || x == ' ')   
    return y
        
getFieldValue :: Monad m => ConduitM Char Char m ()
getFieldValue = do
    val <- takeWhileC (\x -> x /= ',' && x /= '}') .| sinkList
    dropWhileC (\x -> x == ',' || x == '}')
    yieldMany val


    
        
    
    
    
    