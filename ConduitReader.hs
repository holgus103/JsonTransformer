module ConduitReader where 

import Conduit

testString = "{\"asd\":val,\"asd2\":val2}"


-- run = runConduit $ yieldMany testString .| processUknownStart .| sinkList

processUnknownStart :: Monad m => ConduitM Char Char m ()
processUnknownStart = do
    -- check first character
    val <- peekC 
    case val of 
        Nothing -> return ()
        -- if object beginning detected call processObject
        Just '{' -> processObject
        -- if array beginning detected call processArray
        Just '[' -> processArray
        -- if none of the above keep dropping chars 
        Just x ->  do {takeWhileC (\x -> x /= '[' && x /= '{'); processUnknownStart }

processUnknownValue :: Monad m => ConduitM Char Char m ()
processUnknownValue = do
    val <- peekC 
    case val of
        Nothing -> return ()
        Just '{' -> processObject
        Just '[' -> processArray
        Just ' ' -> do { takeWhileC (==' '); processUnknownValue}
        Just x -> getFieldValue

processArray :: Monad m => ConduitM Char Char m ()
processArray = do
    dropWhileC (/= '[')
    dropC 1


processObject :: Monad m => ConduitM Char Char m ()
processObject = do
    -- consume object start
    dropWhileC (== '{')
    yield '{'
    processField
    -- consume object end
    dropWhileC (== '}')
    yield '}'

processField :: Monad m => ConduitM Char Char m ()
processField =  do
    fieldName <- getFieldName
    yieldMany fieldName
    yield ':'
    processUnknownValue
    val <- peekC
    case val of 
        Nothing -> return ()
        Just x -> if x == '}' then return ()
                  else do {yield ','; processField}

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


    
        
    
    
    
    