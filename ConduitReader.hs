module ConduitReader where 

import Conduit

testString = "{\"asd\":val,\"asd2\":val2}"


-- run = runConduit $ yieldMany testString .| processObject .| sinkList

getFieldName :: Monad m => ConduitM Char o m [Char]
getFieldName = do
    -- drop object opening and first quotation mark
    dropWhileC (\x-> x == '{' || x == '\"' || x == ' ')
    -- get field name
    y <- takeWhileC (/='\"') .| sinkList 
    -- drop field name end 
    dropWhileC (\x -> x =='\"' || x == ':' || x == ' ')   
    return y

processObject :: Monad m => ConduitM Char Char m ()
processObject = do
    -- consume object start
    dropWhileC (== '{')
    yield '{'
    processField
    -- consume object end
    dropWhileC (== '}')
    yield '}'

    

getFieldValue :: Monad m => ConduitM Char o m [Char]
getFieldValue = do
    val <- takeWhileC (\x -> x /= ',' && x /= '}') .| sinkList
    dropWhileC (\x -> x == ',' || x == '}')
    return val

processField :: Monad m => ConduitM Char Char m ()
processField =  do
    fieldName <- getFieldName
    fieldValue <- getFieldValue
    yieldMany fieldName
    yield ':'
    yieldMany fieldValue
    val <- peekC
    case val of 
        Nothing -> return ()
        Just x -> if x == '}' then return ()
                  else do {yield ','; processField}
        


    
        
    
    
    
    