module ConduitReader where 

import Conduit
import Operations

-- disperse :: Monad m => ConduitM Text Char m ()
-- disperse = do
--     yieldMany 

-- run = runConduit $ yieldMany testString .| processUknownStart .| sinkList

type CharConduit m = ConduitM Char Char m ()

processUnknownStart :: Monad m => [Op] -> CharConduit m
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

processUnknownValue :: Monad m => [Op] -> CharConduit m
processUnknownValue ops = do
    val <- peekC 
    case val of
        Nothing -> return ()
        Just '{' -> processObject ops
        Just '[' -> processArray
        Just ' ' -> do { takeWhileC (==' '); processUnknownValue ops}
        Just x -> getFieldValue

processArray :: Monad m => CharConduit m
processArray = do
    dropWhileC (/= '[')
    dropC 1


processObject :: Monad m => [Op] -> CharConduit m
processObject ops = do
    -- consume object start
    dropWhileC (== '{')
    yield '{'
    processField ops
    -- consume object end
    dropWhileC (== '}')
    yield '}'   

processField :: Monad m => [Op] -> CharConduit m
processField ops =  do
    fieldName <- getFieldName
    if all (\x -> case x of
                -- check if field needs to be removed
                Removal [name] -> name /= fieldName 
                _ -> True
            ) ops then do 
                -- write field to output with it's value
                { yieldMany fieldName; yield ':'; processUnknownValue ops; nextField True;} 
                else do
                -- keep droping field
                {dropField; nextField False}
    where
        nextField :: Monad m => Bool -> CharConduit m
        nextField persisted = do         
            val <- peekC
            case val of 
                Nothing -> return ()
                Just x -> if x == '}' then return ()
                        else do {if persisted then  yield ',' else return (); processField ops} 

getFieldName :: Monad m => ConduitM Char o m [Char]
getFieldName = do
    -- drop object opening and first quotation mark
    dropWhileC (\x-> x == '{' || x == '\"' || x == ' ')
    -- get field name
    y <- takeWhileC (/='\"') .| sinkList 
    -- drop field name end 
    dropWhileC (\x -> x =='\"' || x == ':' || x == ' ')   
    return y
        
getFieldValue :: Monad m => CharConduit m
getFieldValue = do
    val <- takeWhileC (\x -> x /= ',' && x /= '}') .| sinkList
    dropWhileC (\x -> x == ',' || x == '}')
    yieldMany val

--"{\"b\":{\"name\":Jan, \"surname\": {\"v\":Kowalski}}, \"c\": c}"
dropField :: Monad m => CharConduit m
dropField = do    
    dropWhileC (\x ->x /= '{' && x /= '[' && x /= ',' && x /= '}') 
    val <- takeC 1 .| sinkList
    case val of
        "{" -> keepDropping '}'
        "[" -> keepDropping ']'
        -- done dropping
        "," -> return ()
        "}" -> return ()
    where
        keepDropping :: Monad m => Char -> CharConduit m
        keepDropping v = do
            dropTillFound
            val <- takeC 1 .| sinkList
            case val of
                "{" -> do {keepDropping '}'; dropTillFound;}
                "[" -> do {keepDropping ']'; dropTillFound;}
                v -> return ()
            where 
                dropTillFound :: Monad m => CharConduit m
                dropTillFound = dropWhileC (\x -> not $ elem x (v:"{[") )



    
        
    
    
    
    