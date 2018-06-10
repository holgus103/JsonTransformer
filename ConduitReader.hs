module ConduitReader where 

import Conduit
import Operations
import Data.Text
import Debug.Trace
import Flow
import Helpers


linesToChars :: Monad m => ConduitM Text Char m ()
linesToChars = do
    val <- await
    case val of 
        Just x -> do {yieldMany $ unpack x; linesToChars;}
        Nothing -> return (); 

processUnknownStart :: Monad m => [Op] -> CharConduit m
processUnknownStart ops = do
    -- check first character
    val <- peekC 
    case val of 
        Nothing -> return ()
        -- if object beginning detected call processObject
        Just '{' -> processObject ops "{"
        -- if array beginning detected call processArray
        Just '[' -> processArray
        -- if none of the above keep dropping chars 
        Just x ->  do {takeWhileC (\x -> x /= '[' && x /= '{'); processUnknownStart ops}

processUnknownValue :: Monad m => [Op] -> [Char] -> CharConduit m
processUnknownValue ops buf = do
    val <- peekC 
    case val of
        Nothing -> return ()
        Just '{' -> processObject ops buf
        Just '[' -> processArray
        Just ' ' -> do { takeWhileC (==' '); processUnknownValue ops buf}
        Just x -> do {yieldMany buf; processFieldValue;}

processArray :: Monad m => CharConduit m
processArray = do
    dropWhileC (/= '[')
    dropC 1


processObject :: Monad m => [Op] -> [Char] -> CharConduit m
processObject ops buf = do
    -- consume object start
    dropWhileC (== '{')
    processField ops (buf ++ "{")
    -- consume object end
    dropWhileC (== '}')
    yield '}'   



processField :: Monad m => [Op] -> [Char] -> CharConduit m
processField ops buf =  do
    fieldName <- getFieldName
    -- check if field is to be removed
    if notRemovable fieldName ops then do 
        -- write field to output with it's value
        yieldMany buf
        yieldMany fieldName; yield ':'
        processUnknownValue (subrules fieldName ops) buf
        nextField True
    else do
        -- keep droping field
        dropField []
        nextField False
    where

        nextField :: Monad m => Bool -> CharConduit m
        nextField persisted = do         
            val <- peekC
            case val of 
                Nothing -> return ()
                Just x -> if x == '}' then return ()
                        else do {if persisted then  yield ',' else return (); processField ops buf} 

getFieldName :: Monad m => ConduitM Char o m [Char]
getFieldName = do
    -- drop object opening and first quotation mark
    dropWhileC (\x-> x == '{' || x == '\"' || x == ' ')
    -- get field name
    y <- takeWhileC (/='\"') .| sinkList 
    -- drop field name end 
    dropWhileC (\x -> x =='\"' || x == ':' || x == ' ')   
    return y
        
-- simply flushes a field
processFieldValue :: Monad m => CharConduit m
processFieldValue = do
    val <- takeWhileC (\x -> x /= ',' && x /= '}') .| sinkList
    dropWhileC (\x -> x == ',' || x == '}')
    yieldMany val

    
    
    