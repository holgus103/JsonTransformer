module ConduitReader where 

import Conduit
import Enums
import Data.Text
import Debug.Trace
import Flow
import Helpers
import Control.Monad

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
        Just '{' -> void $ processObject ops ""
        -- if array beginning detected call processArray
        Just '[' -> void $ processArray
        -- if none of the above keep dropping chars 
        Just x ->  do {takeWhileC (\x -> x /= '[' && x /= '{'); void $ processUnknownStart ops}

processUnknownValue :: Monad m => [Op] -> [Char] -> ContainerConduit m
processUnknownValue ops buf = do
    val <- peekC 
    case val of
        Nothing -> return Empty
        Just '{' -> processObject ops buf
        Just '[' -> processArray
        Just ' ' -> do { takeWhileC (==' '); processUnknownValue ops buf}
        Just x -> do {yieldMany buf; processFieldValue; return NonEmpty}

processArray :: Monad m => ContainerConduit m
processArray = do
    dropWhileC (/= '[')
    dropC 1
    return Empty


processObject :: Monad m => [Op] -> [Char] -> ContainerConduit m
processObject ops buf = do
    -- consume object start
    dropWhileC (== '{')
    res <- processField ops (buf ++ "{") $ trace "consumed { of object" Empty
    -- consume object end
    case res of
        Empty -> return $ trace "object got empty" Empty
        -- if the buffered values were flushed - the object is not empty
        NonEmpty -> do {yield '}'; return $ trace "object got nonempty" NonEmpty;}



processField :: Monad m => [Op] -> [Char] -> ConduitResult -> ContainerConduit m
processField ops buf res = do     
    val <- peekC 
    case val of     
        Nothing -> return res
        Just x -> 
            -- object end found, flush and return if level is empty
            if x == '}' then do
                yieldMany buf
                return res
            else do
                fieldName <- getFieldName
                -- check if field is to be removed
                if notRemovable fieldName ops then do 
                    case res of
                        -- nothing has been flushed before
                        Empty -> do
                            val <- processUnknownValue (subrules fieldName ops) (buf ++ fieldName ++ ":")
                            case val of
                                Empty -> processField ops buf Empty
                                NonEmpty -> processField ops "" NonEmpty
                        -- non empty 
                        NonEmpty -> do
                            yieldMany buf
                            processUnknownValue (subrules fieldName ops) (',':fieldName ++ ":")
                            processField ops "" NonEmpty
                else do
                    -- drop field
                    dropField $ trace ("dropping field " ++ fieldName ++ " buffer: " ++ buf) []
                    processField ops buf res

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

    
    
    