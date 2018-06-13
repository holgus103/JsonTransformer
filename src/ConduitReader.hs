-- | This module contains several conduits used for parsing parts or whole JSON data
module ConduitReader where 

import Conduit
import Enums
import Data.Text  
import Flow
import Helpers
import Control.Monad
import Data.List

-- | Conduit splitting lines into separate chars.
linesToChars :: Monad m => ConduitM Text Char m ()
linesToChars = 
    await
    >>= (\val ->
        case val of 
            Just x -> (yieldMany $ unpack x) >> linesToChars
            Nothing -> return (); 
    )

-- | Starts all parsing of various data.
processUnknownStart :: Monad m => [Op] -> CharConduit m
processUnknownStart ops = do
    -- check first character
    dropWhileC (==' ')
    >> peekC 
    >>= (\val ->
    case val of 
        Nothing -> return ()
        -- if object beginning detected call processObject
        Just '{' -> void $ processObject ops ""
        -- if array beginning detected call processArray
        Just '[' -> void $ processArray ops ""
        -- if none of the above keep dropping chars 
        Just x ->  takeWhileC (\x -> x /= '[' && x /= '{') >> (void $ processUnknownStart ops)
    )

-- | Processes an unknown value within an array or object.
processUnknownValue :: Monad m => [Op] -> [Char] -> ContainerConduit m
processUnknownValue ops buf = 
    peekC 
    >>= (\val -> case val of
        Nothing -> return Empty
        Just '{' -> processObject ops buf
        Just '[' -> processArray ops buf
        Just ' ' -> takeWhileC (==' ') >> processUnknownValue ops buf
        Just x -> yieldMany buf >> processFieldValue >> return NonEmpty
    )

-- | Processes an whole array.
processArray :: Monad m => [Op] -> [Char] -> ContainerConduit m
processArray ops buf = 
    dropWhileC (/= '[')
    >> dropC 1
    >> processArrayElement ops (buf ++ "[") 0 Empty
    >>= (\res -> 
        case res of 
            Empty -> return Empty
            NonEmpty -> yield ']' >> return NonEmpty
        )

-- | Processes an object.
processObject :: Monad m => [Op] -> [Char] -> ContainerConduit m
processObject ops buf =
    -- consume object start
    dropWhileC (== '{')
    >> processField ops (buf ++ "{") Empty
    -- consume object end
    >>= (\res -> case res of
        Empty -> 
            return Empty
        -- if the buffered values were flushed - the object is not empty
        NonEmpty -> 
            yield '}' 
            >> return NonEmpty
        )


-- | Executes direct assignments for an object and adds them at the end of it.
flushAdditions :: Monad m => ConduitResult -> [Op] -> ContainerConduit m
flushAdditions res ops = 
    case ops of
        [] -> return res
        _  -> case res of
                Empty -> 
                    Data.List.map (\(AddD [name] val) -> ("\"" ++ name ++ "\":" ++ val)) ops
                        |> Data.List.intersperse "," 
                NonEmpty ->
                    Data.List.map (\(AddD [name] val) -> (",\"" ++ name ++ "\":" ++ val)) ops
            |> Data.List.map (\x -> yieldMany x >> return NonEmpty)
            |> Data.List.head

-- | Processes object fields one by one till the object's end.
processField :: Monad m => [Op] -> [Char] -> ConduitResult -> ContainerConduit m
processField ops buf res = do     
    dropWhileC (==' ')
    val <- takeC 1 .| sinkList 
    case val of     
        "" ->
            return res
        x -> 
            -- object end found, flush and return if level is empty
            if x == "}" then
                (getDirectAdditions ops |> flushAdditions res)
                >>= (\r -> case r of 
                    Empty -> return r;
                    NonEmpty -> yieldMany buf >> return r
                )
            else do
                fieldName <- getFieldName
                -- check if field is to be removed
                case fieldAction fieldName ops of
                    Just FieldRemove -> 
                        -- drop field
                        dropField []
                        >> processField ops buf res
                    Just (FieldAssignD val) -> 
                        dropField []
                        >> yieldMany buf
                        >> case res of
                                NonEmpty -> yieldMany (",\"" ++ fieldName ++ "\":" ++ val)
                                Empty -> yieldMany ("\"" ++ fieldName ++ "\":" ++ val)
                        >> processField ops "" NonEmpty
                    Just FieldSrc -> 
                        takeField [] []
                        >>= (\val ->
                            case res of
                                Empty -> yieldMany (buf ++ "\"" ++ fieldName ++ "\":" ++ val) 
                                NonEmpty -> yieldMany (buf ++ ",\"" ++ fieldName ++ "\":" ++ val)
                            >> processField (convertRelatives fieldName val ops) "" NonEmpty
                        )
                    _ -> 
                        case res of
                            -- nothing has been flushed before
                            Empty -> 
                                processUnknownValue (subrules fieldName ops) (buf ++ "\"" ++ fieldName ++ "\":")
                                >>= (\val -> case val of
                                    Empty -> processField ops buf Empty
                                    NonEmpty -> processField ops "" NonEmpty
                                )
                            -- non empty 
                            NonEmpty -> 
                                yieldMany buf
                                >> processUnknownValue (subrules fieldName ops) (",\"" ++ fieldName ++ "\":")
                                >> processField ops "" NonEmpty


-- | Returns a fieldname found within an object.
getFieldName :: Monad m => ConduitM Char o m [Char]
getFieldName = 
    -- drop object opening and first quotation mark
    dropWhileC (\x-> x == '{' || x == '\"' || x == ' ')
    -- get field name
    >> takeWhileC (/='\"') .| sinkList 
    -- drop field name end 
    >>= (\y -> dropWhileC (=='\"') >> dropWhileC (\x -> x == ':' || x == ' ')  >> return y)
        
-- | Writes a simple value to the output stream, used to process primitive object fields.
processFieldValue :: Monad m => CharConduit m
processFieldValue = 
    takeWhileC (\x -> x /= ',' && x /= '}' && x /= ']') .| sinkList
    >>= (\val -> dropWhileC (\x -> x == ',') >> yieldMany val)

-- | Processes all array elements one by one, similar to processField.        
processArrayElement :: Monad m => [Op] -> [Char] -> Int -> ConduitResult -> ContainerConduit m
processArrayElement ops buf index res =
    -- add fields  
    dropWhileC (\x -> elem x ", " )
    >> peekC
    >>= (\val ->
        case val of
            Nothing -> return Empty
            -- found array end
            Just ']' -> case res of
                Empty -> dropC 1 >> return Empty
                NonEmpty -> dropC 1 >> yieldMany buf >> return NonEmpty
            x ->
                case arrayAction index ops of
                    Just (ArrayAdd v) ->
                        case res of
                            Empty -> 
                                yieldMany buf
                            NonEmpty -> 
                                yieldMany buf >> yield ',' 
                        >> yieldMany v 
                        >> processArrayElement (removeAdditionRule index ops) "" index NonEmpty
                    Just FieldRemove -> 
                        dropField  []
                        >> processArrayElement ops buf (index + 1) res
                    Just (FieldAssignD v) -> 
                        dropField []
                        >> yieldMany buf
                        >> case res of
                                NonEmpty -> yieldMany ("," ++ v)
                                Empty -> yieldMany (v)
                        >> processArrayElement ops "" (index + 1) NonEmpty
                    Just FieldSrc -> 
                        takeField [] []
                        >>= (\val ->
                            let convertedOps = convertRelatives ("[" ++ (show index) ++  "]") val ops in 
                                case res of
                                    Empty -> yieldMany (buf ++ val) 
                                    NonEmpty -> yieldMany (buf ++ "," ++ val)
                                >> processArrayElement convertedOps "" (index + 1) NonEmpty                                
                        )
                    _ -> 
                        case res of
                            -- nothing has been flushed before
                            Empty -> 
                                processUnknownValue (subrulesArray index ops) buf
                                >>= (\val -> case val of
                                    Empty -> processArrayElement ops buf (index + 1) Empty
                                    NonEmpty -> processArrayElement ops "" (index + 1) NonEmpty
                                )
                            -- non empty 
                            NonEmpty -> 
                                yieldMany buf
                                >> processUnknownValue (subrulesArray index ops) ","
                                >> processArrayElement ops "" (index + 1) NonEmpty      
    )



