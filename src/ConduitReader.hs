module ConduitReader where 

import Conduit
import Enums
import Data.Text
import Debug.Trace
import Flow
import Helpers
import Control.Monad
import Data.List

linesToChars :: Monad m => ConduitM Text Char m ()
linesToChars = 
    await
    >>= (\val ->
        case val of 
            Just x -> (yieldMany $ unpack x) >> linesToChars
            Nothing -> return (); 
    )

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


processObject :: Monad m => [Op] -> [Char] -> ContainerConduit m
processObject ops buf =
    -- consume object start
    dropWhileC (== '{')
    -- >> trace "processing object" (return 1)
    -- let relatives = getRelativeOps ops
    >> case additions of
        -- no additons
        [] -> 
            processField ops (buf ++ "{") Empty
            -- consume object end
            >>= (\res -> case res of
                Empty -> 
                    -- trace "object got empty" (return 1) >>
                    return Empty
                -- if the buffered values were flushed - the object is not empty
                NonEmpty -> 
                    yield '}' 
                    -- >> yield '!'
                    -- >> trace "object got nonempty" (return 1)
                    >> return NonEmpty
                )
        -- some additions were done
        _  -> 
            yieldMany (buf ++ "{")
            >> flushAdditions additions
            >> processField ops "" NonEmpty
            >> yield '}'
            >> return NonEmpty
        where 
            additions = getDirectAdditions ops


flushAdditions :: Monad m => [Op] -> ContainerConduit m
flushAdditions ops = 
    case ops of
        [] -> return Empty
        _ -> Data.List.map (\(AddD [name] val) -> (name ++ ":" ++ val)) ops
            |> Data.List.intersperse "," 
            |> Data.List.map (\x -> yieldMany x >> return NonEmpty)
            |> Data.List.head

processField :: Monad m => [Op] -> [Char] -> ConduitResult -> ContainerConduit m
processField ops buf res = do     
    dropWhileC (==' ')
    val <- takeC 1 .| sinkList 
    case val of     
        -- "" -> (trace ("returning with " ++ buf) (return 1) >> return res
        x -> 
            -- object end found, flush and return if level is empty
            if x == "}" then
                case res of 
                    Empty -> return res;
                    NonEmpty -> yieldMany buf >> return res
            else do
                fieldName <- getFieldName
                -- d <- (trace ("processing field " ++ fieldName ++ " " ++ (show $ fieldAction fieldName ops) ++ " ops:" ++ (show ops) ) (return 1) )
                -- check if field is to be removed
                case fieldAction fieldName ops of
                    Just FieldRemove -> 
                        -- drop field
                        dropField []
                        -- >> trace ("dropping field " ++ fieldName ++ " buffer: " ++ buf) (return 1)
                        >> processField ops buf res
                    Just (FieldAssignD val) -> 
                        dropField []
                        -- d <- trace ("assignment buffer " ++ buf) (return 1)
                        >> yieldMany buf
                        >> case res of
                                NonEmpty -> yieldMany ("," ++ fieldName ++ ":" ++ val)
                                Empty -> yieldMany (fieldName ++ ":" ++ val)
                        >> processField ops "" NonEmpty

                    _ -> 
                        case res of
                            -- nothing has been flushed before
                            Empty -> 
                                processUnknownValue (subrules fieldName ops) (buf ++ fieldName ++ ":")
                                >>= (\val -> case val of
                                    Empty -> processField ops buf Empty
                                    NonEmpty -> processField ops "" NonEmpty
                                )
                            -- non empty 
                            NonEmpty -> 
                                yieldMany buf
                                >> processUnknownValue (subrules fieldName ops) (',':fieldName ++ ":")
                                >> processField ops "" NonEmpty



getFieldName :: Monad m => ConduitM Char o m [Char]
getFieldName = 
    -- drop object opening and first quotation mark
    dropWhileC (\x-> x == '{' || x == '\"' || x == ' ')
    -- get field name
    >> takeWhileC (/='\"') .| sinkList 
    -- drop field name end 
    >>= (\y -> dropWhileC (\x -> x =='\"' || x == ':' || x == ' ')  >> return y)
        
-- simply flushes a field
processFieldValue :: Monad m => CharConduit m
processFieldValue = 
    takeWhileC (\x -> x /= ',' && x /= '}' && x /= ']') .| sinkList
    -- d <- trace ("field value: " ++ val) (return 1)
    >>= (\val -> dropWhileC (\x -> x == ',') >> yieldMany val)

        
processArrayElement :: Monad m => [Op] -> [Char] -> Int -> ConduitResult -> ContainerConduit m
processArrayElement ops buf index res = 
    dropWhileC (\x -> elem x ", " )
    -- >> trace ("processing index: " ++ (show index) ++ " buf:" ++ buf) (return 1)
    >> peekC
    -- >>= (\v -> trace (show v) (return v))
    >>= (\val ->
        case val of
            Nothing -> return Empty
            -- found array end
            Just ']' -> case res of
                Empty -> dropC 1 >> return Empty
                NonEmpty -> dropC 1 >> yieldMany buf >> return NonEmpty
            x ->
                case arrayAction index ops of
                Just FieldRemove -> 
                    dropField  []
                    >> processArrayElement ops buf (index + 1) res
                Just (FieldAssignD val) -> 
                    dropField []
                    -- d <- trace ("assignment buffer " ++ buf) (return 1)
                    >> yieldMany buf
                    >> case res of
                            NonEmpty -> yieldMany ("," ++ val)
                            Empty -> yieldMany (val)
                    >> processArrayElement ops "" (index + 1) NonEmpty

                _ -> 
                    case res of
                        -- nothing has been flushed before
                        Empty -> 
                            processUnknownValue (subrulesArray index ops) buf
                            -- >>= (\x -> yield '1' >> return x)
                            >>= (\val -> case val of
                                Empty -> processArrayElement ops buf (index + 1) Empty
                                NonEmpty -> processArrayElement ops "" (index + 1) NonEmpty
                            )
                        -- non empty 
                        NonEmpty -> 
                            yieldMany buf
                            >> processUnknownValue (subrulesArray index ops) ","
                            -- >> yield '!'
                            >> processArrayElement ops "" (index + 1) NonEmpty      
    )



