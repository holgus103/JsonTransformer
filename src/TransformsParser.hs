module TransformsParser where 

import System.Environment
import Enums  
import Flow  
import Data.List.Split
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List


-- | Parses an array of input arguments for operations
processArgs :: [String] -> IO [Op]
processArgs args = 
    Prelude.concat args
    |> splitOn ","
    |> Prelude.map parseOperation 
    |> return

-- | Parses a string for an operation and returns it on success
parseOperation :: String -> Op
parseOperation s = 
    Prelude.dropWhile (==' ') s
    |> (\i -> parseFilter i <|> parseRemove i <|> parseAdd i <|> parseAssignment i)
    |> (\x -> case x of 
            Just v -> v
            Nothing -> None
        )

-- | Parses a string for a filtering operation and returns it on success
parseFilter :: String -> Maybe Op
parseFilter (x:xs) = 
    case x of
        '.' -> processField Filtering xs "" []
        _ -> Nothing

parseFilter [] = Nothing

-- | Parses a string for an addition operation and returns it on success
parseAdd :: String -> Maybe Op
parseAdd s = 
    if isPrefixOf "add" s then 
        drop 3 s
        |> dropWhile (==' ')
        |> (\(x:xs) -> 
            case x of
                '.' -> fields xs "" [] AddD AddR
                _ -> Nothing
        )
    else Nothing

-- | Parses a string for an assignment operation and returns it on success
parseAssignment :: String -> Maybe Op
parseAssignment (x:xs) = 
    case x of 
        '.' -> fields xs "" [] AssignmentD AssignmentR
        _ -> Nothing

parseAssignment [] = Nothing

-- | Helper function used to get a value for assignment or addition and return an Operation of [String] String 
parseType :: String -> [String] -> 
    ([String] -> String -> Op) ->
    ([String] -> String -> Op) -> 
    Maybe Op
parseType (y:ys) acc d r = 
    case y of
        -- direct assignment 
        '\"' -> Just $ d (reverse acc) ('\"':ys) 
        -- relative assignment
        x -> Just $ r (reverse acc) $ (x:) $ takeWhile (/=' ') ys 

-- | Parses a string to extract subsequent field names and array indices 
-- | e.g. .message.[0].val becomes ["message", "[0]", "val"]
fields :: String -> String -> [String] -> 
    ([String] -> String -> Op) ->
    ([String] -> String -> Op) -> 
    Maybe Op
fields (y:ys) f acc d r =
    case y of
        '.' -> reverse f 
                |> (:acc)
                |> (\x -> fields ys "" x d r)
        '=' -> parseType ys ((reverse f):acc) d r
        x -> fields ys (y:f) acc d r  

-- | Parses a string for a removal operation and returns it on success
parseRemove :: String -> Maybe Op
parseRemove s =
    if isPrefixOf "del" s then 
        drop 3 s
        -- remove spaces
        |> dropWhile (==' ')
        |> (\(x:xs) ->
             case x of
                '.' -> processField Removal xs "" []
                _ -> Nothing)
    else Nothing


processField :: ([String] -> Op) -> String -> String -> [String] -> Maybe Op    
processField op (y:ys) acc res = 
    case y of 
        -- encountered next field
        '.' -> reverse acc 
                |> (:res)
                |>  processField op ys ""
        -- reached end of sequence
        ' ' -> reverse res
                |> op
                |> Just
        -- is append - fail
        '+' -> Nothing
        -- is assignment - fail
        '=' -> Nothing
        -- proceed processing field name
        x -> processField op ys (x:acc) res 

processField op [] acc res =
    case acc of 
        [] -> Nothing
        x -> reverse acc
            |> (:res)
            |> reverse 
            |> op
            |> Just




    


 






