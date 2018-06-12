module TransformsParser where 

import System.Environment
import Enums  
import Flow  
import Data.List.Split
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List



processArgs :: [String] -> IO [Op]
processArgs args = 
    Prelude.concat args
    |> splitOn ","
    |> Prelude.map parseOperation 
    |> return


parseOperation :: String -> Op
parseOperation s = 
    Prelude.dropWhile (==' ') s
    |> (\i -> parseFilter i <|> parseRemove i <|> parseAdd i <|> parseAssignment i)
    |> (\x -> case x of 
            Just v -> v
            Nothing -> None
        )

ensureDot :: String -> Maybe String
ensureDot (x:xs) = 
    case x of
        '.' -> Just xs
        _ -> Nothing


parseFilter :: String -> Maybe Op
parseFilter (x:xs) = 
    case x of
        '.' -> processField Filtering xs "" []
        _ -> Nothing

parseFilter [] = Nothing


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


parseAssignment :: String -> Maybe Op
parseAssignment (x:xs) = 
    case x of 
        '.' -> fields xs "" [] AssignmentD AssignmentR
        _ -> Nothing

parseAssignment [] = Nothing

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




    


 






