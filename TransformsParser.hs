module TransformsParser where 

import System.Environment
import Operations  
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
    |> (\i -> parseFilter i <|> parseRemove i <|> parseAssignment i)
    |> (\x -> case x of 
            Just v -> v
            Nothing -> None
        )

-- [].asd
-- .asd
-- .asd[].asd
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

parseAssignment :: String -> Maybe Op
parseAssignment (x:xs) = 
    case x of 
        '.' -> fields xs "" []
        _ -> Nothing
    where 
        fields :: String -> String -> [String] -> Maybe Op
        fields (y:ys) f acc =
            case y of
                '.' -> reverse f 
                        |> (:acc)
                        |> fields ys ""
                '=' -> parseType ys ((reverse f):acc) AssignmentD AssignmentR
                '+' -> parseType ys ((reverse f):acc) AppendingD AppendingR
                x -> fields ys (y:f) acc
        
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




    


 






