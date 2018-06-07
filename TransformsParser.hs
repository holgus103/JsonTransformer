module TransformsParser where 

import System.Environment
import Operations  
import Flow  
import Data.List.Split
import Control.Applicative
import Control.Monad
import Data.Char
import Data.List

parseArgs :: IO [Op]
parseArgs = do
    args <- getArgs
    Prelude.concat args |> processArgs |> return 

processArgs :: String -> [Op]
processArgs args = 
    splitOn "," args
    |> Prelude.map parseOperation 


parseOperation :: String -> Op
parseOperation s = 
    Prelude.dropWhile (==' ') s
    |> (\i -> parseFilter i <|> parseRemove i)
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




    


 






