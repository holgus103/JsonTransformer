module Helpers where 


import Enums
import Conduit
import Debug.Trace
import Flow
import Data.List
import Control.Applicative

type CharConduit m = ConduitM Char Char m ()

type ContainerConduit m = ConduitM Char Char m ConduitResult


-- check ops for a matching removal token
removable :: [Char] -> [Op] -> Maybe Op
removable fieldName ops = 
    find (\x -> case x of
        Removal [name] -> name == fieldName 
        _ -> False
    ) ops

removableArray :: Int -> [Op] -> Maybe Op
removableArray index ops =
    find (\x -> case x of
        Removal [v]-> appliesToIndex v index
        _ -> False
    ) ops
    
assignableDirectly :: [Char] -> [Op] -> Maybe Op
assignableDirectly name ops =
    find (\x -> case x of 
        AssignmentD [f] _ -> f == name
        _ -> False 
    ) ops

assignableDirectlyArray :: Int -> [Op] -> Maybe Op
assignableDirectlyArray index ops =
    find (\x -> case x of
        AssignmentD [v] _ -> appliesToIndex v index
        _ -> False
    ) ops

getDirectAdditions :: [Op] -> [Op]
getDirectAdditions ops =
    filter (\x -> case x of
        AddD [f] _ -> True
        _ -> False
    ) ops

appliesToIndex :: [Char] -> Int -> Bool
appliesToIndex val index =
    val == "[" ++ (show index) ++ "]" || val == "[]"


reduceRuleLevel :: [Op] -> [Op]
reduceRuleLevel ops = 
    Prelude.map (\x -> case x of 
        Removal (h:t) -> Removal t
        AssignmentD (h:t) v -> AssignmentD t v
        AddD (h:t) v -> AddD t v
        AssignmentR (h:t) v -> AssignmentR t v
        AddR (h:t) v -> AddR t v
        v -> v
    ) ops

-- filters rules for a field 
subrules :: [Char] -> [Op] -> [Op]
subrules name ops = 
    Prelude.filter (\x-> case x of
            Removal (h:h2:t) -> h == name
            AssignmentD (h:h2:t) _ -> h == name 
            AddD (h:h2:t) _ -> h == name
            AssignmentR (h:h2:t) _ -> h == name
            AddR (h:h2:t) _ -> h == name 
            _ -> False 
        ) ops   
    |> reduceRuleLevel


subrulesArray :: Int -> [Op] -> [Op]
subrulesArray index ops =
    Prelude.filter (\x -> case x of
        Removal (h:h2:t) -> appliesToIndex h index
        AssignmentD (h:h2:t) _ -> appliesToIndex h index
        AddD (h:h2:t) _ -> appliesToIndex h index
        AssignmentR (h:h2:t) _ -> appliesToIndex h index
        AddR (h:h2:t) _ -> appliesToIndex h index
        v -> False
    ) ops
    |> reduceRuleLevel


fieldAction :: [Char] -> [Op] -> Maybe Op
fieldAction name ops =
    (removable name ops)  <|> (assignableDirectly name ops)

arrayAction :: Int -> [Op] -> Maybe Op
arrayAction index ops =
    (removableArray index ops) <|> (assignableDirectlyArray index ops)
    

-- drops an entire field 
dropField :: Monad m => [Char] -> CharConduit m

dropField [] = do
    dropWhileC (\x -> not $ elem x "{[,}]")
    val <- takeC 1 .| sinkList
    case val of
        "{" -> 
            -- trace "pushing { onto the stack" (return 1) >>
            dropField "{"
        "," -> 
            -- trace "found end" (return 1) >>
            return ()
        "[" -> 
            dropField "["
        "}" -> 
            leftover '}' >> return ()

dropField stack = do
    dropWhileC (\x -> not $ elem x "{[,}]")
    val <- takeC 1 .| sinkList
    case val of
        "{" -> dropField $ trace "pushing { onto the stack" ('{':stack)
        "[" -> dropField ('[':stack)
        "}" -> dropField $ Prelude.tail $ trace "taking } from the stack" stack
        "]" -> dropField $ Prelude.tail stack
        x -> dropField $ trace ("found" ++ show x) stack


    
        
    