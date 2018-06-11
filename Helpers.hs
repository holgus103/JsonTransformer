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

assignableDirectly :: [Char] -> [Op] -> Maybe Op
assignableDirectly name ops =
    find (\x -> case x of 
        AssignmentD [f] _ -> f == name
        _ -> False 
    ) ops

-- filters rules for a field 
subrules :: [Char] -> [Op] -> [Op]
subrules name ops = 
    Prelude.filter (\x-> case x of
            Removal (h:h2:t) -> h == name
            AssignmentD (h:h2:t) _ -> h == name 
            _ -> False 
        ) ops   
    |> Prelude.map (\x -> case x of 
            Removal (h:t) -> Removal t
            AssignmentD (h:t) v -> AssignmentD t v
            v -> v
        ) 

fieldAction :: [Char] -> [Op] -> Maybe Op
fieldAction name ops =
    (removable name ops)  <|> (assignableDirectly name ops)



-- drops an entire field 
dropField :: Monad m => [Char] -> CharConduit m

dropField [] = do
    dropWhileC (\x -> not $ elem x "{[,}]")
    val <- takeC 1 .| sinkList
    case val of
        "{" -> dropField $ trace "pushing { onto the stack" "{"
        "," -> (trace "found end" return) ()
        "[" -> dropField "["
        "}" -> do {leftover '}'; return ();}

dropField stack = do
    dropWhileC (\x -> not $ elem x "{[,}]")
    val <- takeC 1 .| sinkList
    case val of
        "{" -> dropField $ trace "pushing { onto the stack" ('{':stack)
        "[" -> dropField ('[':stack)
        "}" -> dropField $ Prelude.tail $ trace "taking } from the stack" stack
        "]" -> dropField $ Prelude.tail stack
        x -> dropField $ trace ("found" ++ show x) stack


    
        
    