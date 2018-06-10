module Helpers where 


import Enums
import Conduit
import Debug.Trace
import Flow

type CharConduit m = ConduitM Char Char m ()

type ContainerConduit m = ConduitM Char Char m ConduitResult


-- check ops for a matching removal token
notRemovable :: [Char] -> [Op] -> Bool
notRemovable fieldName ops = 
    Prelude.all (\x -> case x of
        Removal [name] -> name /= fieldName 
        _ -> True
    ) ops

-- filters rules for a field 
subrules :: [Char] -> [Op] -> [Op]
subrules name ops = 
    Prelude.filter (\x-> case x of
            Removal (h:h2:t) -> h == name
            _ -> False 
        ) ops   
    |> Prelude.map (\x -> case x of 
            Removal (h:t) -> Removal t
            v -> v
        ) 

        
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


    
        
    