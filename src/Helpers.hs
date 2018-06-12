module Helpers where 


import Enums
import Conduit
-- import Debug.Trace
import Flow
import Data.List
import Control.Applicative

-- | A conduit with an input and output stream of Char and no return value
type CharConduit m = ConduitM Char Char m ()

-- | Similar to .CharConduit but with a return value of ConduitResult
type ContainerConduit m = ConduitM Char Char m ConduitResult



-- | Checks whether a field is to be removed or not
removable :: [Char] -> [Op] -> Maybe Action
removable fieldName ops = 
    find (\x -> case x of
        Removal [name] -> name == fieldName 
        _ -> False
    ) ops
    >>= (\_ -> return (FieldRemove))

-- | Checks whether an array element is to be removed or not
removableArray :: Int -> [Op] -> Maybe Action
removableArray index ops =
    find (\x -> case x of
        Removal [v]-> appliesToIndex v index
        _ -> False
    ) ops
    >>= (\_ -> return (FieldRemove))
    
-- | Checks whether a value should be assigned to a field
assignableDirectly :: [Char] -> [Op] -> Maybe Action
assignableDirectly name ops =
    find (\x -> case x of 
        AssignmentD [f] _ -> f == name
        _ -> False 
    ) ops
    >>= (\(AssignmentD _ v) -> return (FieldAssignD v))

-- | Checks whether a value should be assigned to an array index
assignableDirectlyArray :: Int -> [Op] -> Maybe Action
assignableDirectlyArray index ops =
    find (\x -> case x of
        AssignmentD [v] _ -> appliesToIndex v index
        _ -> False
    ) ops
    >>= (\(AssignmentD _ v) -> return (FieldAssignD v))

-- | Checks whether a field is a source for a relative assignment
relativeSource :: [Char] -> [Op] -> Maybe Action
relativeSource name ops =
    find (\x -> case x of 
        AssignmentR [v] value -> value == name
        _ -> False
    ) ops
    >>= (\_ -> return FieldSrc)

-- | Returns all direct assignments from an operation list
getDirectAdditions :: [Op] -> [Op]
getDirectAdditions ops =
    filter (\x -> case x of
        AddD [f] _ -> True
        _ -> False
    ) ops

-- | Checks if an operation is applicable to an array index
appliesToIndex :: [Char] -> Int -> Bool
appliesToIndex val index =
    val == "[" ++ (show index) ++ "]" || val == "[]"

-- | Recudes rules by one level - i.e. rule applicable to .message.name becomes applicable to .name
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

-- | Converts a relative assignment to a direct assignmnet - storing the value specified for direct use
convertRelativeAssignment :: [Char] -> [Char] -> [Op] -> [Op]
convertRelativeAssignment name value ops =
    map (\x -> 
        case x of 
            AssignmentR [v] src -> if src == name then AssignmentD [v] value 
                                   else AssignmentR [v] src
            v -> v 
    ) ops

-- | Gets all rules applicable for nested values within a field
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

-- | Gets all rules applicable for nested values within an array element
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

-- | Returns the action that is applicable to the currently processed field
fieldAction :: [Char] -> [Op] -> Maybe Action
fieldAction name ops =
    removable name ops <|> relativeSource name ops  <|> assignableDirectly name ops

-- | Returns the action that is applicable to the currently processed array index
arrayAction :: Int -> [Op] -> Maybe Action
arrayAction index ops =
    removableArray index ops <|> relativeSource name ops <|> assignableDirectlyArray index ops
    where 
        name = "[" ++ (show index) ++  "]"
    

-- | Consumes a whole field from the input stream
dropField :: Monad m => [Char] -> CharConduit m
dropField [] =
    dropWhileC (\x -> not $ elem x " {[,}]")
    >> takeC 1 .| sinkList
    -- >>= (\val -> trace (show val) (return val))
    >>= (\val -> case val of
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
        "]" -> leftover ']' >> return ()    
    )

dropField stack =
    dropWhileC (\x -> not $ elem x "{[,}]")
    >> takeC 1 .| sinkList
    >>= (\val -> case val of
            "{" -> dropField ('{':stack)
            "[" -> dropField ('[':stack)
            "}" -> dropField $ Prelude.tail stack
            "]" -> dropField $ Prelude.tail stack
            x -> dropField stack
    )

-- | Returns the value of a whole field, including its subvalues
takeField :: Monad m => String -> String -> ConduitM Char Char m [Char] 
takeField [] buf = do
    val <- takeWhileC (\x -> not $ elem x "[{,}]") .| sinkList
    c <- takeC 1 .| sinkList
    case c of 
        "," -> return (buf ++ val)
        "]" -> return (buf ++ val)
        "}" -> return (buf ++ val)
        "[" -> takeField "[" (buf ++ val)
        "{" -> takeField "{" (buf ++ val)

takeField (e:rest) buf = do
    val <- takeWhileC (\x -> not $ elem x "[{}]") .| sinkList
    c <-takeC 1 .| sinkList
    case c of
        "}" -> takeField (buf ++ val) rest
        "]" -> takeField (buf ++ val) rest
        "{" -> takeField (buf ++ val) ('{':e:rest)
        "[" -> takeField (buf ++ val) ('[':e:rest)



    
        
    