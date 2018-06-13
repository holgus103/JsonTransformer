module Buffer where 

import Data.List
import Conduit
import Enums
import Flow


write :: Buffer -> String -> Buffer
write [] val =
    [("", val)]

write buf val = 
    let (t,l) = last buf in 
        (init buf) ++ [(t, l ++ val)]   
    
addBuffer :: Buffer -> String -> Buffer
addBuffer buf name = 
    buf ++ [(name, [])]

yieldBuffer :: Monad m => Buffer -> String -> ConduitM Char Char m Buffer 
yieldBuffer buf val =
    write buf val 
    |> flush

flush :: Monad m => Buffer -> ConduitM Char Char m Buffer
flush buf =
    helper "" buf
    where 
        -- helper ::        
        helper name list@[(t,l)] =  
            if name == t || t == "" then 
                yieldMany l >> return []
            else return list

        helper name list@((t,l):rest) =
            if name == t || t == "" then 
                yieldMany l >> helper name rest
            else return list
