module Buffer where 

import Data.List
import Conduit
import Helpers

type Buffer = [(String, String)]

(+=) :: Buffer -> String -> Buffer
infixr 7 +=
(+=) buf val =
    (init buf) ++ [(t, l ++ val)]
    where 
     (t, l) = last buf 

flush :: Monad m => String -> Buffer ->  CharConduit m
flush name buf =
    helper name buf
    where 
        -- helper ::        
        helper name [(t,l)] =  
            if name == t || t == "" then 
                yieldMany l
            else return ()

        helper name ((t,l):rest) =
            if name == t || t == "" then 
                yieldMany l >> helper name rest
            else return ()
