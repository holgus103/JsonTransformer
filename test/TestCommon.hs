module TestCommon where 

import Conduit
import Data.Conduit.Text
import ConduitReader
import TransformsParser
import Control.Monad
import Test.HUnit

performParsingTest :: [String] -> String -> String -> Test
performParsingTest args input output = TestCase 
    (processArgs args
    -- >>=(\ops -> putStrLn (Prelude.concat opsArgs) >> return ops)
    -- >>= (\ops -> (putStrLn $ show ops) >> return ops)
    >>= (\ops->
    runConduitRes $ yieldMany input
    .| processUnknownStart ops
    .| sinkList
    ) 
    >>= assertEqual "deleteFromArray" output) 