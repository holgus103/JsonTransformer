module Main where

import System.IO
import System.Environment
import TransformsParser
import ConduitReader
import Conduit

testString :: String
testString = "{\"asd\":val,\"asd2\":val2}"

args :: IO [String]
args = return ["del .asd"]

main :: IO ()
main = 
    args 
    >>= processArgs 
    >>= (\ops->
    runConduit $ yieldMany testString 
    .| processUnknownStart ops
    .| mapM_C print
    )
    



