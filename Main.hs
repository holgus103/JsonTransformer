module Main where

import System.IO
import System.Environment
import TransformsParser
import ConduitReader
import Conduit
import Control.Monad
import Flow


testString :: String
testString = "{\"a\":a,\"b\":b, \"c\": c}"

args :: IO [String]
args = return ["del .a,", "del .b"]


main = 
    args 
    >>= processArgs 
    >>= (\ops->
     runConduit $ yieldMany testString 
    .| processUnknownStart ops
    .| sinkList
    )
    



