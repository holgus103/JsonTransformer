module Main where

import System.IO
import System.Environment
import TransformsParser
import ConduitReader
import Conduit
import Control.Monad
import Data.Conduit.Text
import Flow


args :: IO [String]
args = return ["del .a.1.y,", "del .a.1.v,", "del .a.2,", "del .b"]


main = 
    args 
    >>= processArgs 
    >>= (\ops->
    runConduitRes $ sourceFile "test.txt" 
    .| decodeUtf8C 
    .| Data.Conduit.Text.lines
    .| linesToChars  
    .| processUnknownStart ops
    .| sinkList
    )
    



