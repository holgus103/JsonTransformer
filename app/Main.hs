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
args = return ["add .[].b=\"11\""]

main :: IO ()
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
    >>= (\x -> putStrLn $ show x)
    >> return ()
    


