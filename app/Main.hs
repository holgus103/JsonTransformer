module Main where

import System.IO
import System.Environment
import TransformsParser
import ConduitReader
import Conduit
import Control.Monad
import Data.Conduit.Text
import Flow
import qualified Data.Text as T


args :: IO [String]
args = return ["test.txt", "a", ".b=a"]

main :: IO ()
main = 
    args
    -- getArgs 
    >>= (\args ->
        if length args < 2 then return ()
        else
            let (pathIn:pathOut:opsArgs) = args in 
             
                processArgs opsArgs
                -- >>=(\ops -> putStrLn (Prelude.concat opsArgs) >> return ops)
                >>= (\ops -> (putStrLn $ show ops) >> return ops)
                >>= (\ops->
                runConduitRes $ sourceFile pathIn
                .| decodeUtf8C 
                .| Data.Conduit.Text.lines
                .| linesToChars  
                .| processUnknownStart ops
                -- .| mapC (\x -> T.pack [x])
                -- .| encodeUtf8C
                -- .| sinkFile pathOut
                .| sinkList
                )
                >>= (\x -> putStrLn $ show x)
                >> return ()
    )
    



