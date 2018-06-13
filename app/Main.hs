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


-- | Main function, starts the processing of data specified by program arguments .
main :: IO ()
main = 
    getArgs 
    >>= (\args ->
        if length args < 2 then return ()
        else
            let (pathIn:pathOut:opsArgs) = args in 
             
                processArgs opsArgs
                >>= (\ops->
                runConduitRes $ sourceFile pathIn
                .| decodeUtf8C 
                .| Data.Conduit.Text.lines
                .| linesToChars  
                .| processUnknownStart ops
                .| mapC (\x -> T.pack [x])
                .| encodeUtf8C
                .| sinkFile pathOut
                )
                >> return ()
    )
    



