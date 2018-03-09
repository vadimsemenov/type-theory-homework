module Util where

import System.IO
import System.Environment


io :: (Handle -> Handle -> IO ()) -> IO ()
io action = do
    (inputFile : outputFile : _) <- getArgs
    input <- openFile inputFile ReadMode
    output <- openFile outputFile WriteMode
    action input output
    hClose input
    hClose output
