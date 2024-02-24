module Main where

import Codec.Picture()
import System.Environment (getArgs)
import Data.List (intercalate)
import ProcessResize (processResize)
import ProcessMosaic (processMosaic)
import ProcessEdgeDetection (processEdgeDetection)

main :: IO ()
main = do
    -- Get the command line arguments and process them accordingly
    args <- getArgs
    case args of
        ("resize":params) -> processResize params -- If the first argument is "resize", call processResize
        ("mosaic":params) -> processMosaic params -- If the first argument is "mosaic", call processMosaic
        ("edge-detection":params) -> processEdgeDetection params -- If the first argument is "edge-detection", call processEdgeDetection
        _ -> putStrLn "Invalid command or parameters."

-- Takes a list of strings and returns a single string with the elements separated by a space.
-- e.g parseParams ["--width", "800", "--height", "600"]
--     => "--width 800 --height 600"
parseParams :: [String] -> [String]
parseParams [] = []
parseParams (key:value:rest)
    | null rest = [combine key value]  -- If 'rest' is empty, just combine the last key-value pair.
    | otherwise = combine key value : parseParams rest  -- Combine key-value and continue parsing.
    where
        combine k v = k ++ " " ++ v 
parseParams _ = ["Incomplete or improperly formatted parameters"]
