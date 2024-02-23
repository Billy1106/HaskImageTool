module Main where

import Codec.Picture()
import System.Environment (getArgs)
import Data.List (intercalate)
import ProcessResize (processResize)
import ProcessMosaic (processMosaic)
import ProcessEdgeDetection (processEdgeDetection)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("resize":params) -> processResize params
        ("mosaic":params) -> processMosaic params
        ("edge-detection":params) -> processEdgeDetection params
        _ -> putStrLn "Invalid command or parameters."

parseParams :: [String] -> [String]
parseParams [] = []
parseParams (key:value:rest) = (key ++ " " ++ value) : parseParams rest
parseParams _ = ["Incomplete parameters"]

