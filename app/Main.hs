module Main where

import Codec.Picture()
import System.Environment (getArgs)
import Data.List (intercalate)
import ProcessResize (processResize)

main :: IO ()
main = do
    args <- getArgs
    case args of
        ("resize":params) -> processResize params
        ("mosaic":params) -> processMosaic params
        ("edge-detection":params) -> processEdgeDetection params
        _ -> putStrLn "Invalid command or parameters."

processMosaic :: [String] -> IO ()
processMosaic params = putStrLn $ "Applying Mosaic Effect with parameters: " ++ intercalate ", " (parseParams params)

processEdgeDetection :: [String] -> IO ()
processEdgeDetection params = putStrLn $ "Applying Edge Detection with parameters: " ++ intercalate ", " (parseParams params)

parseParams :: [String] -> [String]
parseParams [] = []
parseParams (key:value:rest) = (key ++ " " ++ value) : parseParams rest
parseParams _ = ["Incomplete parameters"]

