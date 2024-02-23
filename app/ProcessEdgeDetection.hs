module ProcessEdgeDetection where

import Codec.Picture
import Codec.Picture.Types
import System.Environment (getArgs)
import Data.List (isPrefixOf)

processEdgeDetection :: [String] -> IO ()
processEdgeDetection args = do
    let (inputPath, outputPath) = parseIOPaths args
    eitherImg <- readImage inputPath
    case eitherImg of
        Left err -> putStrLn $ "Error reading image: " ++ err
        Right dynImg -> case convertRGB8 dynImg of
            img -> do
                let edgeImg = detectEdges img
                savePngImage outputPath (ImageRGB8 edgeImg)
                putStrLn $ "Edge detected image saved to " ++ outputPath

parseIOPaths :: [String] -> (FilePath, FilePath)
parseIOPaths args =
    let inputPath = head [drop 8 arg | arg <- args, "--input=" `isPrefixOf` arg]
        outputPath = head [drop 9 arg | arg <- args, "--output=" `isPrefixOf` arg]
    in (inputPath, outputPath)

rgbToGrayscale :: PixelRGB8 -> Pixel8
rgbToGrayscale (PixelRGB8 r g b) = 
    round $ fromIntegral r * 0.299 + fromIntegral g * 0.587 + fromIntegral b * 0.114

safePixelAt :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
safePixelAt img x y = if inBounds then pixelAt img x y else PixelRGB8 0 0 0
  where
    width = imageWidth img
    height = imageHeight img
    inBounds = x >= 0 && y >= 0 && x < width && y < height

detectEdges :: Image PixelRGB8 -> Image PixelRGB8
detectEdges img = generateImage pixelFunc (imageWidth img) (imageHeight img)
  where
    pixelFunc x y = if sobel x y > edgeThreshold then PixelRGB8 255 255 255 else PixelRGB8 0 0 0
    sobel x y = sqrt (fromIntegral gx * fromIntegral gx + fromIntegral gy * fromIntegral gy)
      where
        gx = sum [fromIntegral (rgbToGrayscale (safePixelAt img (x + dx) (y + dy))) * kernelX !! (dx + 1) !! (dy + 1) | dx <- [-1..1], dy <- [-1..1]]
        gy = sum [fromIntegral (rgbToGrayscale (safePixelAt img (x + dx) (y + dy))) * kernelY !! (dx + 1) !! (dy + 1) | dx <- [-1..1], dy <- [-1..1]]
    edgeThreshold = 0.2 * fromIntegral (maxBound :: Pixel8)
    kernelX = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]
    kernelY = [[-1, -2, -1], [0, 0, 0], [1, 2, 1]]

