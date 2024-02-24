module ProcessResize (processResize) where

import Codec.Picture
import FindArgValue (findArgValue)
import GaussianOperation (applyGaussian)

parseDimensions :: [String] -> (Int, Int)
parseDimensions args = (width, height)
  where
    -- use read to convert the string to an integer
    width = read $ findArgValue "--width=" args
    height = read $ findArgValue "--height=" args

parseIOPaths :: [String] -> (FilePath, FilePath)
parseIOPaths args = (inputPath, outputPath)
    where
        inputPath = findArgValue "--input=" args
        outputPath = findArgValue "--output=" args

-- Resizing uses "takes nth pixel" strategy.
resizeImage :: DynamicImage -> Int -> Int -> Image PixelRGB8
resizeImage resizingImage newWidth newHeight = generateImage getPixel newWidth newHeight
  where
    img = convertRGB8 resizingImage
    scaleX = fromIntegral (imageWidth img) / fromIntegral newWidth
    scaleY = fromIntegral (imageHeight img) / fromIntegral newHeight
    
    getPixel x y = 
        let originalX = floor $ fromIntegral x * scaleX
            originalY = floor $ fromIntegral y * scaleY
        in applyGaussian img originalX originalY

processResize :: [String] -> IO ()
processResize args = do
    eitherImg <- readImage inputPath
    putStrLn $ "Resizing image: " ++ inputPath
    -- If readImage returns a Left err, handleError is called with err as its argument. 
    -- If readImage returns a Right resizingImage, processImage is called with resizingImage as its argument.
    either handleError processImage eitherImg
        where
            -- Destructure the tuples directly in the function 
            (inputPath, outputPath) = parseIOPaths args
            (width, height) = parseDimensions args

            handleError err = putStrLn $ "Error reading image: " ++ err

            processImage resizingImage = do
                savePngImage outputPath . ImageRGB8 $ resizeImage resizingImage width height
                putStrLn $ "Image resized and saved to " ++ outputPath
