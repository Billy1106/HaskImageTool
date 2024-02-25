module ProcessResize (processResize) where

import Codec.Picture
import FindArgValue (findArgValue)
import GaussianOperation (applyGaussian)
import qualified Data.Vector.Storable as VS

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

-- Function to apply Gaussian blur to the entire image
applyBlur :: Image PixelRGB8 -> Image PixelRGB8
applyBlur img = generateImage blurPixel (imageWidth img) (imageHeight img)
  where
    blurPixel x y = applyGaussian img x y

-- Resizing uses "takes nth pixel" strategy.
-- generateImage is a function provided by the JuicyPixels library.
-- It generates a new image by applying a function (getPixel/applyGaussian) to each pixel in the new image.
    -- 1.Create an empty canvas with the new dimensions.
    -- 2.Iterate over each pixel in the new image.
    -- 3.For each pixel, apply Gaussian to get the pixel value.
resizeImage :: Image PixelRGB8 -> Int -> Int -> Image PixelRGB8
resizeImage img newWidth newHeight = generateImageFromList newWidth newHeight resizedPixels
  where
    oldWidth = imageWidth img
    oldHeight = imageHeight img
    scaleX = fromIntegral oldWidth / fromIntegral newWidth
    scaleY = fromIntegral oldHeight / fromIntegral newHeight

    resizedPixels = [pixelAt img (floor $ fromIntegral x * scaleX) (floor $ fromIntegral y * scaleY) | y <- [0..newHeight-1], x <- [0..newWidth-1]]

-- Generate an image from a list of pixels (we can do this by using GenerateImage from JuicyPixels but let's do this manually)
-- Converts a list of PixelRGB8 pixels into an Image by creating a storable vector of pixel data.
generateImageFromList :: Int -> Int -> [PixelRGB8] -> Image PixelRGB8
generateImageFromList width height pixels =
    -- Image constructor (defined by JuicyPixel) takes the width, height, and a storable vector of pixel data.
    -- VS.fromListN converts a list into a storable vector with a predefined size.
    -- The size is width * height * 3 because each PixelRGB8 consists of 3 components (Red, Green, Blue),
    -- each requiring one byte, so we need 3 times the number of pixels to represent the entire image data (this ruined my life).
    Image width height (VS.fromListN (width * height * 3) 
    -- concatMap applies the function 'toList' to each pixel to convert it into a list of its RGB components,
    -- and then concatenates all these lists into a single list of bytes representing the entire image.
    (concatMap toList pixels)) 
  where
    -- Helper function to convert a PixelRGB8 into a list of its RGB components.
    toList (PixelRGB8 r g b) = [r, g, b]

processResize :: [String] -> IO ()
processResize args = do
    eitherImg <- readImage inputPath
    putStrLn $ "Processing image: " ++ inputPath
    either handleError processImage eitherImg
  where
    (inputPath, outputPath) = parseIOPaths args
    (width, height) = parseDimensions args

    handleError err = putStrLn $ "Error reading image: " ++ err

    processImage resizingImage = do
        let img = convertRGB8 resizingImage  -- Convert DynamicImage to Image PixelRGB8
        let blurredImage = applyBlur img  -- Apply Gaussian blur to remove noise (simply resizing will causes some noises)
        let resizedImage = resizeImage blurredImage width height  -- Resize the blurred image
        savePngImage outputPath (ImageRGB8 resizedImage)  -- Save the resized image
        putStrLn $ "Image processed and saved to " ++ outputPath