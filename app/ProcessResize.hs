module ProcessResize (processResize) where

import Codec.Picture
import Data.List (isPrefixOf)


parseDimensions :: [String] -> (Int, Int)
parseDimensions args =
    let width = read $ drop 8 $ head [arg | arg <- args, "--width=" `isPrefixOf` arg]
        height = read $ drop 9 $ head [arg | arg <- args, "--height=" `isPrefixOf` arg]
    in (width, height)

parseIOPaths :: [String] -> (FilePath, FilePath)
parseIOPaths args =
    let inputPath = head [drop 8 arg | arg <- args, "--input=" `isPrefixOf` arg]
        outputPath = head [drop 9 arg | arg <- args, "--output=" `isPrefixOf` arg]
    in (inputPath, outputPath)


resizeImage :: DynamicImage -> Int -> Int -> Image PixelRGB8
resizeImage dynImg newWidth newHeight =
    let img = convertRGB8 dynImg
        (width, height) = (imageWidth img, imageHeight img)
        scaleX = fromIntegral width / fromIntegral newWidth
        scaleY = fromIntegral height / fromIntegral newHeight
        getPixel x y = pixelAt img (floor $ fromIntegral x * scaleX) (floor $ fromIntegral y * scaleY)
    in generateImage getPixel newWidth newHeight


processResize :: [String] -> IO ()
processResize args = do
    let (width, height) = parseDimensions args
        (inputPath, outputPath) = parseIOPaths args
    eitherImg <- readImage inputPath
    putStrLn $ "Resizing image: " ++ inputPath
    case eitherImg of
        Left err -> putStrLn $ "Error reading image: " ++ err
        Right dynImg -> do
            let resizedImg = resizeImage dynImg width height
            savePngImage outputPath (ImageRGB8 resizedImg)
            putStrLn $ "Image resized and saved to " ++ outputPath
