module ProcessMosaic (processMosaic) where

import Codec.Picture
import Data.List (isPrefixOf)
import FindArgValue (findArgValue, parseIOPaths)

mosaicImage :: Int -> Image PixelRGB8 -> Image PixelRGB8
mosaicImage blockSize img = generateImage generateMosaic (imageWidth img) (imageHeight img)
  where
    generateMosaic x y =
      let startX = x - (x `mod` blockSize)
          startY = y - (y `mod` blockSize)
          endX = min (imageWidth img) (startX + blockSize)
          endY = min (imageHeight img) (startY + blockSize)
          pixels = [pixelAt img x' y' | x' <- [startX .. endX - 1], y' <- [startY .. endY - 1]]
          avgColor = averageColor pixels
      in avgColor

    averageColor pixels =
      let (rSum, gSum, bSum, count) = foldr (\(PixelRGB8 r g b) (rs, gs, bs, c) -> (rs + fromIntegral r, gs + fromIntegral g, bs + fromIntegral b, c + 1)) (0, 0, 0, 0) pixels
          avg r = fromIntegral (r `div` count)
      in PixelRGB8 (avg rSum) (avg gSum) (avg bSum)

processMosaic :: [String] -> IO ()
processMosaic args = do
    let (inputPath, outputPath) = parseIOPaths args
        mosaicSize = read $ head [drop 7 arg | arg <- args, "--size=" `isPrefixOf` arg] :: Int
    eitherImg <- readImage inputPath
    case eitherImg of
        Left err -> putStrLn $ "Error reading image: " ++ err
        Right dynImg -> case convertRGB8 dynImg of
            img -> do
                let mosaicImg = mosaicImage mosaicSize img
                savePngImage outputPath (ImageRGB8 mosaicImg)
                putStrLn $ "Mosaic image saved to " ++ outputPath
