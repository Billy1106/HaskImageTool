module ProcessEdgeDetection where
import FindArgValue (parseIOPaths)
import Codec.Picture
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

rgbToGrayscale :: PixelRGB8 -> Pixel8
rgbToGrayscale (PixelRGB8 r g b) = 
    round $ (fromIntegral r * 0.299 :: Double) + (fromIntegral g * 0.587 :: Double) + (fromIntegral b * 0.114 :: Double)


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
    edgeThreshold = (0.2 :: Double) * fromIntegral (maxBound :: Pixel8)
    kernelX = [[-1, 0, 1], [-2, 0, 2], [-1, 0, 1]]
    kernelY = [[-1, -2, -1], [0, 0, 0], [1, 2, 1]]

