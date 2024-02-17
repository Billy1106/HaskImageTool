module Main where

import Codec.Picture

-- 縮小後の画像サイズを指定する関数
resizeImage :: FilePath -> FilePath -> Int -> Int -> IO ()
resizeImage inputPath outputPath newWidth newHeight = do
    -- 画像を読み込む
    result <- readImage inputPath
    case result of
        Left err -> putStrLn err
        Right img -> do
            let imgRGBA8 = convertRGBA8 img
            let resizedImg = generateImage (resizePixel imgRGBA8 newWidth newHeight) newWidth newHeight
            savePngImage outputPath (ImageRGBA8 resizedImg)

resizePixel :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> PixelRGBA8
resizePixel img newWidth newHeight x y = 
    let origX = x * (imageWidth img) `div` newWidth
        origY = y * (imageHeight img) `div` newHeight
    in pixelAt img origX origY

main :: IO ()
main = resizeImage "inputs/cat.jpg" "outputs/resized.jpg" 100 100  -- 新しいサイズを100x100ピクセルに指定
