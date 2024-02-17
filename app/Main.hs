import Codec.Picture

-- 縮小後の画像サイズを指定する関数
resizeImage :: FilePath -> FilePath -> Int -> Int -> IO ()
resizeImage inputPath outputPath newWidth newHeight = do
    -- 画像を読み込む
    result <- readImage inputPath
    case result of
        Left err -> putStrLn err -- エラーが発生した場合
        Right img -> do
            -- DynamicImageをImage PixelRGBA8に変換
            let imgRGBA8 = convertRGBA8 img
            -- 空の画像データを新しいサイズで作成
            let emptyImage = generateImage (\_ _ -> PixelRGBA8 0 0 0 255) newWidth newHeight
            -- 縮小処理
            let resizedImg = pixelMapXY (resizePixel imgRGBA8 newWidth newHeight) emptyImage
            -- 縮小した画像を保存
            savePngImage outputPath (ImageRGBA8 resizedImg)

-- 縮小処理を行う関数
resizePixel :: Image PixelRGBA8 -> Int -> Int -> Int -> Int -> PixelRGBA8
resizePixel img newWidth newHeight x y = 
    let origX = x * (imageWidth img) `div` newWidth
        origY = y * (imageHeight img) `div` newHeight
    in pixelAt img origX origY

main :: IO ()
main = resizeImage "sample.png" "resized.png" 100 100  -- 新しいサイズを100x100ピクセルに指定
