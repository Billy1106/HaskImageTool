import Codec.Picture

main :: IO ()
main = do
    -- 画像を読み込む
    result <- readImage "inputs/cat.jpg"
    case result of
        Left err -> putStrLn err
        Right dynamicImage -> do
            saveJpgImage 100 "outputs/cat_unprocessed.jpg" dynamicImage
