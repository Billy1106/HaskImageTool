module GaussianOperation (applyGaussian) where

import Codec.Picture

-- Gaussian kernel:https://en.wikipedia.org/wiki/Gaussian_function
-- The kernel is a 3x3 matrix of doubles
gaussianKernel :: Double -> [[Double]]
gaussianKernel sigma = 
    [[exp (-(x^2 + y^2) / (2 * sigma^2)) / (2 * pi * sigma^2) | x <- [-1..1]] | y <- [-1..1]]

applyGaussian :: Image PixelRGB8 -> Int -> Int -> PixelRGB8
    -- generate a tuple of dx and dy for each pixel in the kernel
    --  e.g for a 3x3 kernel, the list will be [(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
    -- then for each tuple, get the pixel at the x + dx and y + dy coordinates and multiply it by the corresponding value in the kernel
    -- then sum all the pixels to get the new pixel value (foldr is used to sum the pixels)
    -- reminder: foldr f z [a, b, c] = f a (f b (f c z))
applyGaussian img x y = foldr (\(dx, dy) acc -> convolve img kernel x y dx dy acc) (PixelRGB8 0 0 0) kernelIndices
  where
    kernel = gaussianKernel 1.0  -- Example: Adjust sigma to change the blur strength
    offset = length kernel `div` 2 -- Offset of the kernel to the center (kernel is always square)
    kernelIndices = [(dx, dy) | dy <- [-offset..offset], dx <- [-offset..offset]]

-- Convolve a single pixel around (x, y) with the Gaussian kernel
convolve :: Image PixelRGB8 -> [[Double]] -> Int -> Int -> Int -> Int -> PixelRGB8 -> PixelRGB8
convolve img kernel x y dx dy acc =
    let samplePixel = pixelAt img (clampX (x + dx)) (clampY (y + dy))
        weight = kernel !! (dy + offset) !! (dx + offset)
    in addWeightedPixel acc (samplePixel, weight)
  where
    offset = length kernel `div` 2 -- Offset of the kernel to the center (kernel is always square)

    -- Ensure x and y values are within the image boundaries
    clampX = clamp 0 (imageWidth img - 1)
    clampY = clamp 0 (imageHeight img - 1)

-- The rest of the functions remain the same as before

-- ensure that the pixel coordinates stay within the image boundaries
clamp :: Int -> Int -> Int -> Int
clamp minVal maxVal = max minVal . min maxVal

-- Add weighted pixel to the original pixel in order to apply the kernel
-- e,g addWeightedPixel (PixelRGB8 100 100 100) (PixelRGB8 200 200 200, 0.5) => PixelRGB8 200 200 200
--  where weight is 0.5 and the original pixel is (200, 200, 200)
addWeightedPixel :: PixelRGB8 -> (PixelRGB8, Double) -> PixelRGB8
addWeightedPixel (PixelRGB8 r1 g1 b1) (PixelRGB8 r2 g2 b2, weight) =
    PixelRGB8 (r1 + round (fromIntegral r2 * weight))
              (g1 + round (fromIntegral g2 * weight))
              (b1 + round (fromIntegral b2 * weight))