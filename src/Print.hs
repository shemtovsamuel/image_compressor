--
-- EPITECH PROJECT, 2021
-- B-FUN-400-PAR-4-1-compressor-samuel.shemtov [WSL: Ubuntu-20.04]
-- File description:
-- Print.hs
--

module Print (displayClusters) where

import Text.Printf (printf)
import Types (Color (..), Pixel (..), PosPixel (..))

displayClusters :: [[Pixel]] -> [Pixel] -> IO ()
displayClusters [] _ = return ()
displayClusters _ [] = return ()
displayClusters (x : xs) ((Pixel (PosPixel _ _) (Color r g b)) : ys) =
  printf "--\n(%d,%d,%d)\n-\n" r g b >> displayPixels x
    >> displayClusters xs ys

displayPixels :: [Pixel] -> IO ()
displayPixels [] = return ()
displayPixels (x : xs) = displayPixel x >> displayPixels xs

displayPixel :: Pixel -> IO ()
displayPixel (Pixel (PosPixel x y) (Color r g b)) =
  printf "(%d,%d) (%d,%d,%d)\n" x y r g b
