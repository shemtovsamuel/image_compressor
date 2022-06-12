--
-- EPITECH PROJECT, 2021
-- B-FUN-400-PAR-4-1-compressor-samuel.shemtov [WSL: Ubuntu-20.04]
-- File description:
-- Parser.hs
--

module Parser (parseFileContent) where

import Types (Color (..), Pixel (..), PosPixel (..))

split :: String -> [String]
split [] = []
split (',' : xs) = "" : split xs
split ('(' : xs) = split xs
split (' ' : xs) = split xs
split (')' : xs) = "" : split xs
split (x : xs) = (x : head (split xs)) : tail (split xs)

createPixel :: PosPixel -> Color -> Pixel
createPixel = Pixel

parseFileContent :: [String] -> [Pixel]
parseFileContent file =
  map ( \line -> createPixel
          ( PosPixel (read (head (split line)) :: Int)
              (read (split line !! 1) :: Int))
          ( Color (read (split line !! 2) :: Int)
              (read (split line !! 3) :: Int)
              (read (split line !! 4) :: Int))
    ) (filter (not . null) file)
