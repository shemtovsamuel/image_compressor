--
-- EPITECH PROJECT, 2021
-- B-FUN-400-PAR-4-1-compressor-samuel.shemtov [WSL: Ubuntu-20.04]
-- File description:
-- Types.hs
--

module Types (Input (..), PosPixel (..), Color (..), Pixel (..)) where

data Input = Input
  { nbColors :: Int,
    convergenceLimit :: Float,
    filepath :: String
  }

data PosPixel = PosPixel
  { x :: Int,
    y :: Int
  }
  deriving (Show, Eq)

data Color = Color
  { a :: Int,
    b :: Int,
    c :: Int
  }
  deriving (Show, Eq)

data Pixel = Pixel
  { pos :: PosPixel,
    color :: Color
  }
  deriving (Eq)
