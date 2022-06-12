--
-- EPITECH PROJECT, 2021
-- B-FUN-400-PAR-4-1-compressor-samuel.shemtov [WSL: Ubuntu-20.04]
-- File description:
-- Algo.hs
--

module Algo
  ( randomCentroid,
    getNewCentroid,
    createClusters,
    loop,
  )
where

import Control.Monad (when)
import Data.Foldable
import Data.Function
import Data.List
import System.Random
import Types (Color (..), Pixel (..), PosPixel (..))

distance :: Pixel -> Pixel -> Float
distance (Pixel pos1 (Color a1 b1 c1)) (Pixel pos2 (Color a2 b2 c2)) =
  sqrt (a * a + b * b + c * c)
  where
    a = fromIntegral (a1 - a2) :: Float
    b = fromIntegral (b1 - b2) :: Float
    c = fromIntegral (c1 - c2) :: Float

closest :: Pixel -> [Pixel] -> Pixel
closest pixel = minimumBy (compare `on` distance pixel)

randomCentroid :: Int -> [Pixel] -> StdGen -> [Pixel]
randomCentroid 0 _ _ = []
randomCentroid n list gen =
  (list !! index) :
  randomCentroid
    (n - 1)
    (deleteVector (list !! index) list)
    newGen
  where
    (index, newGen) = randomR (0, length list - 1) gen

deleteVector :: Pixel -> [Pixel] -> [Pixel]
deleteVector equaltovector list = filter (\x -> x /= equaltovector) list

assign :: [Pixel] -> [Pixel] -> [[Pixel]]
assign list centroid = map (assignToCentroid list) centroid

assignToCentroid :: [Pixel] -> Pixel -> [Pixel]
assignToCentroid list centroid = map (getClosestCentroid centroid) list

getClosestCentroid :: Pixel -> Pixel -> Pixel
getClosestCentroid centroid pixel = pixel

clean :: [[Pixel]] -> [Pixel] -> Int -> [[Pixel]] -> [[Pixel]]
clean [] _ _ res = res
clean _ [] _ res = res
clean (x : xs) centroids i res
  | i == length centroids = res
  | otherwise = clean xs centroids (i + 1) (cleanCluster x i centroids : res)

cleanCluster :: [Pixel] -> Int -> [Pixel] -> [Pixel]
cleanCluster [] _ _ = []
cleanCluster (x : xs) i centroids
  | closest x centroids == centroids !! i = x : cleanCluster xs i centroids
  | otherwise = cleanCluster xs i centroids

createClusters :: [Pixel] -> [Pixel] -> [[Pixel]]
createClusters list centroids = clean (assign list centroids) centroids 0 []

getCenter :: [Pixel] -> Pixel
getCenter [] = Pixel (PosPixel 0 0) (Color 0 0 0)
getCenter list =
  Pixel
    (PosPixel 0 0)
    ( Color
        (sum (map (\(Pixel pos (Color a b c)) -> a) list) `div` len)
        (sum (map (\(Pixel pos (Color a b c)) -> b) list) `div` len)
        (sum (map (\(Pixel pos (Color a b c)) -> c) list) `div` len)
    )
  where
    len = fromIntegral (length list)

getNewCentroid :: [[Pixel]] -> [Pixel]
getNewCentroid list = map (getCenter) list

maxDistanceCentroid :: [Pixel] -> [Pixel] -> Float
maxDistanceCentroid [] _ = 0
maxDistanceCentroid _ [] = 0
maxDistanceCentroid (x : xs) (y : ys) =
  max (distance x y) (maxDistanceCentroid xs ys)

loop :: Float -> [Pixel] -> [Pixel] -> [Pixel] -> [[Pixel]]
loop convergence list centroids newCentroids
  | maxDistanceCentroid centroids newCentroids < convergence =
    createClusters list newCentroids
  | otherwise =
    loop convergence list newCentroids (getNewCentroid (createClusters list
      newCentroids))
