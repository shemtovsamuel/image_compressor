--
-- EPITECH PROJECT, 2021
-- B-FUN-400-PAR-4-1-compressor-samuel.shemtov [WSL: Ubuntu-20.04]
-- File description:
-- Main.hs
--

import Algo (createClusters, getNewCentroid, loop, randomCentroid)
import Control.Monad (when)
import Data.Foldable
import Data.Function
import Data.List
import GetInput (getInput)
import Help (help)
import Lib ()
import Parser (parseFileContent)
import Print (displayClusters)
import System.Environment (getArgs)
import System.Exit (ExitCode (ExitFailure), exitWith)
import System.Random
import Text.Read ()
import Types (Color (..), Input (..), Pixel (..), PosPixel (..))

start :: Input -> IO ()
start input = do
  g <- getStdGen
  fileContent <- lines <$> readFile (filepath input)
  let list = parseFileContent fileContent
  let centroids = randomCentroid (nbColors input) list g
  let newCentroids = getNewCentroid (createClusters list centroids)
  displayClusters (createClusters list centroids) newCentroids

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> help
    _ -> maybe help start (getInput args)
