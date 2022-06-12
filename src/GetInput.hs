--
-- EPITECH PROJECT, 2021
-- B-FUN-400-PAR-4-1-compressor-samuel.shemtov [WSL: Ubuntu-20.04]
-- File description:
-- parse.hs
--

module GetInput
  ( getInput,
  )
where

import Text.Read (readMaybe)
import Types (Input (..))

getInput :: [String] -> Maybe Input
getInput args = checkError split
  where
    input = Input (-1) (-1) ""
    split = parse input args

checkError :: Maybe Input -> Maybe Input
checkError (Just (Input n l f))
  | n <= 0 = Nothing
  | l < 0 = Nothing
  | null f = Nothing
  | otherwise = Just (Input n l f)
checkError Nothing = Nothing

parse :: Input -> [String] -> Maybe Input
parse args [] = Just args
parse _ [_] = Nothing
parse (Input _ l f) ("-n" : x : xs) = case readMaybe x :: Maybe Int of
  Just nbColors -> parse (Input nbColors l f) xs
  Nothing -> Nothing
parse (Input n _ f) ("-l" : x : xs) = case readMaybe x :: Maybe Float of
  Just convergenceLimit -> parse (Input n convergenceLimit f) xs
  Nothing -> Nothing
parse (Input n l _) ("-f" : x : xs) = parse (Input n l x) xs
parse _ _ = Nothing