module Main where

import Data.Char (chr,digitToInt,intToDigit,ord)
import Data.List (elemIndex)
import Numeric (readInt,showIntAtBase)

main :: IO ()
main = do
    putStrLn $ base64encode "test"
    putStrLn $ show $ base64decode "dGVzdA=="
    putStrLn $ show $ base64decode "YXNkZg=="
    putStrLn $ show $ base64decode "YXNkZmZ6cA=="

--------------------------------------------------------------------------------

base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64encode :: String -> String
base64encode xs = unpad $ map (base64chars !!) $ map unbits $ splitEvery 6
                  $ concat $ bitsn 8 $ map ord $ xs ++ (replicate pad '\0')
    where r = length xs `rem` 3
          pad
            | r == 1    = 2
            | r == 2    = 1
            | otherwise = 0
          unpad xs = reverse $ (replicate pad '=') ++ (drop pad $ reverse xs)

base64decode :: String -> Maybe String
base64decode xs
  | any (not . (flip elem) ('=':base64chars)) xs = Nothing
  | otherwise = Just $ unpad $ map chr $ map unbits $ splitEvery 8 
                $ concat $ bitsn 6 $ map charToIndex xxs
    where pad = length $ takeWhile (== '=') $ reverse xs
          xxs = reverse $ (replicate pad 'A') ++ (drop pad $ reverse xs)
          unpad = reverse . drop pad . reverse

-- this function should only be used once we've verified our string
charToIndex :: Char -> Int
charToIndex c = val
    where (Just val) = elemIndex c base64chars

bitsn :: Int -> [Int] -> [String]
bitsn n xs = map lpad $ map (\x -> showIntAtBase 2 intToDigit x "") xs
    where lpad :: String -> String
          lpad xs
            | length xs >= n = xs
            | otherwise      = lpad ('0':xs)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = f:(splitEvery n s)
    where (f, s) = splitAt n xs

unbits :: String -> Int
unbits = fst . head . readInt 2 (`elem` "01") digitToInt
