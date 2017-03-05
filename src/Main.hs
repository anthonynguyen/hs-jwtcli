module Main where

import Data.Char (digitToInt,intToDigit,ord)
import Numeric (readInt,showIntAtBase)

main :: IO ()
main = do
    putStrLn $ base64encode "test"

--------------------------------------------------------------------------------

base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"

base64encode :: String -> String
base64encode xs = unpad $ map (base64chars !!) $ map unbits6 $ splitEvery 6
                  $ concat $ bits8 $ map ord $ xs ++ (replicate pad '\0')
    where r = length xs `rem` 3
          pad
            | r == 1    = 2
            | r == 2    = 1
            | otherwise = 0
          unpad :: String -> String
          unpad xs = reverse $ (replicate pad '=') ++ (drop pad $ reverse xs)

bits8 :: [Int] -> [String]
bits8 = map lpad . map (\x -> showIntAtBase 2 intToDigit x "")
    where lpad :: String -> String
          lpad xs
            | length xs >= 8 = xs
            | otherwise      = lpad ('0':xs)

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = f:(splitEvery n s)
    where (f, s) = splitAt n xs

unbits6 :: String -> Int
unbits6 = fst . head . readInt 2 (`elem` "01") digitToInt
