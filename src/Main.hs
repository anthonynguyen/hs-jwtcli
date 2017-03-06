module Main where

import Data.Char (chr,digitToInt,intToDigit,ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust,isNothing,mapMaybe)
import Numeric (readInt,showIntAtBase)
import System.Exit (ExitCode(ExitFailure),exitWith)

data JWT = JWT { header :: String
               , payload :: String
               , signature :: String
               } deriving (Show)

main :: IO ()
main = do
    line <- getLine
    let jwt = parseJWT line

    if isNothing jwt
        then do
            putStrLn "invalid jwt"
            exitWith (ExitFailure 1)
        else do
            let j = fromJust jwt
            putStrLn $ header j
            putStrLn $ payload j

--------------------------------------------------------------------------------

parseJWT :: String -> Maybe JWT
parseJWT xs
  | length decoded /= 3 = Nothing
  | otherwise = Just $ JWT (head decoded) (decoded !! 1) (last decoded)
    where parts = split (== '.') xs
          decoded = mapMaybe base64decode parts

split :: (a -> Bool) -> [a] -> [[a]]
split p xs = snd folded ++ [fst folded]
    where folded = foldl helper ([], []) xs
          helper acc x
            | (p x) = ([], snd acc ++ [fst acc])
            | otherwise = (fst acc ++ [x], snd acc)

--------------------------------------------------------------------------------

base64chars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_"

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
base64decode xs_
  | any (not . (flip elem) ('=':base64chars)) xs = Nothing
  | otherwise = Just $ unpad $ map chr $ map unbits $ splitEvery 8 
                $ concat $ bitsn 6 $ map charToIndex xxs
    where xs = filter (/= '\n') xs_
          pad = length $ takeWhile (== '=') $ reverse xs
          xxs = reverse $ (replicate pad 'A') ++ (drop pad $ reverse xs)
          unpad = reverse . dropWhile (== '\0') . drop pad . reverse

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
