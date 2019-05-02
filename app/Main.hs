module Main where

import System.Random

checkCard :: Int -> (Int, Int) -> Bool
checkCard num (front, back) = num == front || num == back

generateCards :: StdGen -> Int -> [(Int, Int)]
generateCards gen amount = map ([(0, 0), (0, 1), (1, 1)] !!) $ take amount $ randomRs (0 :: Int, 2 :: Int) gen

getDecimal :: Fractional a => StdGen -> Int -> a
getDecimal gen amount = (fromIntegral $ length $ filter (checkCard $ head $ randomRs (0 :: Int, 1 :: Int) gen) $ generateCards gen amount) / (fromIntegral amount)

getPercentage :: StdGen -> Int -> String
getPercentage gen amount = (show $ (fromIntegral $ round $ (getDecimal gen amount) * 10000) / (fromIntegral 100)) ++ "%"

main = do
    gen <- newStdGen
    print $ getPercentage gen amount

amount :: Int
amount = 1000000