{-
Author: Adam Howell
Company: Student
Email: adam.dhowell.19@gmail.com
-}
{-# LANGUAGE GADTs, KindSignatures #-}
import Data.Char
import Data.List
import System.IO

torn :: String->Maybe Int
torn str = case odd (length str) of
				True  -> Nothing
				False -> do 
							let i     = read str :: Int
							let (a,b) = (read (take (quot (length str) 2) str) :: Int,read (drop (quot (length str) 2) str) :: Int)
							case (a+b)^2 == i of
						 		True  -> Just i
						 		False -> Nothing

getIs :: Maybe Int->Int
getIs mi = case mi of
			Just i  -> i
			Nothing -> 0

main :: IO()
main = do
		let x = map show [1..1000000]
		putStrLn $ show (filter (\x -> x/=0) (map getIs (map torn x)))