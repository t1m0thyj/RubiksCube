module Main where

import Lib

main :: IO ()
main = putStrLn $ show $ reduce [CW, CCW, CCW]
