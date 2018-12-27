module Main where

import Lib

data Nat = Z | S Nat

add :: Nat -> Nat -> Nat
add Z y = y
add (S x) y = S (add x y)

main :: IO ()
main = putStrLn "Aardvark"
