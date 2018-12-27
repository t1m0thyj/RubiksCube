{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Test.QuickCheck
import Lib

instance Arbitrary Turn where
  arbitrary = do
    x :: Int <- arbitrary
    case x `mod` 4 of
      0 -> return NoTurn
      1 -> return CW
      2 -> return CCW
      _ -> do
        a <- arbitrary
        b <- arbitrary
        return $ And a b

swapAssoc :: Turn -> Turn
swapAssoc (And x (And y z)) =
  And (And (swapAssoc x) (swapAssoc y)) (swapAssoc z)
swapAssoc (And (And x y) z) =
  And (swapAssoc x) (And (swapAssoc y) (swapAssoc z))
swapAssoc (And x y) = And (swapAssoc x) (swapAssoc y)
swapAssoc x = x

reduceByInv_respects_associativity :: Turn -> Bool
reduceByInv_respects_associativity t =
    let t' = swapAssoc t
        reducedT = reduceByInv $ assoc t
        reducedT' = reduceByInv $ assoc t'
    in iso reducedT reducedT'

main :: IO ()
main = quickCheck reduceByInv_respects_associativity
