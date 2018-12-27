{-# OPTIONS_GHC -Wall #-}

module Lib where

data NTurn = Z | O | T | Th

data Turn = NoTurn | CW | CCW | And Turn Turn deriving (Eq, Show)

assoc :: Turn -> Turn
assoc (And x y) =
  let x' = assoc x
  in case x' of
    (And a b) -> And a (And b y)
    _ -> And x' y
assoc x = x

iso :: Turn -> Turn -> Bool
iso x y = assoc x == assoc y

instance Semigroup Turn where
    x <> y = And x y

instance Monoid Turn where
   mempty = NoTurn

naiveInverse :: Turn -> Turn -> Bool
naiveInverse CW CCW = True
naiveInverse CCW CW = True
naiveInverse _ _ = False

reduceByInv :: Turn -> Turn
reduceByInv (And x a@(And y z)) =
  if naiveInverse x y
  then reduceByInv z
  else And x (reduceByInv a)
reduceByInv (And x y) =
  if naiveInverse x y
  then NoTurn
  else And x (reduceByInv y)
reduceByInv x = x

reduceById :: Turn -> Turn
reduceById (And NoTurn x) = reduceById x
reduceById (And x NoTurn) = reduceById x
reduceById (And x y) = And (reduceById x) (reduceById y)
reduceById x = x

reduceByRing :: Turn -> Turn
reduceByRing (And a b'@(And b (And c (And d e)))) =
  if a == b && b == c && c == d
  then reduceByRing e
  else And a (reduceByRing b')
reduceByRing (And a (And b (And c d))) =
  if a == b && b == c && c == d
  then NoTurn
  else And a (reduceByRing b')


-- cw <> cw <> cw <> cw = NoTurn
-- NoTurn <> NoTurn <> NoTurn <> NoTurn = NoTurn
-- ccw <> ccw <> ccw <> ccw = NoTurn
