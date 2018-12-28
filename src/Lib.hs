{-# OPTIONS_GHC -Wall #-}

module Lib where

data NTurns = Z | N1 | N2 | N3 deriving Show

data Turn = CW | CCW

turnCW :: NTurns -> NTurns
turnCW Z = N1
turnCW N1 = N2
turnCW N2 = N3
turnCW N3 = Z

turnCCW :: NTurns -> NTurns
turnCCW Z = N3
turnCCW N1 = Z
turnCCW N2 = N1
turnCCW N3 = N2

turn :: Turn -> NTurns -> NTurns
turn CW = turnCW
turn CCW = turnCCW

reduce :: [Turn] -> NTurns
reduce = foldr turn Z
