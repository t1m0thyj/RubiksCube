{-# LANGUAGE ScopedTypeVariables #-}

import Test.QuickCheck
import Lib

instance Arbitrary NTurns where
    arbitrary = do
        x :: Int <- arbitrary
        case x `mod` 4 of
            0 -> return Z
            1 -> return N1
            2 -> return N2
            _ -> return N3

turnCW_retracts_turnCCW :: NTurns -> Bool
turnCW_retracts_turnCCW x =
    (turnCW $ turnCCW x) == x

turnCCW_retracts_turnCW :: NTurns -> Bool
turnCCW_retracts_turnCW x =
    (turnCCW $ turnCW x) == x

turnCW_forms_ring :: NTurns -> Bool
turnCW_forms_ring x =
    (turnCW $ turnCW $ turnCW $ turnCW x) == x

turnCCW_forms_ring :: NTurns -> Bool
turnCCW_forms_ring x =
    (turnCCW $ turnCCW $ turnCCW $ turnCCW x) == x

main :: IO ()
main = do
    quickCheck turnCW_retracts_turnCCW
    quickCheck turnCCW_retracts_turnCW
    quickCheck turnCW_forms_ring
    quickCheck turnCCW_forms_ring
