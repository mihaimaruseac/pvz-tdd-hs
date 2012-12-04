import Test.QuickCheck

{-
Tests if a plant with zero life dies.
-}
testPlantWithZeroLifeIsDead = isPlantDead 0
  where
    isPlantDead :: Int -> Bool
    isPlantDead life = False
