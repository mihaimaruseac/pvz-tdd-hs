import Test.QuickCheck

{-
Tests if a plant with zero life dies.
-}
testPlantWithZeroLifeIsDead = isPlantDead 0
  where
    isPlantDead :: Int -> Bool
    isPlantDead life = life == 0

{-
Tests if a plant with positive life is alive.
-}
testPlantWithLifeIsAlive life = life > 0 ==> isPlantAlive life
  where
    isPlantAlive :: Int -> Bool
    isPlantAlive life = life > 0

{-
Tests if a plant with negative life is dead.
-}
testPlantWithNegativeLifeIsDead life = life < 0 ==> isPlantDead life
  where
    isPlantDead :: Int -> Bool
    isPlantDead life = life < 0

{-
Test all properties in a single run.
-}
testAll = do
  putStrLn "Testing testPlantWithZeroLifeIsDead"
  quickCheck testPlantWithZeroLifeIsDead
  putStrLn "Testing testPlantWithLifeIsAlive"
  quickCheck testPlantWithLifeIsAlive
  putStrLn "Testing testPlantWithNegativeLifeIsDead"
  quickCheck testPlantWithNegativeLifeIsDead
