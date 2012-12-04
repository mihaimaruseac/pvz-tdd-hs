import Test.QuickCheck

{-
Returns if a plant is alive, based on its life value.
-}
isPlantAlive :: Int -> Bool
isPlantAlive life = life > 0

{-
Tests if a plant with zero life dies.
-}
testPlantWithZeroLifeIsDead = not $ isPlantAlive 0

{-
Tests if a plant with positive life is alive.
-}
testPlantWithLifeIsAlive life = life > 0 ==> isPlantAlive life

{-
Tests if a plant with negative life is dead.
-}
testPlantWithNegativeLifeIsDead life = life < 0 ==> not $ isPlantAlive life

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
