import Test.QuickCheck

{-
Returns if a plant is alive, based on its life value.
-}
isPlantAlive :: Int -> Bool
isPlantAlive life = life > 0

{-
Returns if a zombie is alive, based on its life value.
-}
isZombieAlive :: Int -> Bool
isZombieAlive life = life > 0

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
Tests if a zombie with zero life dies.
-}
testZombieWithZeroLifeIsDead = not $ isZombieAlive 0

{-
Tests if a zombie with positive life is alive.
-}
testZombieWithLifeIsAlive life = life > 0 ==> isZombieAlive life

{-
Tests if a zombie with negative life is dead.
-}
testZombieWithNegativeLifeIsDead life = life < 0 ==> not $ isZombieAlive life

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
  putStrLn "Testing testZombieWithZeroLifeIsDead"
  quickCheck testZombieWithZeroLifeIsDead
  putStrLn "Testing testZombieWithLifeIsAlive"
  quickCheck testZombieWithLifeIsAlive
  putStrLn "Testing testZombieWithNegativeLifeIsDead"
  quickCheck testZombieWithNegativeLifeIsDead
