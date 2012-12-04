import Test.QuickCheck

type Life = Int

{-
Returns if an entity is alive, based on its life value.
-}
isEntityAlive :: Life -> Bool
isEntityAlive life = life > 0

{-
Tests if an entity with zero life dies.
-}
testEntityWithZeroLifeIsDead = not $ isEntityAlive 0

{-
Tests if an entity with positive life is alive.
-}
testEntityWithLifeIsAlive life = life > 0 ==> isEntityAlive life

{-
Tests if an entity with negative life is dead.
-}
testEntityWithNegativeLifeIsDead life = life < 0 ==> not $ isEntityAlive life

{-
Test all properties in a single run.
-}
testAll = do
  putStrLn "Testing testEntityWithZeroLifeIsDead"
  quickCheck testEntityWithZeroLifeIsDead
  putStrLn "Testing testEntityWithLifeIsAlive"
  quickCheck testEntityWithLifeIsAlive
  putStrLn "Testing testEntityWithNegativeLifeIsDead"
  quickCheck testEntityWithNegativeLifeIsDead
