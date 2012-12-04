import Test.QuickCheck

type Life = Int

{-
Returns if an entity is alive, based on its life value.
-}
isEntityAlive :: Life -> Bool
isEntityAlive life = life > 0

{-
An entity with zero life dies.
-}
testEntityWithZeroLifeIsDead = not $ isEntityAlive 0

{-
An entity with positive life is alive.
-}
testEntityWithLifeIsAlive life = life > 0 ==> isEntityAlive life

{-
An entity with negative life is dead.
-}
testEntityWithNegativeLifeIsDead life = life < 0 ==> not $ isEntityAlive life

{-
An entity should reduce the life of another entity.
-}
testEntityDamageEntity life damage = life - damage == damageEntity life damage
  where
    damageEntity :: Life -> Int -> Life
    damageEntity life damage = life - damage

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
  putStrLn "Testing testEntityDamageEntity"
  quickCheck testEntityDamageEntity
