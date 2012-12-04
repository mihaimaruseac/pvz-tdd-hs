import Test.QuickCheck

type Life = Int
type Position = Int
type Speed = Int
type Damage = Int

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
    damageEntity :: Life -> Damage -> Life
    damageEntity life damage = life - damage

{-
Second elapsed for entity: the position should increase by entity's speed.
-}
testSecondElapsed position speed = position + speed == increasePosition position speed
  where
    increasePosition :: Position -> Speed -> Position
    increasePosition position speed = position + speed

{-
A projectile hits a zombie if its position is greater or equal than zombie's.
-}
testZombieIsShot pp zp = pp >= zp ==> isZombieHit pp zp
  where
    isZombieHit :: Position -> Position -> Bool
    isZombieHit pp zp = pp >= zp

{-
When a zombie is hit, it's life is decreased.
-}
testZombieShot pp zp lifez dmgp = isZombieHit pp zp ==> lifez - dmgp == damageEntity lifez dmgp
  where
    damageEntity :: Life -> Damage -> Life
    damageEntity life damage = life - damage
    isZombieHit :: Position -> Position -> Bool
    isZombieHit pp zp = pp >= zp

{-
A zombie can eat a plant if it is near it.
-}
testZombieCanEat zp = zp == 0 ==> zombieNearPlant zp
  where
    zombieNearPlant :: Position -> Bool
    zombieNearPlant zp = zp == 0

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
  putStrLn "Testing testSecondElapsed"
  quickCheck testSecondElapsed
  putStrLn "Testing testZombieIsShot"
  quickCheck testZombieShot
  putStrLn "Testing testZombieShot"
  quickCheck testZombieIsShot
  putStrLn "Testing testZombieCanEat"
  quickCheck testZombieCanEat
