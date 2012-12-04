{-# LANGUAGE RecordWildCards #-}

import Test.QuickCheck

type Life = Int
type Position = Int
type Speed = Int
type Damage = Int

data Zombie = Z
  { zPos :: Position
  , zLife :: Life
  , zSpeed :: Speed
  , zDmg :: Damage
  } deriving (Eq, Show)

data Plant = P
  { pLife :: Life
  } deriving (Eq, Show)

data Bullet = B
  { bPos :: Position
  , bSpeed :: Speed
  , bDmg :: Damage
  } deriving (Eq, Show)

globalZombieSpeed = 20
globalZombieDmg = 1
globalBulletSpeed = 300
globalBulletDmg = 1

instance Arbitrary Zombie where
  arbitrary = do
    pos <- arbitrary
    life <- arbitrary
    return $ Z (abs pos) (abs life) globalZombieSpeed globalZombieDmg

instance Arbitrary Plant where
  arbitrary = do
    life <- arbitrary
    return $ P (abs life)

instance Arbitrary Bullet where
  arbitrary = do
    pos <- arbitrary
    return $ B (abs pos) globalBulletSpeed globalBulletDmg

{-
Returns if an entity is alive, based on its life value.
-}
isEntityAlive :: Life -> Bool
isEntityAlive life = life > 0

{-
Modifies the position of a bullet of zombie.
-}
increasePosition :: Position -> Speed -> Position
increasePosition position speed = position + speed

{-
Tests if a zombie is hit by a projectile.
-}
isZombieHit :: Position -> Position -> Bool
isZombieHit pp zp = pp >= zp

{-
Returns if a zombie is near a plant.
-}
zombieNearPlant :: Position -> Bool
zombieNearPlant zp = zp == 0

{-
Does a damage to one entity.
-}
damageEntity :: Life -> Damage -> Life
damageEntity life damage = life - damage

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

{-
Second elapsed for entity: the position should increase by entity's speed.
-}
testSecondElapsed position speed = position + speed == increasePosition position speed

{-
A projectile hits a zombie if its position is greater or equal than zombie's.
-}
testZombieIsShot Z{..} B{..}
  = bPos >= zPos
  ==> isZombieHit bPos zPos

{-
When a zombie is hit, it's life is decreased.
-}
testZombieShot Z{..} B{..}
  = isZombieHit bPos zPos
  ==> zLife - bDmg == damageEntity zLife bDmg

{-
A zombie can eat a plant if it is near it.
-}
testZombieCanEat Z{..}
  = zPos == 0
  ==> zombieNearPlant zPos

{-
When a zombie is near the plant, he eats it.
-}
testZombieEatsPlant Z{..} P{..}
  = zombieNearPlant zPos
  ==> pLife - zDmg == damageEntity pLife zDmg

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
  putStrLn "Testing testZombieEatsPlant"
  quickCheck testZombieEatsPlant
