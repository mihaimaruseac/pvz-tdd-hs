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
    life <- arbitrarySizedBoundedIntegral
    return $ Z (abs pos) life globalZombieSpeed globalZombieDmg

instance Arbitrary Plant where
  arbitrary = do
    life <- arbitrarySizedBoundedIntegral
    return $ P life

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
An entity with positive life is alive.
-}
testEntityWithLifeIsAliveP P{..}
  = pLife > 0
  ==> isEntityAlive pLife
testEntityWithLifeIsAliveZ Z{..}
  = zLife > 0
  ==> isEntityAlive zLife

{-
An entity with non-positive life is dead.
-}
testEntityWithNegativeLifeIsDeadP P{..}
  = pLife <= 0
  ==> not $ isEntityAlive pLife
testEntityWithNegativeLifeIsDeadZ Z{..}
  = zLife <= 0
  ==> not $ isEntityAlive zLife

{-
An entity should reduce the life of another entity.
-}
testEntityDamageEntityPZ P{..} Z{..} = pLife - zDmg == damageEntity pLife zDmg
testEntityDamageEntityZB Z{..} B{..} = zLife - bDmg == damageEntity zLife bDmg

{-
Second elapsed for entity: the position should increase by entity's speed.
-}
testSecondElapsedZ Z{..} = zPos + zSpeed == increasePosition zPos zSpeed
testSecondElapsedB B{..} = bPos + bSpeed == increasePosition bPos bSpeed

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
  putStrLn "Testing testEntityWithLifeIsAliveP"
  quickCheck testEntityWithLifeIsAliveP
  putStrLn "Testing testEntityWithLifeIsAliveZ"
  quickCheck testEntityWithLifeIsAliveZ
  putStrLn "Testing testEntityWithNegativeLifeIsDeadP"
  quickCheck testEntityWithNegativeLifeIsDeadP
  putStrLn "Testing testEntityWithNegativeLifeIsDeadZ"
  quickCheck testEntityWithNegativeLifeIsDeadZ
  putStrLn "Testing testEntityDamageEntityPZ"
  quickCheck testEntityDamageEntityPZ
  putStrLn "Testing testEntityDamageEntityZB"
  quickCheck testEntityDamageEntityZB
  putStrLn "Testing testSecondElapsedZ"
  quickCheck testSecondElapsedZ
  putStrLn "Testing testSecondElapsedB"
  quickCheck testSecondElapsedB
  putStrLn "Testing testZombieIsShot"
  quickCheck testZombieShot
  putStrLn "Testing testZombieShot"
  quickCheck testZombieIsShot
  putStrLn "Testing testZombieCanEat"
  quickCheck testZombieCanEat
  putStrLn "Testing testZombieEatsPlant"
  quickCheck testZombieEatsPlant
