import Test.QuickCheck

{-
Tests if a plant with zero life dies.
-}
testPlantWithZeroLifeIsDead = isPlantDead 0
  where
    isPlantDead :: Int -> Bool
    isPlantDead life = life == 0

{-
Tests if a plant with non-zero life is alive.
-}
testPlantWithLifeIsAlive = False

{-
Enumeration containing all tests.
-}
allTests =
  [ (testPlantWithZeroLifeIsDead, "testPlantWithZeroLifeIsDead")
  , (testPlantWithLifeIsAlive, "testPlantWithLifeIsAlive")
  ]

testOne (tf, tn) = do
  putStrLn $ "Testing " ++ tn
  quickCheck tf

testAll = mapM_ testOne allTests
