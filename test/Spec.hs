module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Data.List.Split
import DAOTest (daoTest)
import qualified Model.Lib as LB (version)


versionTest :: TestTree
versionTest = testCase "Testing version number" $ 
    assertEqual "Version number should be a three dot number" 3 $ length $ splitOn "." LB.version

libTest :: TestTree
libTest = testGroup "Lib Test" [versionTest]

tests :: TestTree
tests = testGroup "Client Tests" [libTest, daoTest]

main :: IO ()
main = defaultMain tests