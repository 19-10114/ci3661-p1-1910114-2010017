module Main where

import Test.HUnit

testInicial :: Test
testInicial = TestCase (assertEqual "Inicialización básica" 1 (1 :: Int))

tests :: Test
tests = TestList [TestLabel "testInicial" testInicial]

main :: IO ()
main = do
  _ <- runTestTT tests
  return ()
