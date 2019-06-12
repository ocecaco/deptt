import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testCase "Dummy test" $
      (1 :: Int) + 1 @?= 2
  ]
