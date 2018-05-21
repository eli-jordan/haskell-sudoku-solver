
import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import Control.Exception

test1 = testCase "Test 1" $ assertEqual "1 == 2" 1 2

test2 = testCase "Test 2" $ assertEqual "1 == 1" 1 1

test3 = testCase "Multiple Assertions" $ do
  assertEqual "1 == 1" 1 1
  assertEqual "2 == 2" 2 2

main :: IO ()
main = defaultMain $ testGroup "\nSpecs"
   [
   test1
 , test2
 , test3
   ]
