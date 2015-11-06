import Test.Tasty
import HammingTest

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hammingTests]

