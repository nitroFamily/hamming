import Test.Tasty
import Test.Tasty.HUnit

import Data.Matrix
import Codes.Hamming
import Codes.Bin

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [hammingTests]

hammingTests :: TestTree
hammingTests = testGroup "Hamming Tests"
    [ multMatrix
    , encodeDecodeTest
    , correctOneErr
    , wrongCorrect ]

multMatrix :: TestTree
multMatrix = testCase "G * H^T == [0]" $
    getg `multStd` getht @?= getz

encodeDecodeTest :: TestTree
encodeDecodeTest = testCase "encode - decode without errors" $
    snd (decode hc $ encode hc msg) @?= msg

correctOneErr:: TestTree
correctOneErr = testCase "correct 1 error" $
    decode hc (adderr $ encode hc msg) @?= (syndrom, msg)

wrongCorrect :: TestTree
wrongCorrect = testCase "wrong correct for several errors" $
    (snd (decode hc $ add2err $ encode hc msg) /= msg) @? ""

hc :: HammingCode
hc = hamming 11

getg :: Matrix Bin
getg = getG $ hamming 11

getht :: Matrix Bin
getht = transpose . getH $ hamming 11

getz :: Matrix Bin
getz = zero 11 4

msg :: [Bin]
msg = [ Bin True, Bin False, Bin True, Bin False, Bin False
      , Bin True, Bin False, Bin True, Bin False, Bin False
      , Bin True ]

adderr :: [Bin] -> [Bin]
adderr (x:xs) = negate x : xs

add2err :: [Bin] -> [Bin]
add2err (x:x2:xs) = negate x : negate x2 : xs

syndrom :: [Bin]
syndrom = [Bin True, Bin False, Bin False, Bin False]


