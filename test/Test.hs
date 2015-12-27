module Main where

import Test.HUnit

import Codes.Hamming

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList
    [ TestLabel "decode $ encode msg = msg" encodeDecode
    ]

encodeDecode :: Test
encodeDecode = TestCase $
    (snd . decode hc $ encode hc msg) @?= msg

hc :: HammingCode
hc = hamming 11

msg :: [Int]
msg = [0,1,1,0,1,1,1,0,0,1,0]


