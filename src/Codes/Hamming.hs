module Codes.Hamming
    (
      hammingCodes
    ) where

import Data.Matrix

type Code = (Int, Int)

data HammingCode = HammingCode
    { code :: Code
    , rate :: Float
    , getH :: Matrix Int
    } deriving Show

hammingCodes :: [Code]
hammingCodes = iterate next (3, 1)
  where
    next :: Code -> Code
    next (n, k) =
        let m = (n - k) + 1
         in (2 ^ m - 1, 2 ^ m - m - 1)

parityPos :: [Int]
parityPos = map (\x -> 2 ^ x) [0, 1 ..]

parityLaws :: Int -> [Int]
parityLaws p =
    let seq = replicate (p - 1) 0 ++ replicate p 1 ++ [0]
     in concat $ repeat seq

generateH :: Code -> Matrix Int
generateH (n, k) =
    let lists = map (take n . parityLaws) $ take (n - k) parityPos
     in fromLists lists

hamming :: Int -> HammingCode
hamming i =
    let (n, k) = head $ filter (\(_, k) -> k >= i) hammingCodes
        diff = k - i
        c@(n', k') = (n - diff, k - diff)
        h = generateH c
     in HammingCode c (fromIntegral k' / fromIntegral n') h

removeCol :: Int -> Matrix a -> (Matrix a, Matrix a)
removeCol n mx
    | n == 1    = ( removedCol, submatrix' 2 cols mx )
    | n == cols = ( removedCol, submatrix' 1 (n - 1) mx )
    | otherwise = ( removedCol, submatrix' 1 (n - 1) mx <|> submatrix' (n + 1) cols mx )
  where
    rows = nrows mx
    cols = ncols mx
    removedCol = submatrix 1 rows n n mx
    submatrix' = submatrix 1 rows

removeCols :: [Int] -> Matrix a -> ([Matrix a], Matrix a)
removeCols ns mx = foldr func ([], mx) ns
    where
      func :: Int -> ([Matrix a], Matrix a) -> ([Matrix a], Matrix a)
      func n (removed, m) =
          let (r, m') = removeCol n m
           in (r : removed, m')

insertCol :: (Int, Matrix a) -> Matrix a -> Matrix a
insertCol (n, col) mx
    | n == 1        = col <|> mx
    | n == cols + 1 = mx <|> col
    | otherwise     = submatrix 1 rows 1 (n - 1) mx <|>
                      col <|> submatrix 1 rows n cols mx
  where
    rows = nrows mx
    cols = ncols mx

insertCols :: [(Int, Matrix a)] -> Matrix a -> Matrix a
insertCols ns mx = foldl (flip insertCol) mx ns

generateA :: HammingCode -> Matrix Int
generateA hc =
    let h = getH hc
        (n, k) = code hc
        idx = take (n - k) parityPos
     in snd $ removeCols idx h

encode :: HammingCode -> [Int] -> [Int]
encode = error "encode combination"

decode :: HammingCode -> [Int] -> [Int]
decode = error "decode combination"

getP :: HammingCode -> Matrix Int
getP = error "get matrix"
