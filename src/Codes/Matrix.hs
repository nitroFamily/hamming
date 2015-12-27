module Codes.Matrix (
      removeCol
    , removeCols
    , insertCol
    , insertCols
    , split
    ) where

import Data.Matrix

removeCol :: Int
          -> Matrix a
          -> (Matrix a, Matrix a)
removeCol n mx
    | n == 1    = ( removedCol, submatrix' 2 cols mx )
    | n == cols = ( removedCol, submatrix' 1 (n - 1) mx )
    | otherwise = ( removedCol, submatrix' 1 (n - 1) mx <|> submatrix' (n + 1) cols mx )
  where
    rows = nrows mx
    cols = ncols mx
    removedCol = submatrix 1 rows n n mx
    submatrix' = submatrix 1 rows

removeCols :: [Int]
           -> Matrix a
           -> ([Matrix a], Matrix a)
removeCols ns mx = foldr func ([], mx) ns
    where
      func :: Int -> ([Matrix a], Matrix a) -> ([Matrix a], Matrix a)
      func n (removed, m) =
          let (r, m') = removeCol n m
           in (r : removed, m')

insertCol :: (Int, Matrix a)
          -> Matrix a
          -> Matrix a
insertCol (n, col) mx
    | n == 1        = col <|> mx
    | n == cols + 1 = mx <|> col
    | otherwise     = submatrix 1 rows 1 (n - 1) mx <|>
                      col <|> submatrix 1 rows n cols mx
  where
    rows = nrows mx
    cols = ncols mx

insertCols :: [(Int, Matrix a)]
           -> Matrix a
           -> Matrix a
insertCols ns mx = foldl (flip insertCol) mx ns

split :: Matrix a
      -> [Matrix a]
split mx = foldr (\i acc -> fst (removeCol i mx) : acc) [] [1,2 .. ncols mx]
