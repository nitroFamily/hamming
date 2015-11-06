module Codes.Bin (
      Bin (..)
    , from
    ) where

-- | 'Bin' это обертка для 'Bool'
newtype Bin = Bin Bool
            deriving Eq

-- | Определение экземпляра для класса типов 'Show'
instance Show Bin where
    show (Bin True)  = show 1
    show (Bin False) = show 0


-- | Определение экземпляра для класса типов 'Num'.
instance Num Bin where
    (+) (Bin a) (Bin b) = Bin (not a && b || a && not b)
    (*) (Bin a) (Bin b) = Bin (a && b)
    abs a = a
    signum a = a
    fromInteger i | i `mod` 2 == 1 = Bin True
                  | otherwise = Bin False
    negate (Bin a) = Bin (not a)

-- | Генерация @[Bin]@ из @[Integer]@
from :: [Integer] -> [Bin]
from = map fromInteger
