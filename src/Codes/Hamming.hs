-- | Коды Хэмменга
module Codes.Hamming (
      -- * Тип для кодов Хэмменга
      HammingCode (..)
      -- * Функция строящая код для требуемой длины данных
    , hamming
      -- * Кодирование и Декодирование
    , encode
    , decode
    ) where

import Data.Matrix
import Codes.Matrix

-- | Синонимы типов
type Code = (Int, Int) -- ^ Синоним кортежа обозначающий запись кода в виде @(n, k)@
type Syndrome = [Int]  -- ^ Синоним для массива обозначающий синдром

-- | Тип для кодов Хэмменга
data HammingCode = HammingCode
    { code :: Code       -- ^ Синоним кортежа обозначающий запись кода в виде @(n, k)@
    , rate :: Float      -- ^ Скорость кода
    , getH :: Matrix Int -- ^ Проверочная матрица
    , getG :: Matrix Int -- ^ Порождающая матрица
    } deriving Show

-- | Построитель кода для заданной длины данных.
--   Возвращает тип 'HammingCode', который содержит необходимые матрицы для кодирования и декодирования
--
-- > let hc = hamming 2
-- > code hc -> (5, 2)
-- > getH hc -> ( 1 0 1 0 1 )
-- >            ( 0 1 1 0 0 )
-- >            ( 0 0 0 1 1 )
--
-- > getG hc -> ( 1 1 1 0 0 )
-- >            ( 1 0 0 1 1 )
hamming :: Int -- ^ Длина блока данных, для которого выберется подходящий код
        -> HammingCode
hamming i =
    let (n, k) = head $ filter (\(_, k) -> k >= i) hammingCodes
        diff = k - i
        c@(n', k') = (n - diff, k - diff)
        h = generateH c
        g = generateG c h
     in HammingCode c (fromIntegral k' / fromIntegral n') h g

-- | Кодирование массива данных
--
-- > let hc = hamming 2
-- > let d  = [1,0]
-- > encode hc d -> [1,1,1,0,0]
encode :: HammingCode -- ^ Код, который будет использоваться для кодирования
       -> [Int]       -- ^ Массив данных
       -> [Int]
encode hc d =
    let b = fromList 1 (length d) d
        g = getG hc
     in map (`mod` 2) . toList $ multStd b g

-- | Декодирование данных
--
--  Декодирование без ошибок
--
-- > let hc = hamming 2
-- > let encoded = encode hc [1,0]
-- > decode hs encoded -> ([0,0,0], [1,0])
--
--  Декодирование с 1 ошибкой
--
-- > let hc = hamming 2
-- > encode hc [1,0]       -> [1,1,1,0,0]
-- > decode hs [1,1,1,1,0] -> ([0,0,1],[1,0])
--
--  Декодирование с 2 ошибками
--
-- > let hc = hamming 2
-- > encode hc [1,0]       -> [1,1,1,0,0]
-- > decode hs [1,0,0,0,0] -> ([1,0,0],[0,0])
decode :: HammingCode -- ^ Код, который будет использоваться для декодирования
       -> [Int]       -- ^ Закодированная комбинация
       -> (Syndrome, [Int])
decode hc v =
    let v'     = fromList 1 (length v) v
        (n, k) = code hc
        ht     = transpose $ getH hc
        s      = map (`mod` 2) . toList $ multStd v' ht
        c      = correctError v' s
        w      = toList . snd $ removeCols (take (n - k) parityPos) c
     in (s, w)

-- | Вспомогательные функции

correctError :: Matrix Int
             -> Syndrome
             -> Matrix Int
correctError v s =
    let pos = errorPos s
     in mapCol (\_ x -> negate x) pos v

-- | Генерирует все возможные коды Хэмменга
--
-- > take 4 hammingCodes -> [(3,1),(7,4),(15,11),(31,26)]
hammingCodes :: [Code]
hammingCodes = iterate next (3, 1)
  where
    next :: Code -> Code
    next (n, k) =
        let m = (n - k) + 1
         in (2 ^ m - 1, 2 ^ m - m - 1)

-- | Генерирует позиции проверочных битов
--
-- > take 5 parityPos -> [1,2,4,8,16]
parityPos :: [Int]
parityPos = map (\x -> 2 ^ x) [0, 1 ..]

-- | Генерирует проверочные соотношения
--
-- Проверочные соотношения для кода (5, 2)
--
-- > take 5 $ parityLaws 1 -> [1,0,1,0,1]
-- > take 5 $ parityLaws 2 -> [0,1,1,0,0]
-- > take 5 $ parityLaws 4 -> [0,0,0,1,1]
parityLaws :: Int  -- ^ Номер позиции проверочного бита
           -> [Int]
parityLaws p =
    let seq = replicate (p - 1) 0 ++ replicate p 1 ++ [0]
     in concat $ repeat seq

-- | Генерация проверочной матрицы для данного кода
--
-- Реализация примера из 'parityLaws' для общего слушая
generateH :: Code -- ^ Кортеж (n, k)
          -> Matrix Int
generateH (n, k) =
    let lists = map (take n . parityLaws) $ take (n - k) parityPos
     in fromLists lists

-- | Генерация матрицы А из проверочной матрицы Н
generateA :: Code
          -> Matrix Int
          -> Matrix Int
generateA (n, k) mx =
    let idx = take (n - k) parityPos
     in snd $ removeCols idx mx

-- | Генерация проверочной матрицы из матрицы Н
generateG :: Code
          -> Matrix Int
          -> Matrix Int
generateG c@(n, k) mx =
    let at  = transpose $ generateA c mx
        sat = split at
        ps  = zip parityPos sat
     in insertCols ps $ identity k

-- | Вычисление позиции ошибки по синдрому
--
-- > let syndrome = [1,0,1]
-- > errorPos syndrom -> 5
errorPos :: Syndrome
         -> Int
errorPos s = sum $ zipWith func s parityPos
  where
    func p i | p == 1 = i
             | otherwise = 0

