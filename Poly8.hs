module Poly8 where

import Data.Bits
import Data.List
import Data.Array
import Poly

newtype Poly8 = Poly8 (Array Int Poly)
    deriving (Eq)

poly8 :: [Poly] -> Poly8
poly8 = Poly8 . listArray (0, 7)

polyBit :: Poly -> Poly8
polyBit b = poly8 $ replicate 7 0 ++ [b]

instance Show Poly8 where
    show (Poly8 xs) = "poly8 " ++ show (elems xs)

instance Num Poly8 where
    (Poly8 as) + (Poly8 bs) = Poly8 cs
        where
            carry = listArray (0, 7)
                  $ [as ! i * bs ! i + as ! i * carry ! i + bs ! i * carry ! i | i <- [1..7]]
                 ++ [0]
            cs = listArray (0, 7) [as ! i + bs ! i + carry ! i | i <- [0..7]]
    a - b = a + b
    fromInteger x = Poly8 xs
        where
            xs = listArray (0, 7) [if testBit x (7 - i) then 1 else 0 | i <- [0..7]]
    (Poly8 as) * (Poly8 bs) = sum . map singlePoly8
                            $ sortOn fst [(i+j-7, (as ! i) * (bs ! j)) 
                            | i <- [0..7], j <- [0..7], (7-i) + (7-j) < 8]
        where
            singlePoly8 (i, v) = Poly8 $ listArray (0, 7) (repeat 0) // [(i, v)]
    abs    = undefined
    signum = undefined

instance Bits Poly8 where
    (Poly8 as) .&. (Poly8 bs) = Poly8 cs
        where
            cs = listArray (0, 7) [as ! i * bs ! i| i <- [0..7]]
    a .|. b = (a .&. b) `xor` a `xor` b
    (Poly8 as) `xor` (Poly8 bs) = Poly8 cs
        where
            cs = listArray (0, 7) [as ! i + bs ! i| i <- [0..7]]
    complement (Poly8 xs) = Poly8 $ fmap (+1) xs
    rotate (Poly8 xs) n = Poly8 ys
        where
            ys = listArray (0, 7) [xs ! (mod (i + n) 8) | i <- [0..7]]
    shiftR (Poly8 xs) n = Poly8 ys
        where
            ys = listArray (0, 7) [if i - n < 0 then xs ! 0 else xs ! (i - n) | i <- [0..7]]
    shiftL (Poly8 xs) n = Poly8 ys
        where
            ys = listArray (0, 7) [if i + n > 7 then 0 else xs ! (i + n) | i <- [0..7]]
    bitSize _ = 8
    bitSizeMaybe _ = Just 8
    bit x = Poly8 $ listArray (0, 7) [if x == i then 1 else 0 | i <- [0..7]]
    isSigned = undefined
    testBit  = undefined
    popCount = undefined

eqMask :: Poly8 -> Poly8 -> Poly8 
eqMask (Poly8 as) (Poly8 bs) = Poly8 . listArray (0, 7) . repeat
                             . product . map (+1) $ zipWith (+) (elems as) (elems bs)

poly8ToInteger :: Poly8 -> Maybe Integer
poly8ToInteger (Poly8 xs) = fmap sum . sequence . zipWith mkBit [0..] $ elems xs
    where
        mkBit i p
            | p == 0 = Just 0
            | p == 1 = Just $ 2^(7 - i)
            | otherwise = Nothing
