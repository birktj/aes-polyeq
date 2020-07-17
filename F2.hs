module F2 where

import Data.Ratio
import Data.Bits

newtype F2 = F2 Bool
    deriving (Eq)

instance Show F2 where
    show (F2 False) = "0"
    show (F2 True)  = "1"

instance Num F2 where
    (F2 a) + (F2 b) = F2 $ xor a b
    (F2 a) - (F2 b) = F2 . xor a $ not b
    (F2 a) * (F2 b) = F2 $ a && b
    fromInteger n = F2 $ odd n
    abs x = x
    signum x = x

instance Fractional F2 where
    x / 1 = x
    _ / 0 = error "Cannot divide by zero"
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

f2Num :: Num a => F2 -> a
f2Num (F2 True)  = 1
f2Num (F2 False) = 0

