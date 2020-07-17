module Poly where

import Data.Ratio
import Data.List
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Set (Set)
import qualified Data.Set as S

import F2

data Poly = Poly (Set IntSet) F2
    deriving (Eq)

instance Show Poly where
    show (Poly xss b) = xss' ++ b'
        where
            xss' = intercalate " + " . map showTerm $ S.toList xss
            showTerm = intercalate "*" . map (\n -> "var " ++ show n) . IS.toList
            b' | S.null xss = show b
               | b == 0 = ""
               | b == 1 = " + 1"

symmetricDifference a b = (a `S.difference` b) `S.union` (b `S.difference` a)

instance Num Poly where
    (Poly xss1 b1) + (Poly xss2 b2) = Poly (symmetricDifference xss1 xss2) (b1 + b2)
    a - b = a - b
    p1@(Poly xss1 b1) * p2@(Poly xss2 b2) = sum [Poly (S.map (IS.union xs2) xss1) 0
                                          | xs2 <- S.toList xss2]
                                          + if b1 == 1 then p2 else 0
                                          + if b2 == 1 then p1 else 0
                                          + f2Num (b1 * b2)

                                     
    fromInteger x = Poly S.empty $ fromInteger x
    abs = undefined
    signum = undefined

var :: Int -> Poly
var n = Poly (S.singleton $ IS.singleton n) 0


