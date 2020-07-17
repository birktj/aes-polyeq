import Data.Bits
import Data.Ratio
import Data.List
import Data.Array

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

data PolyF2 = PolyF2 F2 [[Int]]
    deriving (Eq)

polyF2 :: [[Int]] -> F2 -> PolyF2
polyF2 xss b = PolyF2 b . map head . filter (odd . length) . group . sort
             $ map (map head . group . sort) xss

var :: Int -> PolyF2
var n = PolyF2 0 [[n]]

polyDegree :: PolyF2 -> Int
polyDegree (PolyF2 _ xss) = maximum $ map length xss

instance Show PolyF2 where
    show (PolyF2 b [])  = show b
    show (PolyF2 b xss) = intercalate " + " $ vars ++ bs
        where
            bs | b == 0 = []
               | b == 1 = ["1"]
            vars = map (intercalate "*" . map showAtom . group) xss
            showAtom [x] = showVar x
            showAtom xs@(x:_) = showVar x ++ "^" ++ show (length xs)
            showVar n = "var " ++ show n

instance Num PolyF2 where
    (PolyF2 b1 xss1) + (PolyF2 b2 xss2) = polyF2 (xss1 ++ xss2) (b1 + b2)
    a - b = a + b
    (PolyF2 b1 xss1) * (PolyF2 b2 xss2) = polyF2 xss' b'
        where
            xss' = concat [map (xs++) xss2 ++ if b2 == 1 then [xs] else [] | xs <- xss1]
                ++ if b1 == 1 then xss2 else []
            b' = b1 * b2
    fromInteger n = polyF2 [] $ fromInteger n
    abs = undefined
    signum = undefined
    
evaluate :: Num a => (Int -> a) -> PolyF2 -> a
evaluate f (PolyF2 b xss) = b' + sum (map (product . map f) xss)
    where
        b' = if b == 0 then 0 else 1

newtype PolySystem = PolySystem (Array Int PolyF2)
    deriving (Eq)

polySystem :: [PolyF2] -> PolySystem
polySystem xs = PolySystem $ listArray (0, length xs - 1) xs

polySystemList :: PolySystem -> [PolyF2]
polySystemList (PolySystem xs) = elems xs

instance Show PolySystem where
    show (PolySystem xs) = "polySystem " ++ show (elems xs)

(<.>) :: PolySystem -> PolySystem -> PolySystem
(PolySystem as) <.> (PolySystem bs) = PolySystem cs
    where
        cs = fmap (\a -> evaluate (bs !) a) as

type Poly8 = PolySystem

toPoly8 :: Int -> Poly8
toPoly8 x = PolySystem ys
    where
        ys = listArray (0, 7) [if testBit x (7 - i) then 1 else 0 | i <- [0..7]]

poly8Var :: Poly8
poly8Var = PolySystem $ listArray (0, 7) [var i | i <- [0..7]]

notPolyF2 :: PolyF2 -> PolyF2
notPolyF2 x = x - 1

andPolyF2 :: PolyF2 -> PolyF2 -> PolyF2
andPolyF2 x y = x * y

orPolyF2 :: PolyF2 -> PolyF2 -> PolyF2
orPolyF2 x y = x*y + x + y

addPoly8 :: Poly8 -> Poly8 -> Poly8
addPoly8 (PolySystem as) (PolySystem bs) = PolySystem cs
    where
        carry = listArray (0, 7)
              $ [as ! i * bs ! i + as ! i * carry ! i + bs ! i * carry ! i | i <- [1..7]]
             ++ [0]
        cs = listArray (0, 7) [as ! i + bs ! i + carry ! i | i <- [0..7]]

xorPoly8 :: Poly8 -> Poly8 -> Poly8
xorPoly8 (PolySystem as) (PolySystem bs) = PolySystem cs
    where
        cs = listArray (0, 7) [as ! i + bs ! i | i <- [0..7]]

andPoly8 :: Poly8 -> Poly8 -> Poly8
andPoly8 (PolySystem as) (PolySystem bs) = PolySystem cs
    where
        cs = listArray (0, 7) [as ! i * bs ! i | i <- [0..7]]

rshiftPoly8 :: Poly8 -> Int -> Poly8
rshiftPoly8 (PolySystem xs) n = PolySystem ys
    where
        ys = listArray (0, 7) [xs ! (max 0 $ i - n) | i <- [0..7]]

lshiftPoly8 :: Poly8 -> Int -> Poly8
lshiftPoly8 (PolySystem xs) n = PolySystem ys
    where
        ys = listArray (0, 7) [if i + n < 8 then xs ! (max 0 $ i + n) else 0 | i <- [0..7]]
        

lookupPoly8 :: [Poly8] -> Poly8 -> Poly8
lookupPoly8 table (PolySystem xs) = PolySystem ys
    where
        ys = listArray (0, 7)
             [sum . zipWith (*) (map matchByte [0..]) $ getBit i | i <- [0..7]]
        getBit i = map (\(PolySystem zs) -> zs ! i) table
        matchByte b = product [matchBit (xs ! i) (testBit (b :: Int) (7 - i)) | i <- [0..7]]
        matchBit x True  = x
        matchBit x False = x - 1

subPoly8 :: Poly8 -> Poly8
subPoly8 = lookupPoly8 aesSBox

type AesState = Array (Int, Int) Poly8

mkAesState :: [Poly8] -> AesState
mkAesState = array ((0,0),(3,3)) . zip [(i, j) | j <- [0..3], i <- [0..3]] 

subBytes :: AesState -> AesState
subBytes = fmap subPoly8

shiftRows :: AesState -> AesState
shiftRows xss = listArray ix . map (\(i, j) -> xss ! (mod (i + j) 4, j)) $ range ix
    where
        ix = ((0,0), (3,3))

mixColumns :: AesState -> AesState
mixColumns xss = array (bounds xss) . concat
               $ map (\c -> zipWith (\r v -> ((r, c), v)) [0..3] $ mixColumn c) [0..3]
    where
        mixColumn c = r
            where
                a = [xss ! (i, c) | i <- [0..3]]
                b = [lshiftPoly8 r 1 `xorPoly8` (toPoly8 0x1b `andPoly8` (rshiftPoly8 r 7))
                     | i <- [0..3], let r = xss ! (i, c)]
                r = map (foldr1 xorPoly8) [
                        [b !! 0, a !! 3, a !! 2, b !! 1, a !! 1],
                        [b !! 1, a !! 0, a !! 3, b !! 2, a !! 2],
                        [b !! 2, a !! 1, a !! 0, b !! 3, a !! 3],
                        [b !! 3, a !! 2, a !! 1, b !! 0, a !! 0]
                    ]

addRoundKey :: Int -> [AesState] -> AesState -> AesState
addRoundKey r ks s = listArray ix $ zipWith xorPoly8 (elems s) (elems $ ks !! r)
    where
        ix = bounds s

rotWord :: [Poly8] -> [Poly8]
rotWord [b0, b1, b2, b3] = [b1, b2, b3, b0]

subWord :: [Poly8] -> [Poly8]
subWord = map subPoly8

xorWord :: [Poly8] -> [Poly8] -> [Poly8]
xorWord = zipWith xorPoly8


keySchedule :: AesState -> [AesState]
keySchedule key = states $ concat ws
    where
        states xs = mkAesState (take 16 xs) : states (drop 16 xs)
        ix = bounds key
        ks = [[key ! (i, j) | i <- [0..3]] | j <- [0..3]]
        ws = map w [0..]
        w i | i < 4 = ks !! i
            | mod 4 i == 0 = xorWord (ws !! (i - 4))
                           . xorWord (subWord (rotWord (ws !! (i - 1))))
                           $ rcons !! (div i 4)
            | otherwise = xorWord (ws !! (i - 4)) (ws !! (i - 1))
        rcons = map (\rc -> map toPoly8 [rc, 0, 0, 0]) rcs
        rcs = map rc [1..]
        rc 1 = 1
        rc i | rcs !! (i - 2) < 0x80 = 2 * rcs !! (i - 2)
             | otherwise             = 0xff .&. xor 0x1b (2 * rcs !! (i - 2))


aes128 :: AesState -> AesState -> AesState
aes128 k p = addRoundKey 10 ks . shiftRows . subBytes
           . foldr (.) id (map round [1..9])
           $ addRoundKey 0 ks p
    where
        ks = keySchedule k
        round r = addRoundKey r ks . mixColumns . shiftRows . subBytes
 

aesSBox :: [Poly8]
aesSBox = map toPoly8 [
    0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b,
    0xfe, 0xd7, 0xab, 0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0,
    0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4, 0x72, 0xc0, 0xb7, 0xfd, 0x93, 0x26,
    0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71, 0xd8, 0x31, 0x15,
    0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2,
    0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0,
    0x52, 0x3b, 0xd6, 0xb3, 0x29, 0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed,
    0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb, 0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf,
    0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45, 0xf9, 0x02, 0x7f,
    0x50, 0x3c, 0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
    0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec,
    0x5f, 0x97, 0x44, 0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73,
    0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a, 0x90, 0x88, 0x46, 0xee, 0xb8, 0x14,
    0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49, 0x06, 0x24, 0x5c,
    0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 0xe7, 0xc8, 0x37, 0x6d,
    0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08,
    0xba, 0x78, 0x25, 0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f,
    0x4b, 0xbd, 0x8b, 0x8a, 0x70, 0x3e, 0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e,
    0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 0xe1, 0xf8, 0x98, 0x11,
    0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
    0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f,
    0xb0, 0x54, 0xbb, 0x16 ]
