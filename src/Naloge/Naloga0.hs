module Naloge.Naloga0 (simple) where


-- hell yeah, fibonacci by matrices
data Matrix a = Matrix2x2 { m11 :: a
                          , m12 :: a
                          , m21 :: a
                          , m22 :: a
                          } deriving (Show)

matMul :: Matrix Int -> Matrix Int -> Matrix Int
matMul (Matrix2x2 a11 a12 a21 a22) (Matrix2x2 b11 b12 b21 b22) = Matrix2x2
    (a11*b11 + a12*b21) (a11*b12+a12*b22)
    (a21*b11 + a22*b21) (a21*b12+a22*b22)

matPow :: Matrix Int -> Int -> Matrix Int
matPow m 0 = Matrix2x2 1 0 0 1
matPow m 1 = m
matPow m 2 = matMul m m
matPow m p
    | even p    = matPow (matPow m 2) (p `div` 2)
    | otherwise = matMul m $ matPow (matPow m 2) ((p-1) `div` 2)

simple :: String -> Int
simple = m11 . matPow (Matrix2x2 1 1 1 0) . read
