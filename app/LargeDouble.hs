module LargeDouble where
import Data.Int
import GHC.Num
import Data.Bits
import GHC.Float
{-
data LargeDouble = LD { exponent :: Integer, mantissa :: Double }

changeExponent :: LargeDouble -> Integer -> LargeDouble
changeExponent (LD e m) target = LD target (m * 2 ** fromIntegral (e - target))

getExponentOfDouble :: Double -> Integer
getExponentOfDouble 0 = 0 
getExponentOfDouble x = ((+ (-1023)) . (.>>. 52) . (0x7FF0000000000000 .&.) . fromIntegral . castDoubleToWord64) x

--https://hackage.haskell.org/package/numeric-logarithms-0.1.0.0/docs/Numeric-Logarithms.html#g:1
--https://stackoverflow.com/questions/26416323/function-to-calculate-log-of-integer
--https://mail.haskell.org/pipermail/haskell-cafe/2008-February/039493.html
normalizeExponent :: LargeDouble -> LargeDouble
normalizeExponent (LD e m) = changeExponent (LD e m) (e + absLogBase2) where
    absLogBase2 = getExponentOfDouble m


addSameExponent :: LargeDouble -> LargeDouble -> LargeDouble
addSameExponent x (LD _ 0) = x
addSameExponent (LD _ 0) x  = x
addSameExponent (LD a b) (LD c _) = normalizeExponent (LD (a + c) b)

instance Num LargeDouble where
    a + b = addSameExponent (changeExponent a maxExponent) (changeExponent b maxExponent) where
        maxExponent = max (LargeDouble.exponent a) (LargeDouble.exponent b)
    fromInteger x = LD exponent (fromIntegral x / (2 ** fromIntegral exponent)) where
        exponent = getExponentOfDouble (fromIntegral x :: Double)

-}