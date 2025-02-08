module CFixed where

import Control.Exception
import Data.Data
import Numeric.GMP.Types
import Numeric.GMP.Raw.Safe
import Foreign hiding (void)
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe
import Text.Printf
import Data.Ratio (denominator)
import GHC.Real (numerator)
import Debug.Trace
import Data.Kind

newtype CFixed p = CF { value :: MPF }
  deriving (Storable) via MPF

class Precision p where
  getBitCount :: p -> MPBitCnt

newtype FException = FException { unfexception :: CInt }
  deriving (Storable, Show) via CInt
  deriving stock (Typeable)

instance Exception FException

ok :: CInt -> IO ()
ok 0 = pure ()
ok i = throwIO (FException i)

instance Precision p => Ord (CFixed p) where
  compare (CF a) (CF b) = unsafeDupablePerformIO do
    alloca \pA -> do
      poke pA a
      alloca \pB -> do
        poke pB b
        mpf_cmp pA pB >>= \case
          0 -> pure EQ
          -1 -> pure LT
          _ -> pure GT

instance Precision p => Eq (CFixed p) where
  a == b = compare a b == EQ

fromBase10 :: Precision p =>  String -> MPBitCnt -> CFixed p
fromBase10 s b = unsafeDupablePerformIO do
  alloca \pa -> do
    mpf_init2 pa b
    withCString s \cs -> do
      -- base 10
        mpf_set_str pa cs 10 >>= ok
        CF <$> peek pa

performMPF1 :: (Ptr MPF -> Ptr MPF -> IO ()) -> MPF -> MPF
performMPF1 f a = unsafeDupablePerformIO do
  alloca \pA -> do
    poke pA a
    alloca \pB -> do
      mpf_get_prec pA >>= mpf_init2 pB
      f pB pA
      peek pB


performMPF2 :: (Ptr MPF -> Ptr MPF -> Ptr MPF -> IO ()) -> MPF -> MPF -> MPF
performMPF2 function a b  = unsafeDupablePerformIO do
  alloca \pA -> do
      poke pA a
      alloca \pB -> do
          poke pB b
          alloca \pOut -> do
              (max <$> mpf_get_prec pA <*> mpf_get_prec pB) >>= mpf_init2 pOut
              function pOut pA pB
              peek pOut

getPrecision :: MPF -> MPBitCnt
getPrecision a = unsafeDupablePerformIO do
  alloca \pA -> do
    poke pA a
    mpf_get_prec pA

ci :: Precision p => MPBitCnt -> Integer -> CFixed p
ci p x = fromBase10 (show x) p

cd :: Precision p => MPBitCnt -> Double -> CFixed p
cd p x = fromBase10 (show x) p

instance Precision p => Num (CFixed p) where
    (CF a) + (CF b) = CF $ performMPF2 mpf_add a b
    (CF a) * (CF b) = CF $ performMPF2 mpf_mul a b
    (CF a) - (CF b) = CF $ performMPF2 mpf_sub a b
    abs (CF a) = CF $ performMPF1 mpf_abs a
    signum a
      | a > 0 = 1
      | a == 0 = 0
      | otherwise = -1
    fromInteger x = fromBase10 (show x) (getBitCount (undefined :: p))

insertInto :: Int -> a -> [a] -> [a]
insertInto 0 x xs = x : xs
insertInto i x (y : ys) = y : insertInto (i - 1) x ys
insertInto _ _ _ = undefined

instance Precision p => Show (CFixed p) where
  show :: Precision p => CFixed p -> String
  show (CF a) = unsafeDupablePerformIO do
    alloca \pA -> do
      poke pA a
      alloca \pExponent -> do
        uninterruptibleMask_ do
          pMantissa <- mpf_get_str nullPtr pExponent 10 0 pA
          mantissa <- peekCString pMantissa <* free pMantissa
          exponent <- (+(-1)) <$> peek pExponent

          --https://machinecognitis.github.io/Math.Gmp.Native/html/9e7b9239-a7a8-4667-f6c7-bfc142d3f429.htm
          print exponent
          if exponent >= 0 then
            pure mantissa
          else do
            let longerMantissa = replicate (-fromIntegral exponent) '0' ++ mantissa
            pure $ insertInto 1 '.' longerMantissa




instance Precision p => Fractional (CFixed p) where
  CF a / CF b = CF $ performMPF2 mpf_div a b
  fromRational a = fromIntegral (numerator a) / fromIntegral (denominator a)
instance Precision p => Real (CFixed p)
instance Precision p => RealFrac (CFixed p) where
  properFraction _ = (0, 1)
instance Precision p => Floating (CFixed p) where
  exp _ = 0
instance Precision p => RealFloat (CFixed p) where
  decodeFloat _ = (0, 0)