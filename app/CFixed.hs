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

instance Ord (CFixed p) where
  compare (CF a) (CF b) = unsafeDupablePerformIO do
    alloca \pA -> do
      poke pA a
      alloca \pB -> do
        poke pB b
        mpf_cmp pA pB >>= \case
          0 -> pure EQ
          -1 -> pure LT
          _ -> pure GT

instance Eq (CFixed p) where
  a == b = compare a b == EQ

fromBase10 :: String -> MPBitCnt -> CFixed p
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

instance Precision p => Num (CFixed p) where
    (CF a) + (CF b) = CF $ performMPF2 mpf_add a b
    (CF a) * (CF b) = CF $ performMPF2 mpf_mul a b
    (CF a) - (CF b) = CF $ performMPF2 mpf_sub a b
    abs x = if x >= 0 then x else -x
    signum a
      | a > 0 = 1
      | a == 0 = 0
      | otherwise = -1
    fromInteger x = fromBase10 (show x) (getBitCount (undefined :: p))

insertInto :: Int -> a -> [a] -> [a]
insertInto 0 x xs = x : xs
insertInto _ _ [] = error "Out of bounds"
insertInto i x (y : ys) = y : insertInto (i - 1) x ys

instance Show (CFixed p) where
  show (CF a) = unsafeDupablePerformIO do
    alloca \pA -> do
      poke pA a
      alloca \pExponent -> do
        uninterruptibleMask_ do
          pMantissa <- mpf_get_str nullPtr pExponent 10 0 pA
          mantissa <- peekCString pMantissa <* free pMantissa
          exponent <- peek pExponent
          let sign = if head mantissa == '-' then "-" else ""
          let absMantissa = filter (/= '-') mantissa

          --https://machinecognitis.github.io/Math.Gmp.Native/html/9e7b9239-a7a8-4667-f6c7-bfc142d3f429.htm
          if fromIntegral exponent == length absMantissa then
            pure $ sign ++ absMantissa
          else if exponent >= 1 then
            pure $ sign ++ insertInto (fromIntegral exponent) '.' absMantissa
          else do
            let longerAbsMantissa = replicate (-fromIntegral exponent + 1) '0' ++ absMantissa
            pure $ sign ++ insertInto 1 '.' longerAbsMantissa




instance Precision p => Fractional (CFixed p) where
  CF a / CF b = CF $ performMPF2 mpf_div a b where !_ = traceId ("Denominator: " ++ show (CF b))
  fromRational a = fromIntegral (numerator a) / fromIntegral (denominator a)
instance Precision p => Real (CFixed p)
instance Precision p => RealFrac (CFixed p) where
  properFraction _ = (0, 1)
instance Precision p => Floating (CFixed p) where
  exp _ = 0
instance Precision p => RealFloat (CFixed p) where
  decodeFloat _ = (0, 0)