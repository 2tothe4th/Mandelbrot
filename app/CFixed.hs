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

newtype CFixed = CF { unf :: MPF }
  deriving (Storable) via (MPF)

newtype FException = FException { unfexception :: CInt }
  deriving (Storable, Show) via (CInt)
  deriving stock (Typeable)

instance Exception FException

ok :: CInt -> IO ()
ok 0 = pure ()
ok i = throwIO (FException i)

instance Ord CFixed where
  compare (CF a) (CF b) = unsafeDupablePerformIO do
    alloca \pa -> do
      poke pa a
      alloca \pb -> do
        poke pb b
        mpf_cmp pa pb >>= \case
          -1 -> pure LT
          0 -> pure EQ
          _ -> pure GT

instance Eq CFixed where
  a == b = compare a b == EQ

fromBase10 :: String -> MPBitCnt -> CFixed
fromBase10 s b = unsafeDupablePerformIO do
  alloca \pa -> do
    mpf_init2 pa b
    withCString s \cs -> do
      -- base 10
        mpf_set_str pa cs 10 >>= ok
        CF <$> peek pa


preformMPF :: (Ptr MPF -> SrcPtr MPF -> SrcPtr MPF -> IO ()) -> MPF -> MPF -> MPF
preformMPF function a b  = unsafeDupablePerformIO do
  alloca \pa -> do
      poke pa a
      alloca \pb -> do
          poke pb b
          alloca \psum -> do
              function psum pa pb
              peek psum

instance Num CFixed where
    (CF a) + (CF b) = CF $ preformMPF mpf_add a b
    (CF a) * (CF b) = CF $ preformMPF mpf_mul a b
    (CF a) - (CF b) = CF $ preformMPF mpf_sub a b             
    fromInteger x = fromBase10 (show x) 128