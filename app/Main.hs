module Main where
import Data.Fixed
import Data.Complex
import Data.Traversable
import Data.Foldable
--import Graphics.Image
--import Graphics.Color.Algebra
import Graphics.Image.Interface hiding (map)
import Control.Monad.Primitive
import Graphics.Image.ColorSpace hiding (magnitude, map)
import Graphics.Image hiding (magnitude, map)
import Data.Fixed
import Numeric
import GHC.Real
import Numeric.GMP.Types (MPBitCnt(MPBitCnt))
import CFixed
import Debug.Trace

qfix :: (a -> a) -> a -> Int -> (a -> Bool) -> Int
qfix _ _ 0 _ = 0
qfix f !x !i p
    | not (p x) = i 
    | otherwise = qfix f (f x) (i - 1) p

magnitudeSquared :: Num a => Complex a -> a
magnitudeSquared (a :+ b) = a * a + b * b

m :: (Num a, RealFloat a, Ord a) => Complex a -> Int -> Int
m x i  = qfix (\y -> y * y + x) 0 i ((<= 4) . magnitudeSquared)
 
lerp :: Num a => a -> a -> a -> a
lerp a b t = (1 - t) * a + b * t

elemMul :: RealFloat a => Complex a -> Complex a -> Complex a
elemMul (a :+ b) (c :+ d) = a * c :+ b * d

elemDiv :: RealFloat a => Complex a -> Complex a -> Complex a
elemDiv (a :+ b) (c :+ d) = a / c :+ b / d

getWorldPosition :: (RealFloat a) => (Int, Int) -> Complex a -> Complex a -> Int -> Int -> Complex a
getWorldPosition (b, a) bottomLeft topRight screenWidth screenHeight = bottomLeft + (topRight - bottomLeft) `elemMul` 
    ((fromIntegral a :+ (fromIntegral screenHeight - 1 - fromIntegral b)) `elemDiv` ((fromIntegral screenWidth - 1) :+ (fromIntegral screenHeight - 1))) 

clamp :: Ord a => a -> a -> a -> a 
clamp low high = min high . max low

color :: Int -> Pixel RGB Double
color i = if i == 0 then PixelRGB 0 0 0 else toPixelRGB $ PixelHSI t 0.5 0.5
    where 
        t = ((fromIntegral i :: Double) / 100) `mod'` 1

zoom :: RealFloat a => a -> Complex a -> [Complex a] -> [Complex a]
zoom value pivot = map (\x -> (x - pivot) / (value :+ 0) + pivot)

pan :: RealFloat a => Complex a -> [Complex a] -> [Complex a]
pan value = map (+ value)



data SpecifiedPrecision
instance Precision SpecifiedPrecision where
    getBitCount = 128

main :: IO ()
main = do

    let screenWidth = 1920
    let screenHeight = 1080

    let center = -1.74999 :: Complex (CFixed SpecifiedPrecision)
    let startZoom = 1

    let iterations = 5000

    let [bottomLeft, topRight] = [-(8 :+ 4.5), 8 :+ 4.5]

    let targetZoom = 1000
    let secondsPer10x = 1 
    let frameRate = 60

    forM_ [0..(ceiling $ frameRate * secondsPer10x * 2)] (\i -> do
        let [newBottomLeft, newTopRight] = zoom (startZoom * (10 ** (fromIntegral i / frameRate / secondsPer10x))) center (pan center [bottomLeft, topRight])
        let image = makeImageR VU (screenHeight, screenWidth) (\x -> color (m (getWorldPosition x newBottomLeft newTopRight screenWidth screenHeight) iterations))
        writeImage ("Output/frame" ++ show i ++ ".png") image
        putStrLn $ "Frame " ++ show i ++ " finished" 
        )
    putStrLn "Hello"
