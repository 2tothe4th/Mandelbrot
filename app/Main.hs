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


specifiedResolution :: Integer
specifiedResolution = 1_000_000

data CustomE

instance HasResolution CustomE where
    resolution _ = specifiedResolution


qfix :: (a -> a) -> a -> Int -> (a -> Bool) -> Int
qfix _ _ 0 _ = 0
qfix f !x !i p
    | not (p x) = i 
    | otherwise = qfix f (f x) (i - 1) p

newtype NewFixed r = NewFixed (Fixed r) deriving (Eq, Ord, Num, Real, RealFrac, Fractional)

instance HasResolution r => Floating (NewFixed r)
instance HasResolution r => RealFloat (NewFixed r) where
    decodeFloat x = (0, 0)

{-
instance HasResolution r => Ord (Complex (NewFixed r))
instance HasResolution r => Real (Complex (NewFixed r))
instance HasResolution r => RealFrac (Complex (NewFixed r))
instance HasResolution r => RealFloat (Complex (NewFixed r))-}
    

magnitudeSquared :: Num a => Complex a -> a
magnitudeSquared (a :+ b) = a * a + b * b

m :: (Num a, RealFloat a, Ord a) => Complex a -> Int -> Int
m x i  = qfix (\y -> y * y + x) 0 i ((<= 4) . magnitudeSquared)
 
 {-
--render :: Complex
render :: (Monad m, RealFloat a) => [Complex a] -> (Complex a -> Int -> m ()) -> m ()
render positions monad = for_ positions $ (\x -> monad x (m x))

--w position iteration = putStr (if iteration == 0 then "(" ++ show (realPart position) ++ ", " ++ show (imagPart position) ++ ")," else "") 

customMakeImage :: (Elevator e, MArray a cs e, PrimMonad m) => 
    (Complex f -> (Int, Int))
    -> (Int -> Pixel cs e)
    -> MImage (PrimState m) a cs e
    -> Complex f
    -> Int
    -> m ()
customMakeImage transform color array position iteration = 
    write array (transform position) (color iteration)-}

{-
bottomLeft, topRight :: RealFloat a => Complex a
bottomLeft = -(8 :+ 4.5)
topRight = 8 :+ 4.5

screenWidth, screenHeight :: Int
screenWidth = 1280
screenHeight = 720-}

lerp :: Num a => a -> a -> a -> a
lerp a b t = (1 - t) * a + b * t

elemMul :: RealFloat a => Complex a -> Complex a -> Complex a
elemMul (a :+ b) (c :+ d) = a * c :+ b * d

elemDiv :: RealFloat a => Complex a -> Complex a -> Complex a
elemDiv (a :+ b) (c :+ d) = a / c :+ b / d


{-
getPixelPosition :: RealFloat a => Complex a -> (Int, Int)
getPixelPosition z = let a :+ b = s `elemMul` ((z - bottomLeft) `elemDiv` (topRight - bottomLeft)) in (round a, round b) where
    s = fromIntegral (screenWidth - 1) :+ fromIntegral (screenHeight - 1)-}

getWorldPosition :: (RealFloat a) => (Int, Int) -> Complex a -> Complex a -> Int -> Int -> Complex a
getWorldPosition (b, a) bottomLeft topRight screenWidth screenHeight = bottomLeft + (topRight - bottomLeft) `elemMul` 
    ((fromIntegral a :+ (fromIntegral screenHeight - 1 - fromIntegral b)) `elemDiv` ((fromIntegral screenWidth - 1) :+ (fromIntegral screenHeight - 1))) 

color :: Int -> Pixel RGB Double
color i = PixelRGB t t t 
    where 
        t = lerp 0 1 ((-exp (-fromIntegral i / 50) + 1) :: Double)

zoom :: RealFloat a => Complex a -> Complex a -> [Complex a] -> [Complex a]
zoom value pivot = map (\x -> (x - pivot) / value + pivot)

pan :: RealFloat a => Complex a -> [Complex a] -> [Complex a]
pan value = map (+ value)

main :: IO ()
main = do
    let screenWidth = 1920
    let screenHeight = 1080

    let center = -1.74
    let zoomValue = 100 :: Complex (NewFixed CustomE)

    let iterations = 100

    let [bottomLeft, topRight] = zoom zoomValue center (pan center [-(8 :+ 4.5), 8 :+ 4.5])

    let image = makeImageR VU (screenHeight, screenWidth) (\x -> color (m (getWorldPosition x bottomLeft topRight screenWidth screenHeight) iterations))
    writeImage "output.png" image
    putStrLn "Hello"
