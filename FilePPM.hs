{-
 - Module FilePPM which represent an image in PPM P3 format
 -}

module FilePPM (FilePPM
, make
, Pixel
, header
, getPixeis
, horizontalFlip
, verticalFlip
, halfHeight
, halfWidth
, doMeRed
, doMeGreen
, doMeBlue
, doMeGray
, arbitrary
) where

import Data.List
import System.Environment
import Test.QuickCheck

-- RGB : (r, g, b)
type Pixel = (Int, Int, Int)

-- Data type of an image PPM
data FilePPM = F Int Int Int [[Pixel]] 

-- Returns width, height and maximum image value
header :: FilePPM -> (Int, Int, Int)
header (F w h maxV _) = (w,h,maxV)

-- Returns the pixel array of 1 File PPM
getPixeis :: FilePPM -> [[Pixel]] 
getPixeis (F _ _ _ pixeis) = pixeis

-- Returns the red value of a Pixel
pixelRed :: Pixel -> Pixel
pixelRed (r, _, _) = (r, 0, 0)

-- Returns the green value of a Pixel
pixelGreen :: Pixel -> Pixel
pixelGreen (_, g, _) = (0, g, 0)

-- Returns the blue value of a Pixel
pixelBlue :: Pixel -> Pixel
pixelBlue (_, _, b) = (0, 0, b)

-- Returns the grey value of a Pixel
pixelGray :: Pixel -> Pixel
pixelGray (r, g, b) = (gray, gray, gray)
  where gray = div (r + g + b) 3

{- 
 - Receive a lists of args, PPM P3 image
 - Returns a where FilePPM (data for a PPM image) and 
 - apply the argumens
 -}
make :: [String] -> String -> FilePPM
make funcs xs = makeAux imgOriginal funcs
  where
    imgOriginal = makeFilePPM list
    list = convertToInt $ words removeJunk
    removeJunk = unlines $ filter (\x -> head x /= '#') (tail (lines xs))

-- Aux function for make a FilePPM
makeAux :: FilePPM -> [String] -> FilePPM
makeAux  imgOriginal funcs = (foldr (.) id listaFuncoes) imgOriginal
  where
    listaFuncoes = operacao funcs

{-
 - Receive a list of Strings wich are number
 - Return the list but converted in a list of Ints
 -}

convertToInt :: [String] -> [Int]
convertToInt xs = map (\x -> read x :: Int) xs

-- Receive a list fo ints and return a list of Pixels
convertToPixeis :: [Int] -> [Pixel]
convertToPixeis [] = []
convertToPixeis (a:b:c:xs) = (a,b,c) : convertToPixeis xs

{-
 - Make a FilePPM from a list of Ints
 - Receive a list of Ints and returns a  FilePPM that the first
 - 3 elements are: width, height and maximum image value
 -}
makeFilePPM :: [Int] -> FilePPM
makeFilePPM (w:h:maxV:xs) = F w h maxV $ split w $ convertToPixeis xs

-- Receive a list of args and immediately applies all received functions
operacao :: [String] -> [(FilePPM -> FilePPM)]
operacao [] = []
operacao (f:funcs) 
  | f == "-fh" = horizontalFlip : operacao funcs
  | f == "-fv" = verticalFlip   : operacao funcs
  | f == "-rc" = doMeRed        : operacao funcs
  | f == "-bc" = doMeBlue       : operacao funcs
  | f == "-gc" = doMeGreen      : operacao funcs
  | f == "-gs" = doMeGray       : operacao funcs
  | f == "-hh" = halfHeight     : operacao funcs
  | f == "-hw" = halfWidth      : operacao funcs

-- Receive a FilePPM and horizontally inverts
horizontalFlip :: FilePPM -> FilePPM
horizontalFlip (F w h maxV pixeis) = F w h maxV $ map reverse pixeis

-- Receive a FilePPM and vertically inverts
verticalFlip :: FilePPM -> FilePPM
verticalFlip (F w h maxV pixeis) =  F w h maxV $ reverse pixeis

-- Receive a PPM image and decreases by half the height
halfHeight :: FilePPM -> FilePPM
halfHeight (F w h maxV pixeis) = F w (h `div` 2) maxV $ avgLinhas pixeis

-- Receive a PPM image and decreases by half the width
halfWidth :: FilePPM -> FilePPM
halfWidth (F w h maxV pixeis) = F (w `div` 2) h maxV $ map avgColunas pixeis

{-
 - Receive a Pixel array
 - Return a Pixel array decreased by half
 -}
avgLinhas :: [[Pixel]] -> [[Pixel]]
avgLinhas [] = []
avgLinhas [x] = []
avgLinhas (x:y:xs) = avgPixels x y : avgLinhas xs

{-
 - Receive a list of Pixel
 - Return a list of Pixels with columns decreased by half
 -}
avgColunas :: [Pixel] -> [Pixel]
avgColunas [] = []
avgColunas [x] = []
avgColunas (x:y:xs) = (avgPixel x y):avgColunas xs

{-
 - Receive 2 lists of Pixels
 - Return a list of Pixel with the average color Pixel
 - in each Pixel
 -}
avgPixels :: [Pixel] -> [Pixel] -> [Pixel]
avgPixels [] _ = []
avgPixels (x:xs) (y:ys) = (avgPixel x y):avgPixels xs ys

{-
 - Receive 2 Pixels
 - Return a Pixel with the average color Pixel
 -}
avgPixel :: Pixel -> Pixel -> Pixel
avgPixel (r1,g1,b1) (r2,g2,b2) = (newRed, newGreen, newBlue)
  where 
    newRed   = div (r1 + r2) 2
    newGreen = div (g1 + g2) 2
    newBlue  = div (b1 + b2) 2

{-
 - Receive a PPM image
 - Return a PPM image only with the RED Pixels
 - By other words, a red filter
 -}
doMeRed :: FilePPM -> FilePPM
doMeRed (F w h maxV pixeis)   = F w h maxV $ map (map pixelRed) pixeis

{-
 - Receive a PPM image
 - Return a PPM image only with the GREEN Pixels
 - By other words, a green filter
 -}
doMeGreen :: FilePPM -> FilePPM
doMeGreen (F w h maxV pixeis) = F w h maxV $ map (map pixelGreen) pixeis

{-
 - Receive a PPM image
 - Return a PPM image only with the BLUE Pixels
 - By other words, a blue filter
 -}
doMeBlue :: FilePPM -> FilePPM
doMeBlue (F w h maxV pixeis)  = F w h maxV $ map (map pixelBlue) pixeis

{-
 - Receive a PPM image
 - Return a PPM image only with the GREY Pixels
 - By other words, a black and white filter
 -}
doMeGray :: FilePPM -> FilePPM
doMeGray (F w h maxV pixeis)  = F w h maxV $ map (map pixelGray) pixeis

{-
 - Receive a list 
 - Return a list of lists grouped by n
 -}
split :: Int -> [a] -> [[a]]
split _ [] = []
split n xs = [elem] ++ (split n param)
  where
    elem = take n xs
    param = drop n xs

------- INSTANCES -------

instance Show FilePPM where
  show = showFilePPM

{-
 - FilePPM to String with their RGB's values separeted
 - with spaces
 -}
showFilePPM :: FilePPM -> String                                        
showFilePPM (F w h maxV pixeis) = "P3\n" ++ show w ++ " " ++ show h ++ " " ++ show maxV ++ "\n" ++ showAllPixeis
  where showAllPixeis = concat . concat . map (map showPixel) $ pixeis

{-
 - Pixel to String with their RGB's values separeted
 - with spaces
 -}
showPixel :: Pixel -> String
showPixel (a,b,c) = show a ++ " " ++ show b ++ " " ++ show c ++ "\n"

-------
instance Eq FilePPM where
  (==) = eqImages

{-
 - Verifies if 2 PPM images are equal
 -}
eqImages :: FilePPM -> FilePPM -> Bool
eqImages file1@(F _ _ _ pixeis1) file2@(F _ _ _ pixeis2) = header file1 == header file2 && pixeis1 == pixeis2

-------
instance Arbitrary FilePPM where
  arbitrary = do
    let maxWidthHeight = 500
    maxVal <- choose (1, 255) :: Gen Int
    width  <- choose (1, maxWidthHeight) :: Gen Int
    height <- choose (1, maxWidthHeight) :: Gen Int
    pss <- arbiPixelM width height maxVal
    return $ F width height maxVal pss

{-
 - Create a arbitrary Pixel array 
 -}
arbiPixelM :: Int -> Int -> Int -> Gen [[Pixel]]
arbiPixelM width height maxVal = mapM (mapM arbiPixel) matriz_default
  where matriz_default = replicate height $ replicate width maxVal

{-
 - Creates a arbitrary Pixel
 - Receive : maximum image value
 -}
arbiPixel :: Int -> Gen Pixel
arbiPixel max = do
  r <- choose (0, max) :: Gen Int
  g <- choose (0, max) :: Gen Int
  b <- choose (0, max) :: Gen Int
  return (r, g , b)
