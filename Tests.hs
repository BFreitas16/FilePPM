{-
 - Tests to FilePPM
 -}

module Tests (todosTestes) where

import FilePPM
import Test.QuickCheck

getFirst :: (Int, Int, Int) -> Int
getFirst (a,_,_) = a

getSecond :: (Int, Int, Int) -> Int
getSecond (_,b,_) = b


{- 
 - Verifies that inverted property:
 - An image inverted twice is the same image
 -}
prop_InvertTwice :: FilePPM ->  Bool
prop_InvertTwice image = verticalFlip (verticalFlip image) == image 
                         && horizontalFlip (horizontalFlip image) == image


{-
 - Verifies that every image has the same number of
 - pixels that the multiplication of the file's dimensions
 -}
prop_DimensoesCorretas :: FilePPM -> Bool
prop_DimensoesCorretas image = width * height == (sum $ map length pixeis)
             where width = getFirst $ header image
                   height = getSecond $ header image
                   pixeis = getPixeis image


{-
 - Verifies that take half of one of its dimensions, the
 - ration is the same:
 - Due to the fact of dividing odd numbers by 2, you lose
 - your price. It is therefore considered, for the tests,
 - that both the height and the width have to be pairs
-}
prop_RacioIgual :: FilePPM -> Property
prop_RacioIgual image = even originalWidth && even originalHeight ==> 
                        classify (originalWidth == 1 && originalHeight == 1) "trivial" $ 
                        abs (originalRacio - newRacio) < 0.00001
  where
    originalRacio = (fromIntegral originalWidth :: Float)/(fromIntegral originalHeight :: Float)
    newRacio = (\img -> (fromIntegral (getFirst (header img)) :: Float)
                        /(fromIntegral (getSecond (header img)) :: Float)) newImage
    newImage = halfWidth (halfHeight (image))
    originalWidth = (getFirst (header image))
    originalHeight = (getSecond (header image))


------- EXECUTE THE TESTES -------
todosTestes :: IO ()
todosTestes =  do
    quickCheck prop_InvertTwice
    quickCheck prop_DimensoesCorretas
    quickCheck prop_RacioIgual
