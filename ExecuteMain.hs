{-
 - Main function, to execute the program
-}

import Tests
import FilePPM
import System.Environment

main :: IO()
main = do
  args <- getArgs
  -- verifies if the 1st arg is "-t" to execute the tests
  if args !! 0 == "-t"    
    then todosTestes
    else do
      contents <- readFile (head args)
      let fileOut = args !! 1
      let imagem = make (drop 2 args) contents
      writeFile fileOut (show imagem)
