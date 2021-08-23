import Data.List (intercalate)
import System.Environment (getArgs)

-- Ex 3
firstWord :: String -> String
firstWord s =
  intercalate
    "\n"
    (map head (filter (not . null) (map words (filter (not . null) (lines s)))))

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"
    myFunction = firstWord
