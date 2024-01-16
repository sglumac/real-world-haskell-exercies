import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

safeHead :: [[Char]] -> [Char]
safeHead [] = ""
safeHead cs = head cs

main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"

    myFunction text = unlines (map (safeHead . words) (lines text))
