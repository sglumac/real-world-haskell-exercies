import System.Environment (getArgs)

interactWith function inputFile outputFile = do
  input <- readFile inputFile
  writeFile outputFile (function input)

safeHead :: [[Char]] -> [Char]
safeHead [] = ""
safeHead cs = head cs

myTranspose :: [[a]] -> [[a]]
myTranspose ([] : _) = []
myTranspose x = map head x : myTranspose (map tail x)

transposeText :: [Char] -> [Char]
transposeText text = unlines (myTranspose (lines text))

main = mainWith transposeText
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"
