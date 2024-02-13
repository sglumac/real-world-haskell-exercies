import System.Posix.Files (fileExist)

doesNameExist :: FilePath -> IO Bool
doesNameExist = fileExist
