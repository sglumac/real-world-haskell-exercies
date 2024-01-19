{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use concat" #-}
myConcat :: [[a]] -> [a]
myConcat = foldr (++) []
