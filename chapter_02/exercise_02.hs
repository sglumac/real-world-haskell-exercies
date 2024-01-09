lastButOne :: [a] -> a
lastButOne [] = error "not enough elements"
lastButOne [x] = error "not enough elements"
lastButOne [x, _] = x
lastButOne (x : xs) = lastButOne xs
