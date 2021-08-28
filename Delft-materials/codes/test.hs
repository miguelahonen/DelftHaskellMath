double x = x + x
quadruple x = double (double x)

factorial n = product [1..n]

average ns = sum ns `div` length ns

n = a `div` length xs
  where
    a = 10
    xs = [1,2,3,4,5]

lastElem1 ns = ns !! ((length ns) - 1)
lastElem2 ns = head (reverse ns)

hsOrig a = init a
hs1 a = reverse (tail (reverse a))
hs2 a = take ((length a) - 1) a

-- Ctrl+Shift+P -> run stylish-haskell





