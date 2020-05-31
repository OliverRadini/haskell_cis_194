filterWithIndex :: ((Int, a) -> Bool) -> [a] -> [a]
filterWithIndex f x = map (\(_, x) -> x) (filter f (zip [a+1|a<-[0..]] x))

skips :: [a] -> [[a]]
skips x = [(filterWithIndex (\(i, _) -> (i `mod` (y + 1) == 0)) x) | y <- [0..(length x)]]

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs) = (if a < b && b > c then [b] else []) ++ (localMaxima ([c] ++ xs))
localMaxima x                = []

main = do
  print (skips [True, False])