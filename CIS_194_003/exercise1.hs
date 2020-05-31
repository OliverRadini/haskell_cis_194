filterWithIndex :: ((Int, a) -> Bool) -> [a] -> [a]
filterWithIndex f x = map (\(_, x) -> x) (filter f (zip [a+1|a<-[0..]] x))

skips :: [a] -> [[a]]
skips x = [(filterWithIndex (\(i, _) -> (i `mod` (y + 1) == 0)) x) | y <- [0..(length x)]]

main = do
  print (skips [True, False])