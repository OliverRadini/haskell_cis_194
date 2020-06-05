filterWithIndex :: ((Int, a) -> Bool) -> [a] -> [a]
filterWithIndex f x = map (\(_, x) -> x) (filter f (zip [a+1|a<-[0..]] x))

skips :: [a] -> [[a]]
skips x = [(filterWithIndex (\(i, _) -> (i `mod` (y + 1) == 0)) x) | y <- [0..(length x)]]

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : xs) = (if a < b && b > c then [b] else []) ++ (localMaxima ([c] ++ xs))
localMaxima x                = []

count :: (Eq a) => a -> [a] -> Int
count x xs = length (filter (\a -> a == x) xs)

rotate :: [[a]] -> [[Maybe a]]
rotate [] = []
rotate xs = [(map (getItemAtIndex i) xs) |i<-[0..(getMaxLength xs)]]

histogram :: [Integer] -> [[Char]]
histogram x = map (map (takeOr ' ')) (rotate [(show y) ++ "=" ++ (take (count y x) (repeat '*')) | y<-[0..9]])

getMaxLength :: [[a]] -> Int
getMaxLength = maximum . (map length)

getItemAtIndex :: Int -> [a] -> Maybe a
getItemAtIndex _ []     = Nothing
getItemAtIndex 0 (x:_)  = Just x
getItemAtIndex i (_:xs) = getItemAtIndex (i - 1) xs

takeOr :: a  -> Maybe a -> a
takeOr x Nothing  = x
takeOr _ (Just x) = x

join :: Char -> [String] -> String
join c []     = []
join _ [x]    = x
join c (x:xs) = x ++ [c] ++ (join c xs)


main = do
  print (join ' ' (histogram [1, 1, 2, 1, 3, 4, 5]))