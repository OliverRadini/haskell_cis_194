toListOfIntegers :: Integer -> [Integer]
toListOfIntegers n = map (\x -> read [x] :: Integer) (show n)

toOnlySingleDigits :: [Integer] -> [Integer]
toOnlySingleDigits = ((foldl (++) []) . (map toListOfIntegers))

sumDigits :: [Integer] -> Integer
sumDigits = ((foldl (+) 0) . toOnlySingleDigits)

doubleEveryOtherForward :: [Integer] -> [Integer]
doubleEveryOtherForward (a : b : xs) = a : (b * 2) : (doubleEveryOtherForward xs)
doubleEveryOtherForward x = x

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = (reverse . doubleEveryOtherForward . reverse)

divisibleBy :: Integer -> Integer -> Bool
divisibleBy x y = (y `mod` x) == 0

validate :: Integer -> Bool
validate = ((\x -> x == 0) . (divisibleBy 10) . sumDigits . doubleEveryOther . toListOfIntegers)

main :: IO ()
main = do
  let result = (validate 4012888888881882)
  print result
