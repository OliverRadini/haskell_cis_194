type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 1 from to proxy = [(from, to)]
hanoi 2 from to proxy = [(from, proxy), (from, to), (proxy, to)]
hanoi n from to proxy = (hanoi (n - 1) from proxy to) ++ [(from, to)] ++ (hanoi (n - 1) proxy to from)

main = do
  let result = hanoi 4 "Peg 1" "Peg 2" "Peg 3"
  print result