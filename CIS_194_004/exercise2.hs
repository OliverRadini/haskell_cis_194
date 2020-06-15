data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

j- Change this to use iterate
asIterable :: Tree a -> [(a, Integer)]
asIterable Leaf = []
asIterable (Node h l x r) = [(x, h)] ++ (asIterable l) ++ (asIterable r)

insert :: Tree a -> a -> Tree a
insert Leaf                     x = (Node 1 Leaf x Leaf)
insert (Node h (Leaf) v (Leaf)) x = (Node h (Node (h + 1) Leaf x Leaf) v Leaf)
insert (Node h (Leaf) v r)      x = (Node h (Node (h + 1) Leaf x Leaf) v r)
insert (Node h l v (Leaf))      x = (Node h l v (Node (h + 1) Leaf x Leaf))
insert (Node h l v r)           x
  | ((getHeight l) > (getHeight r)) = (Node h (insert l x) v r)
  | ((getHeight l) < (getHeight r)) = (Node h l v (insert r x))
  | otherwise                       = (Node h (insert l x) v r)

getHeight :: Tree a -> Integer
getHeight Leaf           = 0
getHeight node = maximum (map (\(_, h) -> h) (asIterable node))

--foldTree :: [a] -> Tree a

main = do
  let test = (insert (insert (insert (insert Leaf 5) 7) 3) 12)

  print test