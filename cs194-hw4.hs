--file cs194-hw4.hs
fun1 = foldl (\acc x -> (x - 2)*acc) 1 . filter even 

--think about fun2 carefully 
fun2 = foldl (\acc x -> if even x then acc + x else acc) 0 . takeWhile (>1) . iterate (\x -> if even x then x `div` 2 else 3*x+1)

fun21 1 = 0
fun21 n | even n = n + fun21 (n `div` 2)
        | otherwise = fun21 (3 * n + 1)

data Tree a = Leaf 
            | Node Integer (Tree a) a (Tree a)
 deriving (Show,Eq)

foldTree :: [a] -> Tree a
foldTree = foldl ff Leaf

flip' f = g  
    where g x y = f y x  

heightOfTree Leaf Leaf = 0
heightOfTree (Node i _ _ _) Leaf = i
heightOfTree Leaf (Node i _ _ _)= i
heightOfTree n1@(Node i1 _ _ _) n2@(Node i2 _ _ _) = i2 + 1

ff Leaf x = Node 0 Leaf x Leaf
ff n@(Node i Leaf v Leaf) x = Node (i + 1) n x Leaf
ff n@(Node i left v Leaf) x = Node i left x (ff Leaf v)
ff n@(Node i left@(Node i1 _ _ _) v right@(Node i2 _ _ _)) x | (i1 == i2) = Node (heightOfTree left (ff right v)) left x (ff right v)
ff n@(Node i left@(Node i1 _ _ _) v right@(Node i2 _ _ _)) x | (i1 < i2) = Node i (ff left v) x right
ff n@(Node i left@(Node i1 _ _ _) v right@(Node i2 _ _ _)) x | (i1 > i2) = Node i left x (ff right v)

xor :: [Bool] -> Bool
xor = odd . foldl (\acc x -> if x then acc+1 else acc) 0   

xor1 :: [Bool] -> Bool
xor1 = foldl (\acc x -> if x then not acc else acc) False

map1 :: (a -> b) -> [a] -> [b]
map1 f = foldr (\z acc -> (f z) : acc) []

--Write foldl in terms of foldr
--myfoldl f base xs = foldr (\z acc -> (flip f) acc ) base xs

sieveOfSundaram n = map (\x -> 2*x + 1) (filter (`notElem` filtSieve n) [1..n])

filtSieve n = [(i + j + 2*i*j)| j <- [1..(n `div` 2)], i <- [1..j],i + j + 2*i*j <= n]

