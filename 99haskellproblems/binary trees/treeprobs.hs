data Tree a = Empty |
            Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

tree1 = Branch 'a' (Branch 'b' (Branch 'd' Empty Empty)
                               (Branch 'e' Empty Empty))
                   (Branch 'c' Empty
                               (Branch 'f' (Branch 'g' Empty Empty)
                                           Empty))
tree2 = Branch 'a' Empty Empty
tree3 = Empty

cbalTree :: Int
cbalTree a = 
