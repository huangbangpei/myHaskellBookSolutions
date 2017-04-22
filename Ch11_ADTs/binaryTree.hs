-- P430

-- Write map for BinaryTree

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Show, Ord)

mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = 
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' = Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

mapExpected = Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

mapOkay = 
    if mapTree (+1) testTree' == mapExpected
    then print "Okay!"
    else error "failed!"

-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] ++ preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = inorder left ++ [a] ++ inorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = postorder left ++ postorder right ++ [a]

testTree :: BinaryTree Integer
testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
    if preorder testTree == [2, 1, 3]
    then putStrLn "Preorder fine!"
    else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
    if inorder testTree == [1, 2, 3]
    then putStrLn "Inorder fine!"
    else putStrLn "Bad news bears."
testPostorder :: IO ()
testPostorder =
    if postorder testTree == [1, 3, 2]
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"

main :: IO ()
main = do
    testPreorder
    testInorder
    testPostorder


-- Write foldr for BinaryTree

-- inorder
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ acc Leaf = acc
foldTree f acc (Node left a right) = foldTree f (f a (foldTree f acc left)) right

-- preorder
foldTree' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree' _ acc Leaf = acc
foldTree' f acc (Node left a right) = foldTree' f (foldTree' f (f a acc) left) right

-- postorder
foldTree'' :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree'' _ acc Leaf = acc
foldTree'' f acc (Node left a right) = f a (foldTree f (foldTree f acc left) right)

-- https://github.com/dmvianna/haskellbook/blob/master/src/Ch11-BinaryTree.hs

-- also https://www.reddit.com/r/HaskellBook/comments/4czzpp/haskellbookch_11_problems_implementing_maptree_in/
foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
foldTree f b node =
  case node of
       Node Leaf a Leaf -> f a b b
       Node Leaf a right -> f a b (foldTree f b right)
       Node left a Leaf -> f a b (foldTree f b left)
       Node left a right -> f a (foldTree f b right) (foldTree f b left)

mapTree' :: (a -> b) -> BinaryTree a -> BinaryTree b
mapTree' f = foldTree (\a b c -> Node b (f a) c) Leaf