import Prelude
import Data.List


data Tree a = Node (Tree Int) Int (Tree Int) | Leaf
    deriving (Show)



{-build_tree::Tree a -> Int -> IO (Tree a)
build_tree (Node Leaf n Leaf) 0| n == -1 = return (Node Leaf n Leaf)
                           |otherwise = do
                            line <- getLine
                            let [n1, n2] = words line
                            return (Node (Node Leaf (read n1::Int) Leaf) n (Node Leaf (read n2::Int) Leaf))
build_tree (Node Leaf (-1) Leaf) _ = return (Node Leaf (-1) Leaf)
build_tree (Node t1 n t2) level = do
    left_tree <- build_tree t1 (level - 1)
    right_tree <- build_tree t2 (level - 1)
    return (Node left_tree n right_tree)


iterate_to_build_tree::Tree a -> Int -> Int -> IO (Tree a)
iterate_to_build_tree t current_level last_level|current_level == last_level = return t
iterate_to_build_tree t current_level last_level = do
    new_tree <- build_tree t current_level
    iterate_to_build_tree new_tree (current_level + 1) last_level
-}
--the level is the level of the nodes, the root is the level 1, and its children are level 2...
--it returns the index that we have arrived to in the list of nodes (arr)
build_tree_with_list::Tree a -> Int -> [Int] -> Int -> ((Tree a), Int)
build_tree_with_list (Node Leaf n Leaf) 0 arr index |n == -1 = ((Node Leaf n Leaf), 0)
                                                    |otherwise = ((Node (Node Leaf n1 Leaf) n (Node Leaf n2 Leaf)), 2)
                                                    where
                                                        n1 = arr !! index
                                                        n2 = arr !! (index + 1)
build_tree_with_list (Node Leaf (-1) Leaf) _ _ _ = ((Node Leaf (-1) Leaf), 0)
build_tree_with_list (Node t1 n t2) level arr index = ((Node left_tree n right_tree), i1 + i2)
    where
        (left_tree, i1) = build_tree_with_list t1 (level - 1) arr index
        (right_tree, i2) = build_tree_with_list t2 (level - 1) arr (index + i1)



--it iterates to build the tree by increasing the reached level by one after each iteration
iterate_to_build_tree_with_list::Int -> [Int] -> Int -> Int -> Tree a -> Tree a
iterate_to_build_tree_with_list level arr index last t|last <= index = t
                                                      |otherwise = iterate_to_build_tree_with_list (level + 1) arr (index + i) last new_t
    where
        (new_t, i) = build_tree_with_list t level arr index



--read the nodes from the input, n is the number of lines it will read, the nodes will be given in pairs
--first 5
--2 3
-- -1 4
-- -1 5
-- -1 -1
-- -1 -1
read_nodes:: IO [Int]
read_nodes = do
    n <- getLine
    arr <- sequence [getLine  | _ <- [1..(read n::Int)]]
    let
        nodes = map (\x -> read x ::Int) $ concat $ map words arr
    return nodes



--gets the nodes and build the tree
use_nodes_to_build_tree::IO(Tree a)
use_nodes_to_build_tree = do
    nodes <- read_nodes
    let
        tree = iterate_to_build_tree_with_list 0 nodes 0 (length nodes) (Node Leaf 1 Leaf)
    return tree


--swap the nodes at the given swappinlevel, that is if the swapping level is 2, then the children of the node at level 2,4,6... will be
--swaped. 
swap_nodes::Int -> Int -> Tree a -> Tree a
swap_nodes _ _ Leaf = Leaf
swap_nodes _ _ (Node Leaf n Leaf) = Node Leaf n Leaf
swap_nodes level swapping_level (Node t1 n t2)
    |level `mod` swapping_level == 0 = Node (swap_nodes (level + 1) swapping_level t2) n (swap_nodes (level + 1) swapping_level t1)
    |otherwise = Node (swap_nodes (level + 1) swapping_level t1) n (swap_nodes (level + 1) swapping_level t2)





inorder_traversal::Tree Int -> String
inorder_traversal Leaf = ""
inorder_traversal (Node Leaf n Leaf)|n == -1 = ""
                                    |otherwise = (show n)
inorder_traversal (Node t1 n t2)|n == -1= ""
                                |otherwise = (inorder_traversal t1) ++ ((show n) ++ " ") ++ (inorder_traversal t2)


--after a swap the tree is traversed to obtain the nodes, all the swaps will be put in the list 
inorder_traversals::Tree Int -> [Int] -> [String]
inorder_traversals _ [] = []
inorder_traversals t (x:xs) = str_tree:(inorder_traversals new_tree xs)
    where
        new_tree = swap_nodes 1 x t
        str_tree = inorder_traversal new_tree



swap_nodes_from_input::IO()
swap_nodes_from_input = do
    tree <- use_nodes_to_build_tree
    number_of_swaps <- getLine
    swapping_levels_str <- sequence [getLine | _ <- [1..(read number_of_swaps ::Int)]]
    let
        swapping_levels = map (\x -> read x :: Int) swapping_levels_str
        str_trees = inorder_traversals tree swapping_levels
    mapM putStrLn str_trees
    return()