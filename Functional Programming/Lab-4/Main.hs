{-
- Name: Mateusz Waloszek.
- Number: 120412764.
- Assignment: 4.
-}



-- Define a binary search tree data
-- type that is polymorphic over the type `a`.
data BST a = Empty                -- An empty tree
           | Node {               -- A tree node that contains:
               value :: a,        -- A value of type `a`
               left  :: BST a,    -- A left subtree
               right :: BST a     -- A right subtree
           } deriving (Show, Eq)  -- Derive Show and Eq for easy 
                                  -- printing and equality checks


-- A fold function for the BST
tree_fold :: (a -> b -> b) -> b -> BST a -> b


-- If the tree is empty,  return the accumulator
tree_fold f acc Empty = acc


-- Apply the folding function to the current value and accumulator
-- Recursively fold the left subtree, then the right subtree
tree_fold f acc (Node {value = v, left = l, right = r}) =
    let acc' = f v acc 
    in tree_fold f (tree_fold f acc' l) r 


-- A type synonym for a comparison function that takes 
-- two keys of type `k` and returns an Ordering.
type Comparator k = k -> k -> Ordering


-- Define a dictionary data type that uses the BST to
-- store (key, value) pairs and includes a comparison function for keys.
data Dictionary k v = Dictionary {

    -- The BST that stores the key-value pairs
    bst :: BST (k, v),

    -- A comparison function to compare keys within the BST
    compareKeys :: Comparator k
}


-- A function to create an empty dictionary given a comparison function.
create_dict :: Comparator k -> Dictionary k v

-- Create a dictionary with an empty BST and the provided comparator
create_dict comp = Dictionary Empty comp

-- A function to find a value by its key in a dictionary.
find :: k -> Dictionary k v -> Maybe v

-- Use the findInBST helper function
find key (Dictionary bst comp) = findInBST key bst comp


-- A helper function for 'find' that operates on BSTs directly.
findInBST :: k -> BST (k,v) -> Comparator k -> Maybe v

-- If the BST is empty, return Nothing (key not found)
findInBST _ Empty _ = Nothing      

findInBST key (Node {value = (k,v), left = l, right = r}) comp =
    -- Compare the search key with the current node key
    case comp key k of
        -- If equal, the key is found, return the value
        EQ -> Just v
        -- If less, search the left subtree
        LT -> findInBST key l comp
        -- If greater, search the right subtree
        GT -> findInBST key r comp

-- A function to insert a value into the BST, 
-- parameterized by a comparison function.
insert_into_bst :: (a -> a -> Ordering) -> a -> BST a -> BST a
-- If the BST is empty, insert the value here
insert_into_bst _ val Empty = Node val Empty Empty

-- Otherwise, compare and decide where to insert
insert_into_bst comp val (Node x l r) =
    -- If the value is equal to the current node, don't insert
    case comp val x of
        -- If less, insert into the left subtree
        EQ -> Node val l r
        -- If less, insert into the left subtree
        LT -> Node x (insert_into_bst comp val l) r
        -- If greater, insert into the right subtree
        GT -> Node x l (insert_into_bst comp val r) 

-- A function to insert a key-value pair into a dictionary.
insert_into_dict :: k -> v -> Dictionary k v -> Dictionary k v
insert_into_dict key value dict@(Dictionary bst comp) =
    dict { bst = insert_into_bst (\(k1, _) (k2, _) 
    -> comp k1 k2) (key, value) bst }
    -- Update the dictionary's BST by inserting 
    -- the new (key, value) pair using the provided comparator

    
-- The main function, which creates a dictionary 
-- and demonstrates insertion and finding operations.
main :: IO ()
main = do

    -- Define a comparison function
    let comp = compare :: Int -> Int -> Ordering
    -- Create a new dictionary
    let dict = create_dict comp

    -- Insert several (key, value) pairs into the dictionary using foldl
    let inserts = [(1,'x'), (0,'y'), (3,'z'), (1,'a'), (4,'e')]
    let dict' = foldl (\d (k,v) -> insert_into_dict k v d) dict inserts

    -- Find values for keys in the range [-2..5] and print the results
    let finds = [ find i dict' | i <- [-2..5] ]
    print finds
