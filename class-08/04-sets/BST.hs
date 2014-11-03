module BST where

data BST a = EmptyBST | Node a (BST a) (BST a) deriving (Show, Eq)

-- Добавление элемента 
insertBST :: (Ord a) => a -> BST a -> BST a    
insertBST x EmptyBST = Node x EmptyBST EmptyBST   
insertBST x (Node a left right)     
    | x == a = Node a left right    
    | x < a  = Node a (insertBST x left) right    
    | x > a  = Node a left (insertBST x right) 

-- Принадлежность элемента
containsBST :: (Ord a) => a -> BST a -> Bool  
containsBST _ EmptyBST = False  
containsBST x (Node a left right)   
    | x == a = True  
    | x < a  = containsBST x left  
    | x > a  = containsBST x right 

-- Удаление элемента
deleteBST :: (Ord a) => a -> BST a -> BST a 
deleteBST _ EmptyBST = EmptyBST
deleteBST x (Node a left right)   
    | x == a = deleteNode (Node a left right)  
    | x < a  = deleteBST x left  
    | x > a  = deleteBST x right 
  where
    deleteNode (Node _ EmptyBST EmptyBST) = EmptyBST
    deleteNode (Node _ EmptyBST right) = right
    deleteNode (Node _ left EmptyBST) = left
    deleteNode (Node _ left right) = Node root left right'
      where 
        minKey (Node x EmptyBST _) = x
        minKey (Node _ l _) = minKey l
        root = minKey right
        right' = deleteBST root right
    
