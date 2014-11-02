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
containsBST x EmptyBST = False  
containsBST x (Node a left right)   
    | x == a = True  
    | x < a  = containsBST x left  
    | x > a  = containsBST x right 
