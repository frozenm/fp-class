module TreeSet (Set, empty, isEmpty, add, contains, remove) where

import AbstractSet
import BST

newtype Set = SetImpl (BST Int)

instance AbstractSet Set where
  empty = SetImpl EmptyBST

  isEmpty (SetImpl EmptyBST) = True
  isEmpty (SetImpl _) = False

  add (SetImpl t) x = SetImpl (insertBST x t)

  contains (SetImpl t) x = containsBST x t

  remove (SetImpl t) x = SetImpl (deleteBST x t)
