{-# LANGUAGE DeriveFoldable #-}
module TypeList.Tree where


-- |
-- an expression like @(as ++ '[]) ++ bs@ would be represented as
-- @Branch (Branch (Leaf as) Nil) (Leaf bs)@
data Tree a
  = Nil
  | Leaf a
  | Branch (Tree a) (Tree a)
  deriving (Foldable, Show)


isSingletonTree
  :: Tree a -> Bool
isSingletonTree (Leaf _)
  = True
isSingletonTree _
  = False

isLeftAssociativeTree
  :: Tree a -> Bool
isLeftAssociativeTree Nil
  = True
isLeftAssociativeTree (Branch t (Leaf _))
  = isLeftAssociativeTree t

isRightAssociativeTree
  :: Tree a -> Bool
isRightAssociativeTree Nil
  = True
isRightAssociativeTree (Branch (Leaf _) t)
  = isRightAssociativeTree t


toLeftAssociativeTree
  :: [a] -> Tree a
toLeftAssociativeTree []
  = Nil
toLeftAssociativeTree xs
  = toLeftAssociativeTree (init xs) `Branch` Leaf (last xs)

toRightAssociativeTree
  :: [a] -> Tree a
toRightAssociativeTree []
  = Nil
toRightAssociativeTree (x:xs)
  = Leaf x `Branch` toRightAssociativeTree xs
