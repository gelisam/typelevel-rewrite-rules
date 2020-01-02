{-# LANGUAGE DeriveFoldable #-}
module TypeLevel.Rewrite.Tree where


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
isLeftAssociativeTree (Leaf _)
  = True
isLeftAssociativeTree (Branch t (Leaf _))
  = isLeftAssociativeTree t
isLeftAssociativeTree _
  = False

isRightAssociativeTree
  :: Tree a -> Bool
isRightAssociativeTree (Leaf _)
  = True
isRightAssociativeTree (Branch (Leaf _) t)
  = isRightAssociativeTree t
isRightAssociativeTree _
  = False


toLeftAssociativeTree
  :: [a] -> Tree a
toLeftAssociativeTree []
  = Nil
toLeftAssociativeTree [x]
  = Leaf x
toLeftAssociativeTree xs
  = toLeftAssociativeTree (init xs) `Branch` Leaf (last xs)

toRightAssociativeTree
  :: [a] -> Tree a
toRightAssociativeTree []
  = Nil
toRightAssociativeTree [x]
  = Leaf x
toRightAssociativeTree (x:xs)
  = Leaf x `Branch` toRightAssociativeTree xs
