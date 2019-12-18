{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeList.Normalize #-}
import TypeList.Append


ex1 :: (((as ++ '[]) ++ (bs ++ '[])) ~ (as ++ bs) => r)
    -> proxy as
    -> proxy bs
    -> r
ex1 r _ _ = r

ex2 :: (((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs)) => r)
    -> proxy as
    -> proxy bs
    -> proxy cs
    -> r
ex2 r _ _ _ = r


main :: IO ()
main = putStrLn "typechecks."
