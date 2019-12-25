{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeList.Normalize #-}
import TypeList.Append


ex1a :: (((as ++ '[]) ++ (bs ++ '[])) ~ (as ++ bs) => r)
     -> proxy as
     -> proxy bs
     -> r
ex1a r _ _ = r

ex1b :: ((as ++ bs) ~ ((as ++ '[]) ++ (bs ++ '[])) => r)
     -> proxy as
     -> proxy bs
     -> r
ex1b r _ _ = r

ex2a :: (((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs)) => r)
     -> proxy as
     -> proxy bs
     -> proxy cs
     -> r
ex2a r _ _ _ = r

ex2b :: ((as ++ (bs ++ cs)) ~ ((as ++ bs) ++ cs) => r)
     -> proxy as
     -> proxy bs
     -> proxy cs
     -> r
ex2b r _ _ _ = r


main :: IO ()
main = putStrLn "typechecks."
