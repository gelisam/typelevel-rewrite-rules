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

ex1c :: ((as ++ (bs ++ cs)) ~ (((as ++ '[]) ++ (bs ++ '[])) ++ (cs ++ '[])) => r)
     -> proxy as
     -> proxy bs
     -> proxy cs
     -> r
ex1c r _ _ _ = r

ex1d :: (proxy (as ++ bs) ~ proxy ((as ++ '[]) ++ (bs ++ '[])) => r)
     -> proxy as
     -> proxy bs
     -> r
ex1d r _ _ = r

ex1e :: proxy as
     -> proxy bs
     -> proxy (as ++ bs)
     -> proxy ((as ++ '[]) ++ (bs ++ '[]))
ex1e _ _ r = r


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

ex2c :: ((as ++ (bs ++ (cs ++ ds))) ~ (((as ++ bs) ++ cs) ++ ds) => r)
     -> proxy as
     -> proxy bs
     -> proxy cs
     -> proxy ds
     -> r
ex2c r _ _ _ _ = r

ex2d :: (proxy (as ++ (bs ++ cs)) ~ proxy ((as ++ bs) ++ cs) => r)
     -> proxy as
     -> proxy bs
     -> proxy cs
     -> r
ex2d r _ _ _ = r

ex2e :: proxy as
     -> proxy bs
     -> proxy cs
     -> proxy (as ++ (bs ++ cs))
     -> proxy ((as ++ bs) ++ cs)
ex2e _ _ _ r = r


main :: IO ()
main = putStrLn "typechecks."
