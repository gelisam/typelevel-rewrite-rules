import Data.Vinyl.TypeLevel.Test ()
import GHC.TypeLits.Test ()
import SamePackage.Test ()
import TypeLevel.Append.Test ()


main :: IO ()
main = putStrLn "typechecks."
