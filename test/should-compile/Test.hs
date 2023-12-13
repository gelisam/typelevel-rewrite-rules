import Data.Vinyl.TypeLevel.Test ()
import GHC.TypeLits.Test ()
import InstanceConstraints.Test ()
import SamePackage.Test ()
import TypeLevel.Append.Test ()
import UnwrapIdentity.Test ()


main :: IO ()
main = putStrLn "typechecks."
