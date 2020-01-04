import Data.Vinyl.TypeLevel.Test
import SameModule.Test  -- doesn't work :(
import SamePackage.Test
import TypeLevel.Append.Test


main :: IO ()
main = putStrLn "typechecks."
