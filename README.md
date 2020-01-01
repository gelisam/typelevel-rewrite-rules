# typelevel-rewrite-rules

(work in progress: this Readme documents the aspirations of this package, not its current status)

A typechecker plugin solving equations involving type-level lists. If GHC complains that it doesn't know how to deduce `((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs))`, add

    {-# OPTIONS_GHC -fplugin TypeList.Normalize #-}

to make the error go away.
