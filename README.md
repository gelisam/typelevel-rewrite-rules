# typelevel-rewrite-rules

## Current status

A typechecker plugin solving equations involving type-level lists. If GHC complains that it doesn't know how to deduce `((as ++ bs) ++ cs) ~ (as ++ (bs ++ cs))` or `(as ++ '[]) ~ as`, add

    {-# OPTIONS_GHC -fplugin TypeLevel.Rewrite #-}

to make the error go away.

## Aspiration

The above is a good start, but it was a lot of work for just one typelevel operation. I think it wouldn't be too hard to generalize the typechecker plugin so that the user can specify which implementation of typelevel concatenation they want to use. Some of those definitions pattern-match on their second argument rather than their first, in which case it would make more sense to rewrite the expressions to be right-associative rather than rewriting them to be left-associative, so it would be useful to also let the user specify which way to perform the rewrite. While we're at it, we may also allow the user to specify a custom identity element, so that they may apply the rewrite to other associative operations than typelevel append. And maybe that identity element should be optional, so that we also support semigroup-but-not-monoidal operations.

At this point, I realized that the most general version of this typechecker plugin would be a system for specifying arbitrary rewrite rules, for example `((as ++ bs) ++ cs) = (as ++ (bs ++ cs))` and `(as ++ '[]) = as`.
