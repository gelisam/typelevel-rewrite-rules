# Type-Level Rewrite Rules [![Hackage](https://img.shields.io/hackage/v/typelevel-rewrite-rules.svg)](https://hackage.haskell.org/package/typelevel-rewrite-rules) [![Build Status](https://secure.travis-ci.org/gelisam/typelevel-rewrite-rules.png?branch=master)](http://travis-ci.org/gelisam/typelevel-rewrite-rules)

Solve type equalities using custom type-level rewrite rules like `(n + 'Z) ~ n` and `((m + n) + o) ~ (m + (n + o))`.


## The problem

Type equalities involving type families sometimes get stuck:

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
module My.Module where

import Prelude hiding ((++))

import Data.Type.Nat (Nat(Z, S), type (+))
import Data.Vec.Lazy (Vec, (++))

-- Couldn't match type ‘(((m + 'Z) + n) + 'Z) + o’
--                with ‘(m + n) + o’
simplify
  :: Vec  m a
  -> Vec 'Z a
  -> Vec  n a
  -> Vec 'Z a
  -> Vec  o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO
```

This is unfortunate because the equation is valid; for any three concrete `Nat`s `l`, `m` and `n`, ghc would gladly accept the equation as true, but when `l`, `m` and `n` are abstract, it gets stuck.

```haskell
-- ok
simplifyConcrete
  :: Vec ('S 'Z) a
  -> Vec 'Z a
  -> Vec ('S ('S 'Z)) a
  -> Vec 'Z a
  -> Vec ('S ('S ('S 'Z))) a
  -> Vec ('S 'Z + 'S ('S 'Z) + 'S ('S ('S 'Z))) a
simplifyConcrete xsM empty1 xsN empty2 xsO
  = (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO
```

The reason this particular type family gets stuck is because it pattern-matches on its left argument. Pattern-matching proceeds as designed when that argument is a known type-level value like `'S 'Z`, but when that argument is the type variable `m`, evaluation cannot proceed until we learn more about `m`.

```haskell
module Data.Type.Nat where

type family (+) (m :: Nat) (n :: Nat) :: Nat where
  'Z   + n = n
  'S m + n = 'S (m + n)
```


## The solution

First, define some rewrite rules. Each rewrite rule has a name, some variables, a left-hand side, and a right-hand side. The left-hand side gets rewritten to the right-hand side. Syntactically, a rewrite rule is defined via a constraint type synonym expanding to a type equality between the two sides.

For technical reasons, these must be defined in a different module than the one in which the stuck constraints appear, but they could be defined in the same module as the one which defines the type family.

```haskell
{-# LANGUAGE ConstraintKinds, DataKinds, TypeFamilies, TypeOperators #-}
module My.RewriteRules where

import Data.Type.Nat (Nat(Z, S), type (+))

type RightIdentity n
  = (n + 'Z) ~ n
type RightAssociative m n o
  = ((m + n) + o) ~ (m + (n + o))
```

Next, coming back to our original module, point the `TypeLevel.Rewrite` plugin to those rewrite rules. The type error disappears!

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:My.RewriteRules.RightIdentity
                -fplugin-opt=TypeLevel.Rewrite:My.RewriteRules.RightAssociative #-}
module My.Module where

import Prelude hiding ((++))

import Data.Type.Nat (Nat(Z, S), type (+))
import Data.Vec.Lazy (Vec, (++))

-- the module which contains the rewrite rules must be imported
import My.RewriteRules ()

-- now ok!
simplify
  :: Vec  m a
  -> Vec 'Z a
  -> Vec  n a
  -> Vec 'Z a
  -> Vec  o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO
```

For this particular type equality, it just so happens that either of the two rewrite rules would have been sufficient on its own. With only `RightIdentity`, the two `(+ 'Z)`s get removed from `(((m + 'Z) + n) + 'Z) + o`, leaving `(m + n) + o`. With only `RightAssociative`, `(((m + 'Z) + n) + 'Z) + o` gets rewritten to `m + ('Z + (n + ('Z + o)))`, and now `(+)` can pattern-match on the `'Z`s and evaluate to `m + (n + o)`. Meanwhile, the right-hand side `(m + n) + o` also gets rewritten to `m + (n + o)`. With both `RightIdentity` and `RightAssociative`, both sides get rewritten to `m + (n + o)`.


## Dangers

Typechecker plugins are used to extend ghc with domain-specific knowledge about particular types. For example, [ghc-typelits-natnormalise](https://hackage.haskell.org/package/ghc-typelits-natnormalise) simplifies type equalities involving natural numbers. It is the plugin's responsibility to ensure its simplifications are valid.

This plugin is both more general and more dangerous: it allows you to specify any rewrite rules you want, including invalid rules like `(n + 'Z) ~ 'Z` which break the type system:

```haskell
{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:My.RewriteRules.Nonsense #-}
module My.Module where

import Data.Functor.Identity
import Data.Proxy
import Data.Vinyl
import Data.Vinyl.TypeLevel

import My.RewriteRules ()

withNonsense
  :: proxy as
  -> ((as ++ '[]) ~ '[] => r)
  -> r
withNonsense _ r = r

-- |
-- >>> recFromNowhere (Proxy @'[])
-- {}
-- >>> recFromNowhere (Proxy @'[Int, String])
-- error: Impossible case alternative
recFromNowhere
  :: proxy as
  -> Rec Identity (as ++ '[])
recFromNowhere proxy = withNonsense proxy RNil
```

A more subtle danger is that even rewrite rules which are valid, such as `(x + y) ~ (y + x)`, can be problematic. The problem with this rule is that the right-hand side matches the left-hand side, and so the rewrite rule can be applied an infinite number of times to switch the arguments back and forth without making any progress. The same problem occurs if both `((m + n) + o) ~ (m + (n + o))` and `(m + (n + o)) ~ ((m + n) + o)` are included, the parentheses can get rearranged back and forth indefinitely.


## Troubleshooting

Most error messages should be self-explanatory, but there are a few circumstances in which the ghc API produces a confusing error message without giving me the opportunity to replace it with a better one. So if you encounter one of those confusing error messages, hopefully google will lead you to this page explaining what they mean.

### `GHC internal error: ‘My.Module.MyRule’ is not in scope during type checking, but it passed the renamer tcl_env of environment: []`

This means you are in `My.Module` and you are trying to use a rewrite rule which is also defined in `My.Module`. This is unfortunately not supported.

Solution: move your rewrite rule to another module and import that module from `My.Module`.

### `attempting to use module ‘My.RewriteRules’ which is not loaded`

This one is annoying because whether it happens or not depends on the order in which ghc chooses to compile your modules. If ghc happens to compile the module which uses your rewrite rules before the module which defines your rewrite rules, you'll get that error message.

Solution: add an `import My.RewriteRules ()` statement to force ghc to compile `My.RewriteRules` first.

### `Can't find interface-file declaration for type constructor or class My.RewriteRules.MyRule`

That error message is misleadingly followed by `Probable cause: bug in .hi-boot file, or inconsistent .hi file`, but the actual cause is that `MyRule` simply isn't defined in `My.RewriteRules`. Maybe it's a typo?


