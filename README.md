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

This is unfortunate because the equation is valid; for any three concrete `Nat`s `m`, `n` and `o`, ghc would gladly accept the equation as true, but when `m`, `n` and `o` are abstract, it gets stuck.

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

This plugin is both more general and more dangerous: it allows us to specify any rewrite rules we want, including invalid rules like `(n + 'Z) ~ 'Z` which break the type system:

```haskell
{-# LANGUAGE DataKinds, RankNTypes, TypeFamilies, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fplugin TypeLevel.Rewrite
                -fplugin-opt=TypeLevel.Rewrite:My.RewriteRules.Nonsense #-}
module My.Module where

import Prelude hiding ((++))

import Data.Proxy (Proxy(Proxy))
import Data.Type.Nat (Nat(Z, S), type (+))
import Data.Vec.Lazy (Vec((:::), VNil), (++))

import My.RewriteRules ()

withNonsense
  :: proxy n
  -> ((n + 'Z) ~ 'Z => r)
  -> r
withNonsense _ r = r

-- |
-- >>> recFromNowhere (Proxy @'Z)
-- VNil
-- >>> let (n ::: VNil) = recFromNowhere (Proxy @('S 'Z))
-- >>> n
-- internal error: interpretBCO: hit a CASEFAIL
recFromNowhere
  :: proxy n
  -> Vec (n + 'Z) Int
recFromNowhere proxy = withNonsense proxy VNil
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


## Alternatives

Remember, this typechecker plugin is dangerous! Have you considered these other, safer alternatives?


### Propagate the constraints

The easiest alternative is to propagate the constraints. We know that ghc would accept `(m + 'Z + n + 'Z + o) ~ (m + n + o)` if we had concrete `Nat`s for `m`, `n` and `o`; so let's wait until we have a concrete type-level values for them.

```haskell
simplify
  :: (m + 'Z + n + 'Z + o) ~ (m + n + o)
  => Vec  m a
  -> Vec 'Z a
  -> Vec  n a
  -> Vec 'Z a
  -> Vec  o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO

fooBarBazQuux :: Vec ('S ('S ('S ('S 'Z)))) String
fooBarBazQuux
  -- ('S ('S 'Z) + 'Z + 'S 'Z + 'Z + 'S 'Z) ~ ('S ('S 'Z) + 'S 'Z + 'S 'Z) holds
  -- because both sides evaluate to ('S ('S ('S ('S 'Z))))
  = simplify ("foo" ::: "bar" ::: VNil)
             VNil
             ("baz" ::: VNil)
             VNil
             ("quux" ::: VNil)
```

As you can see, once we call `simplify` with concrete values, the constraint gets discharged. The downside of this approach is that if there are a lot of intermediate calls between `simplify` and `fooBarBazQuux`, we might accumulate a lot of constraints. Also, sometimes this approach doesn't work when recursion is involved, because we would need to accumulate an infinite number of constraints.

```haskell
-- nope!
appendSingletons
  :: ( (m + 'Z) ~ m
     , (m + 'S 'Z + 'Z) ~ (m + 'S 'Z)
     , (m + 'S 'Z + 'S 'Z + 'Z) ~ (m + 'S 'Z + 'S 'Z)
     , ...
     , (m + 'S 'Z + n) ~ (m + 'S n)
     , (m + 'S 'Z + 'S 'Z + n) ~ (m + 'S 'Z + 'S n)
     , ...
     )
  => Vec m a
  -> Vec n (Vec ('S 'Z) a)
  -> Vec (m + n) a
appendSingletons xsM VNil
  -- uses (m + 'Z) ~ m
  = xsM
appendSingletons xsM (singleton1 ::: singletons)
  -- uses (m + 'S 'Z + n) ~ (m + 'S n)
  -- but we're recurring on a larger m, so we need to provide both
  -- (m + 'Z) ~ m and (m + 'S 'Z + n) ~ (m + 'S n) for that larger m;
  -- that is, we need to provide ((m + 'S 'Z) + 'Z) ~ (m + 'S 'Z) and
  -- ((m + 'S 'Z) + 'S 'Z + n) ~ ((m + 'S 'Z) + 'S n). But if we add
  -- those constraints to 'appendSingletons', we'll also need to provide
  -- those constraints for that larger m, etc.
  = appendSingletons (xsM ++ singleton1) singletons
```


### Hasochism

If you're worried about accidentally breaking the type system by writing an invalid rule like `(n + 'Z) ~ 'Z`, try proving it correct. The [Hasochism](http://homepages.inf.ed.ac.uk/slindley/papers/hasochism.pdf) paper explains how; but as the title implies, writing proofs in Haskell can be a lot more painful than doing it in a language like Agda which was built for writing proofs. Doing it in Agda is not a painless experience either... but in Haskell, we need to write a lot of boilerplate before we can even begin writing the proofs:

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}


-- called 'SNat' in "Data.Type.Nat"
data Natty n where
  Zy :: Natty 'Z
  Sy :: Natty n -> Natty ('S n)

addy
  :: Natty m
  -> Natty n
  -> Natty (m + n)
addy Zy ny
  = ny
addy (Sy my) ny
  = Sy (addy my ny)


-- called 'SNatI' in "Data.Type.Nat"
class NATTY n where
  natty :: Natty n

instance NATTY 'Z where
  natty = Zy

instance NATTY n => NATTY ('S n) where
  natty = Sy natty
```

Now that we have defined all of those, we can write the proofs. Here, I am proving `(n + 'Z) ~ n`, `((m + n) + o) ~ (m + (n + o))`, and `(m + n) ~ (n + m)`. The proofs are only a few lines long, but as the abundance of comments shows, careful thought is required in order to figure out what those few lines are.

```haskell
{-# LANGUAGE RankNTypes, ScopedTypeVariables, TypeApplications #-}

withRightIdentity
  :: Natty n
  -> ((n + 'Z) ~ n => r)
  -> r
withRightIdentity Zy r
  = r
    -- ('Z + 'Z) ~ 'Z  holds
    -- because ('Z + 'Z) evaluates to 'Z
withRightIdentity (Sy ny) r
  = withRightIdentity ny
    -- we now have (n + 'Z) ~ n
  $ r
    -- ('S n + 'Z) ~ 'S n  now holds
    -- because ('S n + 'Z) evaluates to ('S (n + 'Z))

withRightAssociative
  :: Natty m
  -> Natty n
  -> Natty o
  -> (((m + n) + o) ~ (m + (n + o)) => r)
  -> r
withRightAssociative Zy _ _ r
  = r
    -- (('Z + n) + o) ~ ('Z + (n + o))  holds
    -- because both sides evaluate to (n + o)
withRightAssociative (Sy my) ny oy r
  = withRightAssociative my ny oy
    -- we now have ((m + n) + o) ~ (m + (n + o))
  $ r
    -- (('S m + n) + o) ~ ('S m + (n + o))  now holds
    -- because (('S m + n) + o) evaluates to 'S ((m + n) + o)
    -- and ('S m + (n + o)) evaluates to 'S (m + (n + o))

withCommutative
  :: Natty m
  -> Natty n
  -> ((m + n) ~ (n + m) => r)
  -> r
withCommutative Zy ny r
  = withRightIdentity ny
    -- we now have (n + 'Z) ~ n
  $ r
    -- ('Z + n) ~ (n + 'Z)  now holds
    -- because both sides are equivalent to n
withCommutative my Zy r
  = withRightIdentity my
    -- we now have (m + 'Z) ~ m
  $ r
    -- (m + 'Z) ~ ('Z + m)  now holds
    -- because both sides are equivalent to m
withCommutative (Sy my) (Sy ny) r
  = withCommutative my (Sy ny)
    -- we now have (m + 'S n) ~ ('S n + m)
  $ withCommutative (Sy my) ny
    -- we now have ('S m + n) ~ (n + 'S m)
  $ withCommutative my ny
    -- we now have (m + n) ~ (n + m)
  $ r
    -- ('S m + 'S n) ~ ('S n + 'S m)  now holds
    -- because ('S m + 'S n) evaluates to 'S (m + 'S n)
    -- which is equivalent to 'S ('S (n + m))
    -- similarly ('S n + 'S m) becomes ('S ('S (m + n)))
    -- and ('S ('S n + m)) is equivalent to ('S ('S m + n))
```

One disadvantage of this approach is that careful thought is also needed when applying the properties we proved.

```
simplify
  :: forall m n o a. (NATTY m, NATTY n)
  => Vec  m a
  -> Vec 'Z a
  -> Vec  n a
  -> Vec 'Z a
  -> Vec  o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = withRightIdentity (natty @m)
    -- we now have (m + 'Z) ~ m
  $ withRightIdentity (natty @m `addy` natty @n)
    -- we now have ((m + n) + 'Z) ~ (m + n)
  $ (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO
    -- (((m + 'Z) + n) + 'Z) + o
    -- becomes ((m + n) + 'Z) + o
    -- then (m + n) + o
```

As before, `simplify` has some constraints which propagate until we get concrete values for `m` and `n`.

```haskell
fooBarBazQuux :: Vec ('S ('S ('S ('S 'Z)))) String
fooBarBazQuux
  = simplify ("foo" ::: "bar" ::: VNil)
             VNil
             ("baz" ::: VNil)
             VNil
             ("quux" ::: VNil)
```

This time, however, the constraints don't accumulate as much, because we can construct derived `Natty`s from existing ones. In particular, the recursive function which was giving us trouble before no longer requires an infinite number of constraints.

```haskell
appendSingletons
  :: forall m n a. (NATTY m, NATTY n)
  => Vec m a
  -> Vec n (Vec ('S 'Z) a)
  -> Vec (m + n) a
appendSingletons
  = go (natty @m) (natty @n)

go
  :: Natty m
  -> Natty n
  -> Vec m a
  -> Vec n (Vec ('S 'Z) a)
  -> Vec (m + n) a
go my Zy xsM VNil
  = withRightIdentity my
    -- we now have (m + 'Z) ~ m
  $ xsM
    -- m becomes (m + 'Z)
go my (Sy ny) xsM (singleton1 ::: singletons)
  = withCommutative my (natty @('S 'Z))
    -- we now have (m + 'S 'Z) ~ ('S 'Z + m)
  $ withRightAssociative my (natty @('S 'Z)) ny
    -- we now have ((m + 'S 'Z) + n) ~ (m + ('S 'Z + n))
    -- or equivalently ('S m + n) ~ (m + 'S n)
  $ go (Sy my) ny (xsM ++ singleton1) singletons
    -- (xsM ++ singleton1) has type (Vec (m + 'S 'Z) a)
    -- which becomes (Vec ('S 'Z + m) a) and then (Vec ('S m) a)
    -- the recursive call produces a (Vec ('S m + n) a)
    -- which becomes (Vec (m + 'S n) a)
```

Another disadvantage of this approach is that the proofs have a runtime cost, as we recur down the `Natty` in order to construct our type-level constraint. Especially if, like me, you write `withCommutative` using an `O(3^n)` algorithm in order to avoid having to also prove an extra lemma!


### Axiom

One way to avoid that runtime cost is to write a one-step "trust me" proof. Obviously, this brings us back to the danger zone.

The way to write a one-step "trust me" proof is not obvious, but can be found [in the innards of the `constraints` package](http://hackage.haskell.org/package/constraints-0.11.2/docs/src/Data.Constraint.Nat.html#axiom):

```haskell
{-# LANGUAGE DataKinds, PolyKinds, ScopedTypeVariables, TypeOperators #-}

import Data.Constraint (Dict(Dict), withDict)
import Data.Type.Nat (Nat(Z, S), type (+))
import Data.Vec.Lazy (Vec, (++))
import Unsafe.Coerce (unsafeCoerce)

axiom :: forall a b. Dict (a ~ b)
axiom = unsafeCoerce (Dict :: Dict (a ~ a))

simplify
  :: forall m n o a
   . Vec  m a
  -> Vec 'Z a
  -> Vec  n a
  -> Vec 'Z a
  -> Vec  o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = withDict (axiom :: Dict ((m + 'Z + n + 'Z + o) ~ (m + n + o)))
  $ (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO
```

Notice that the comments indicating what information we learn from applying each property are gone! That's supposed to illustrate another advantage of this approach, namely that we no longer need to think too hard about which proof to apply in order to get our function to typecheck. Just write the function without using `withDict`, look at the two type-level expressions which ghc says don't match, and if they look to you like they should match, use `axiom` to assert that they do.

The biggest disadvantage of this technique is that if we use `axiom` too often, we're likely to spend less and less time worrying about whether the expressions we're pasting from ghc really are equivalent, and so we're likely to accidentally break the type system.

It's better to restrict `axiom` to a much smaller number of definitions, such as the proofs of a few key properties.

```haskell
rightIdentity
  :: proxy n
  -> Dict ((n + 'Z) ~ n)
rightIdentity _
  = axiom

rightAssociative
  :: proxy m
  -> proxy n
  -> proxy o
  -> Dict (((m + n) + o) ~ (m + (n + o)))
rightAssociative _ _ _
  = axiom
```

Unfortunately, with that variant, we once again need some careful thought when applying the properties.

```haskell
simplify
  :: forall m n o a
   . Vec  m a
  -> Vec 'Z a
  -> Vec  n a
  -> Vec 'Z a
  -> Vec  o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = withDict (rightIdentity (Proxy @m))
    -- we now have (m + 'Z) ~ m
  $ withDict (rightIdentity (Proxy @(m + n)))
    -- we now have ((m + n) + 'Z) ~ (m + n)
  $ (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO
    -- (((m + 'Z) + n) + 'Z) + o
    -- becomes ((m + n) + 'Z) + o
    -- then (m + n) + o
```

This typechecker plugin, typelevel-rewrite-rules, can be thought as a way to get about the same level of safety as this variant, except that the properties are applied automatically, so we don't need to think hard at the use sites.

The reason it is not exactly the same level of safety is that while both approaches require us to vouch for the validity of a few key properties, typelevel-rewrite-rules has an extra way we can shoot ourselves in the foot: by writing a set of rules which loop indefinitely. This also means that there are properties, like commutativity, which cannot be expressed.


### ghc-typelits-natnormalise

In order to express properties like commutativity, we need something more sophisticated than rewrite rules: we need a typechecker plugin like [ghc-typelits-natnormalise](https://hackage.haskell.org/package/ghc-typelits-natnormalise), which already knows about commutativity and more.

```haskell
{-# LANGUAGE DataKinds, GADTs, TypeOperators #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}

import Prelude hiding ((++))

import GHC.TypeLits

data Vec n a where
  VNil  :: Vec 0 a
  (:::) :: a -> Vec n a -> Vec (1 + n) a

(++)
  :: Vec m a
  -> Vec n a
  -> Vec (m + n) a
(++) VNil xsN
  = xsN
(++) (x ::: xsM) xsN
  = x ::: (xsM ++ xsN)

simplify
  :: Vec m a
  -> Vec 0 a
  -> Vec n a
  -> Vec 0 a
  -> Vec o a
  -> Vec (m + n + o) a
simplify xsM empty1 xsN empty2 xsO
  = (((xsM ++ empty1) ++ xsN) ++ empty2) ++ xsO

appendSingletons
  :: Vec m a
  -> Vec n (Vec 1 a)
  -> Vec (m + n) a
appendSingletons xsM VNil
  = xsM
appendSingletons xsM (singleton ::: singletons)
  = appendSingletons (xsM ++ singleton) singletons
```

Like typelevel-rewrite-rules, ghc-typelits-natnormalise automatically solves the constraints it knows about, we don't have to manually apply the properties like we did with the Hasochism and Axiom approaches. Furthermore, since ghc-typelits-natnormalise already knows that `(+)` is associative and commutative, we don't have to state nor prove the properties which we expect to hold.

When ghc-typelits-natnormalise works, it works great! Unfortunately, it only works in one narrow situation: when the type indices are `Nat`s from `GHC.TypeLits`. That's why I had to redefine `Vec` above: I cannot use ghc-typelits-natnormalise with the `Vec`s from `Data.Vec.Lazy`, as they use the `Nat`s from `Data.Type.Nat` instead.


### Thoralf

The exact same code compiles with `ThoralfPlugin.Plugin` instead of `GHC.TypeLits.Normalise`. That's because [the Thoralf plugin](https://github.com/bgamari/the-thoralf-plugin#readme) also knows about the properties of `Nat`s from `GHC.TypeLits`. However, unlike ghc-typelits-natnormalise, Thoralf is [designed to be extensible](https://github.com/bgamari/the-thoralf-plugin/blob/master/DOCUMENTATION.md), so it's possible to teach Thoralf about the properties of `Nat`s from `Data.Type.Nat`! Here is the code which does so. Note that the `Vec`s in that code are `Thoralf`'s `Vec`s, not the `Vec`s from `Data.Vec.Lazy`. 

```haskell
{-# LANGUAGE GADTs, PackageImports, RankNTypes #-}
module ThoralfPlugin.Encode.Nat (natTheory) where

import "base" Control.Monad (guard)
import "ghc" DataCon (DataCon, promoteDataCon)
import "ghc" FastString (fsLit)
import "ghc" GhcPlugins (getUnique)
import "ghc" Module (Module, mkModuleName)
import "ghc" OccName (mkDataOcc, mkTcOcc)
import "ghc" TcPluginM (FindResult(..), TcPluginM, findImportedModule, lookupOrig, tcLookupDataCon, tcLookupTyCon)
import "ghc" TyCon (TyCon(..))
import "ghc" Type (Type, splitTyConApp_maybe, tyVarKind)
import ThoralfPlugin.Encode.TheoryEncoding

importModule :: String -> String -> TcPluginM Module
importModule packageName moduleName = do
  let package = fsLit packageName
  Found _ module_ <- findImportedModule (mkModuleName moduleName) (Just package)
  pure module_

findTyCon :: Module -> String -> TcPluginM TyCon
findTyCon md strNm = do
    name <- lookupOrig md (mkTcOcc strNm)
    tcLookupTyCon name

findDataCon :: Module -> String -> TcPluginM DataCon
findDataCon md strNm = do
    name <- lookupOrig md (mkDataOcc strNm)
    tcLookupDataCon name

expectTyCon :: TyCon -> Type -> Maybe [Type]
expectTyCon expectedTyCon ty = do
  (actualTyCon, args) <- splitTyConApp_maybe ty
  guard (actualTyCon == expectedTyCon)
  pure args

addZ3Numbers :: Vec n String -> String
addZ3Numbers VNil      = "0"
addZ3Numbers (x :> xs) = "(+ " ++ x ++ " " ++ addZ3Numbers xs ++ ")"

addExtraZ3Numbers :: [Int] -> Vec n String -> String
addExtraZ3Numbers = go addZ3Numbers
  where
    go :: (forall m. Vec m String -> String) -> [Int] -> Vec n String -> String
    go acc []     = acc
    go acc (x:xs) = go (acc . (show x :>)) xs

mkTyConvCont :: [Type] -> [Int] -> TyConvCont
mkTyConvCont args extra = go VNil (reverse args)
  where
    go :: Vec n Type -> [Type] -> TyConvCont
    go acc []     = let toZ3 z3Numbers VNil = addExtraZ3Numbers extra z3Numbers
                    in TyConvCont acc VNil toZ3 []
    go acc (x:xs) = go (x :> acc) xs

convertTyConToZ3 :: TyCon -> Int -> [Int] -> Type -> Maybe TyConvCont
convertTyConToZ3 tyCon argCount extra ty = do
  args <- expectTyCon tyCon ty
  guard (length args == argCount)
  pure $ mkTyConvCont args extra

natTheory :: TcPluginM TheoryEncoding
natTheory = do
  dataDotNat        <- importModule "fin" "Data.Nat"
  dataDotNatDotType <- importModule "fin" "Data.Type.Nat"
  nat <- findTyCon dataDotNat "Nat"
  z <- promoteDataCon <$> findDataCon dataDotNat "Z"
  s <- promoteDataCon <$> findDataCon dataDotNat "S"
  plus <- findTyCon dataDotNatDotType "Plus"
  pure $ emptyTheory
    { kindConvs = [ \ty -> do [] <- expectTyCon nat ty
                              pure $ KdConvCont VNil (\VNil -> "Int")
                  ]
    , tyVarPreds = \tv -> do _ <- expectTyCon nat (tyVarKind tv)
                             pure ["(assert (<= 0 " ++ show (getUnique tv) ++ "))"]
                   
    , typeConvs = [ convertTyConToZ3 z    0 []
                  , convertTyConToZ3 s    1 [1]
                  , convertTyConToZ3 plus 2 []
                  ]
    }
```

That example code illustrates a few disadvantages of this approach. First, the module name `ThoralfPlugin.Encode.Nat`. This hints at the disadvantage that the way in which we extend Thoralf is rather invasive: we don't extend Thoralf by importing a library or by pointing it to some extension code, but by forking the Thoralf repository and adding a new module to its source code. Next, the imports from the "ghc" package. This hints at the disadvantage that we need to be familiar with (a small part of) the ghc API in order to implement the part of the extension which imports modules, types and data constructors, and the part which converts type expressions into Z3 expressions. The final disadvantage is that we also need to be familiar with the Z3 syntax; although as you can see, here I am simply converting `Nat` to `Int` and `'S ('S 'Z)` to `(+ 1 (+ 1 0))`, so it's not that hard.

Once again, we don't have to state the properties which we expect to hold, as Z3 already knows that `(+)` is associative and commutative. Unlike with ghc-typelits-natnormalise, this does not mean we are limited to a single definition of `Nat` and `(+)`. Z3 only knows about one `(+)`, but it doesn't know about any particular Haskell definition of `(+)`; the magic of Thoralf is that it allows us to translate all the Haskell definitions of `(+)` to Z3's definition of `(+)`, which in turn allows Thoralf to solve type-equality constraints for all of those Haskell definitions.

Z3 is an SMT solver, where "SMT" stands for "Satisfiability-Modulo-Theories" and "theories" refers to the set of types and functions like `Int` and `(+)` which the solver knows about. Thankfully, the translation doesn't have to be one-to-one, and so it is possible to combine several Z3 types and functions in order to encode Haskell types and function for which Z3 doesn't have an equivalent. For example, Z3 only knows about integers, not about natural numbers, and so above we used `(assert (<= 0 n))` to encode Haskell's `Nat`s as non-negative Z3 `Int`s. 

The "satisfiability solver" part means that Thoralf is not rewriting the type equalities it encounters to hopefully-simpler type equalities using the properties it knows about, but rather, it searches for a counter-example which would demonstrate that the type equality is invalid. Even though there are infinitely-many `Int`s, Z3 somehow manages to exhaustively search the space in a finite amount of time, and so if Z3 cannot find a counter-example, then Thoralf knows that the type equality holds and discharges the constraint.


### LiquidHaskell

[LiquidHaskell](https://github.com/ucsd-progsys/liquidhaskell#readme) is in a slightly different category than the other approaches because it doesn't discharge type-equality constraints. Nevertheless, it is very similar to Thoralf in that it also uses an SMT solver to figure out whether to accept or reject the program. Instead of looking at equality constraints, LiquidHaskell looks at `{-@ ... @-}` annotations which specify more precise type signatures for our Haskell functions. Those type signatures use refinement types, which look similar to the GADT-based type signatures we've been using up to now, except they use predicates and support subtyping: if P implies Q, then `{v : T | P}` is a subtype of `{v : T | Q}`.

```haskell
{-@
measure myLen :: [a] -> Int
myLen []     = 0
myLen (x:xs) = 1 + myLen xs
@-}

type Vec a = [a]

{-@ type VecN a N = {v : Vec a | myLen v = N} @-}

{-@ assume (++) :: xs:Vec a -> ys:Vec a -> VecN a {myLen xs + myLen ys} @-}

{-@ simplify
      :: xs:Vec a
      -> VecN a 0
      -> ys:Vec a
      -> VecN a 0
      -> zs:Vec a
      -> VecN a {myLen xs + myLen ys + myLen zs} @-}
simplify :: [a] -> [a] -> [a] -> [a] -> [a] -> [a]
simplify xs empty1 ys empty2 zs
  = (((xs ++ empty1) ++ ys) ++ empty2) ++ zs

{-@ appendSingletons
      :: xs:Vec a
      -> singletons:Vec (VecN a 1)
      -> VecN a {myLen xs + myLen singletons} @-}
appendSingletons :: [a] -> [[a]] -> [a]
appendSingletons xsM []
  = xsM
appendSingletons xsM (singleton : singletons)
  = appendSingletons (xsM ++ singleton) singletons
```

Once again, LiquidHaskell knows that `(+)` is associative and commutative, so we don't have to state the properties which we expect to hold. We don't need to write converters from Haskell types and functions to Z3 types and functions like we did with Thoralf, which means we have to use the types and functions which LiquidHaskell already knows about. Thankfully, these types and functions are the `Int` and `(+)` from Haskell which we are already familiar with, not Z3's `Int` and `(+)`. The way in which we write the `P` and `Q` predicates for our refinement types is also familiar: we write ordinary recursive Haskell functions, such as `myLen` above.

The main disadvantage of this approach is the same as with ghc-typelits-natnormalise: we cannot reuse the existing `Vec` from `Data.Vec.Lazy` nor the existing `Nat` from `Data.Type.Nat`, we have to define a separate type inside LiquidHaskell's framework. There are several reasons for this. First, as I've just explained, we are limited to the types which LiquidHaskell already knows about, and `Nat` is not on that list. Second, `Vec` is a GADTs which uses a `Nat` as a type index, but LiquidHaskell uses refinement types, not GADTs. Finally, LiquidHaskell's refinement types are stricter than the Haskell types it refines, and so LiquidHaskell provides more type safety by rejecting more programs. By contrast, our original `Vec`-based program was already rejected by ghc's regular type checker, and so in order to reuse `Vec`, we need an approach which rejects fewer programs, by discharging some constraints.

In the example above, while we were not able to reuse `Vec`, we were able to use lists, a much more ubiquitous type than `Vec` for which many more functions already exist. In fact, `appendSingletons = foldl' (++)`! Unfortunately, we cannot use that simpler definition, because LiquidHaskell needs to observe the recursive call in order to confirm that `appendSingletons` does have the refined type we stated. 
