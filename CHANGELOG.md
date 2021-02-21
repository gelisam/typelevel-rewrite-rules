# 1.0
* Now supports ghc-8.10! Unfortunately, ghc-8.6 and ghc-8.8 are no longer
  supported.
* Now using ghc's builtin iteration limit instead of a hardcoded internal
  limit. Since ghc's limit is much smaller than our internal limit was, you may
  need to add something like `{-# OPTIONS_GHC -fconstraint-solver-iterations=100 #-}`
  if ghc recommends you to do so.
* Now making use of givens. That is, you can now call a function expecting
  (xs ~ rewritten-expr) given (xs ~ expr). Previously, you could only call a
  function expecting (xs ~ expr) given (xs ~ rewritten-expr).
* Now supports instance constraints. That is, you can now call a function which
  requires an `Eq (Vec rewritten-expr)` given an `Eq (Vec expr)`. Previously,
  we only supported equality constraints (`expr ~ rewritten-expr`).
* Now works with type variables of kind Nat and Symbol.
* The generated code now passes ghc's core-lint check.
* Error messages involving rewritten constraints now include the relevant
  rewrite rules.
* Bugfix: a spurious "the substitution forms a cycle" message was sometimes
  emitted even when the substitution rules did not form a cycle (see #15).
* Bugfix: rewrite rules were sometimes not firing (see #21).
* Bugfix: error messages were sometimes missing the error location (see #17).

# 0.1
* initial release
