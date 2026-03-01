# Overlays: `Package` Indexed by `pkgs` + Backpack Composition

In `encoding.md`, `Package` is indexed by `name` only. This makes
recipes package-set-independent — but it means overlays cannot provide
a different recipe for the same name. This document explores indexing
`Package` by both `name` and `pkgs`, and using Cabal Backpack for
overlay composition.

## §1 The Problem

```haskell
-- encoding.md: one global recipe per name
class Package (name :: Symbol) where
  callPackage :: forall pkgs -> Deps name pkgs => Output name
```

An overlay that replaces openssl's recipe cannot exist — there is only
one `Package "openssl"` instance. The `Has` class controls membership
but not the recipe.

## §2 `Package` Indexed by `(name, pkgs)`

Make the package set a class parameter:

```haskell
class Package (name :: Symbol) (pkgs :: Type) where
  type Deps name pkgs :: Constraint
  type Output name :: Type
  type Output name = Derivation name
  callPackage :: Deps name pkgs => Output name
```

Now different package sets can provide different recipes:

```haskell
-- Base recipe
instance Package "openssl" Nixpkgs where
  type Deps "openssl" Nixpkgs = (Has "stdenv" Nixpkgs, Has "zlib" Nixpkgs)
  callPackage = ...

-- Overlay overrides it
instance Package "openssl" (MyOverlay Nixpkgs) where
  type Deps "openssl" (MyOverlay Nixpkgs) = (Has "stdenv" (MyOverlay Nixpkgs), Has "zlib" (MyOverlay Nixpkgs))
  callPackage = ... -- patched version
```

`Has` inherits via `OVERLAPPABLE`:

```haskell
data MyOverlay pkgs

-- Inherit: anything in pkgs is in MyOverlay pkgs
instance {-# OVERLAPPABLE #-} Has name pkgs => Has name (MyOverlay pkgs)

-- Override: openssl is provided by the overlay
instance {-# OVERLAPPING #-} Has "openssl" (MyOverlay Nixpkgs)
```

And `Package` inherits the same way:

```haskell
-- Inherit: non-overridden recipes fall through to the base
instance {-# OVERLAPPABLE #-} Package name pkgs => Package name (MyOverlay pkgs) where
  type Deps name (MyOverlay pkgs) = Deps name pkgs
  type Output name = Output name
  callPackage = callPackage @name @pkgs
```

Non-overridden packages inherit their recipe from the base but resolve
their dependencies against the overlay — because `Has` resolution goes
through `MyOverlay pkgs`. This is `final:` semantics: the downstream
dependencies see the overridden world.

## §3 `final:` and `prev:`

In Nix:

```nix
overlay = final: prev: {
  openssl = prev.openssl.overrideAttrs { ... };
};
```

In this encoding:

- **`final:`** = the outermost type, e.g. `MyOverlay Nixpkgs`. All
  `callPackage` calls resolve `Has` against this type.
- **`prev:`** = the wrapped `pkgs`. The overlay can reach back:
  `callPackage @"openssl" @pkgs` gets the base openssl.

```haskell
instance Package "openssl" (MyOverlay Nixpkgs) where
  type Deps "openssl" (MyOverlay Nixpkgs) =
    (Has "openssl" Nixpkgs, Has "stdenv" (MyOverlay Nixpkgs))
  callPackage =
    let prev = callPackage @"openssl" @Nixpkgs   -- prev.openssl
        mkDrv = callPackage @"stdenv" @(MyOverlay Nixpkgs)  -- final.stdenv
    in Derivation $ patchOpenssl prev mkDrv
```

The `Nixpkgs` in `Has "openssl" Nixpkgs` is `prev:`. The
`MyOverlay Nixpkgs` everywhere else is `final:`.

Overlay stacking is type nesting:

```haskell
type MyStack = Overlay2 (Overlay1 Nixpkgs)
-- callPackage @"curl" @MyStack
-- resolves Has against Overlay2, falls through to Overlay1, then Nixpkgs
```

## §4 Override Patterns

In Nix, both `callPackage` and `mkDerivation` return overridable
results:

```nix
pkgs.openssl.override { zlib = zlib-ng; }           -- change deps
pkgs.openssl.overrideAttrs (old: { patches = ...; }) -- change build attrs
```

Both are powered by `makeOverridable`, which attaches an `override`
attribute to any function result. The key insight: override is just
**function composition** — an advice that wraps the original recipe.

### 4.1 Recipe vs `callPackage`

Separate the **recipe** (the `.nix` file — a function from deps to
output) from **`callPackage`** (auto-wiring — resolve deps from the
package set, apply the recipe, seal in `Derivation`):

```haskell
class Package (name :: Symbol) (pkgs :: Type) where
  type Deps name pkgs :: Constraint
  type Output name :: Type
  type Output name = Derivation name

  recipe :: Overridable (DepsOf name -> ResultOf name)
         => DepsOf name -> ResultOf name
```

`recipe` is the raw build logic. `callPackage` is derived:

```haskell
callPackage :: forall name pkgs.
               (Package name pkgs, Deps name pkgs)
            => Derivation name
callPackage =
  let deps = resolveDeps @name @pkgs    -- from Has constraints
  in Derivation $ recipe @name deps     -- apply and seal
```

The `Overridable` constraint on `recipe` is what makes overrides
possible — it's a function that can be advised before `callPackage`
seals the result in an existential `Derivation`.

### 4.2 The `Overridable` Class

A single-method typeclass whose dictionary IS a function:

```haskell
class Overridable func where
  mkOverridable :: func -> func
  mkOverridable = id

instance Overridable func  -- blanket instance: default is identity
```

Since `Overridable` has one method `func -> func` with default `id`,
the blanket instance makes every function "overridable" — with the
trivial override (no change). The real power comes from replacing
this dictionary dynamically.

### 4.3 `withDict` Magic

GHC 9.4's `withDict` creates a typeclass dictionary from a function
at runtime. Since `Overridable func` is a single-method class, its
dictionary IS the `func -> func` function:

```haskell
override0 :: forall func r.
             (func -> func)
          -> (Overridable func => r)
          -> r
override0 = withDict @(Overridable func)
```

`override0` takes an advice function and installs it as the
`Overridable` dictionary for the body. Inside the body,
`mkOverridable` IS the advice.

### 4.4 Composable `override`

The key combinator — applies an advice while preserving
overridability of the result:

```haskell
override :: forall func r.
            (func -> func)                    -- advice
         -> (Overridable func => r)           -- body (uses recipe)
         -> (Overridable func => r)           -- result is still overridable
override advice body =
  let composedAdvice = mkOverridable . advice
  --                   ^^^^^^^^^^^^^   ^^^^^^
  --                   existing        new advice
  in override0 composedAdvice body
```

The composition order `mkOverridable . advice` means: apply the new
advice first, then wrap with any existing override. This gives the
correct layering when chaining:

```haskell
override advice2 $ override advice1 $ body
```

Trace:

1. Outermost `override advice2`: installs `advice2` as mkOverridable
   (composed with outermost `mkOverridable = id`)
2. Inner `override advice1`: reads `mkOverridable` (= `advice2`),
   composes: `advice2 . advice1`
3. Body sees `mkOverridable = advice2 . advice1`
4. `mkOverridable recipe = advice2 (advice1 recipe)`

Later (outer) overrides wrap earlier (inner) ones. This matches
the Emacs advice pattern: `:around` advice composes with
previously installed advice.

### 4.5 The Emacs Advice Analogy

Override is exactly Emacs's `advice-add` with `:around`:

| Emacs | Derix |
|---|---|
| `:filter-args` (pre-compose) | `override (\f -> f . modifyArgs)` |
| `:filter-return` (post-compose) | `override (\f -> modifyResult . f)` |
| `:around` (full control) | `override (\f -> \args -> ...)` |
| `advice-add` | `override` |
| advised function | `mkOverridable` |

`:around` subsumes the others — it receives the original function
and can do anything. In Haskell, this is just higher-order functions.

### 4.6 Interaction with Overlays

Override operates BEFORE `callPackage` seals the result in the
existential `Derivation`. This is critical: once inside
`Derivation name = forall a. HasDrv a => Derivation a`, the inner
type `a` is unknown and cannot be modified.

In an overlay, `recipe` is overridden via `override` within the
`Package` instance:

```haskell
instance Package "openssl" (MyOverlay pkgs) where
  type Deps "openssl" (MyOverlay pkgs) =
    (Package "openssl" pkgs, Has "stdenv" (MyOverlay pkgs))
  recipe =
    override (\f args -> f (args { patches = patches args ++ [fixPatch] }))
             (recipe @"openssl" @pkgs)
    --        ^^^^^^^^^^^^^^^^^^^^^^^
    --        prev: recipe, still overridable
```

The overlay references `prev:` (the base recipe), advises it, and
the result remains overridable for further overlays. One-package
override IS a one-package overlay.

## §5 Backpack for Overlay Composition

The typeclass encoding handles resolution *within* a package set.
Backpack handles *composition of overlays* at the Cabal level.

### 5.1 Package Recipes as Backpack Modules

Each package recipe lives in its own module. It depends on an abstract
`Pkgs` signature:

```haskell
-- src/Recipe/Openssl.hs
module Recipe.Openssl where

import Pkgs  -- abstract: Backpack signature

opensslRecipe :: Derivation "openssl"
opensslRecipe = callPackage @"openssl" @PkgSet
```

The signature declares what the recipe needs:

```haskell
-- src/Pkgs.hsig
signature Pkgs where

data PkgSet
instance Has "stdenv" PkgSet
instance Has "zlib" PkgSet
instance Package "openssl" PkgSet
```

### 5.2 Package Sets as Backpack Implementations

A concrete package set is a module that implements the signature:

```haskell
-- nixpkgs/Pkgs.hs
module Pkgs where

data PkgSet = PkgSet  -- or: type PkgSet = Nixpkgs

instance Has "stdenv" PkgSet
instance Has "zlib" PkgSet
instance Has "openssl" PkgSet
-- ...
instance Package "openssl" PkgSet where ...
instance Package "zlib" PkgSet where ...
```

### 5.3 Overlays as Cabal Mixins

An overlay is a Cabal package that `requires` a base `Pkgs` and
provides a new one:

```cabal
-- my-overlay.cabal
library
  signatures: BasePkgs
  exposed-modules: Pkgs
  -- Pkgs re-exports everything from BasePkgs,
  -- overriding openssl
```

Composition via `mixins` with `as`:

```cabal
-- my-project.cabal
library
  build-depends:
    , nixpkgs-base
    , security-overlay
    , my-overlay
  mixins:
    -- Wire base into security-overlay
    nixpkgs-base          (Pkgs as BasePkgs)
    -- Wire security-overlay's output into my-overlay
    security-overlay      (Pkgs as BasePkgs)
    -- Final Pkgs comes from my-overlay
    my-overlay            (Pkgs as Pkgs)
```

The `requires` clause declares what an overlay needs (the base set).
The `as` clause wires overlays together — renaming modules to connect
one overlay's output to another's input.

### 5.4 What Backpack Adds

| Concern | Typeclasses | Backpack |
|---|---|---|
| Resolution | GHC constraint solver | — |
| Recipe parameterization | `Package name pkgs` | `requires Pkgs` |
| Overlay stacking | Type nesting `O2 (O1 Base)` | `mixins` with `as` |
| Separate compilation | No (global instances) | Yes |
| Composition unit | Instance declaration | Cabal component |

Backpack gives **separate compilation** of overlays — each overlay is
compiled independently against the signature, not against a concrete
package set. The wiring happens at link time via Cabal's `mixins`.

The typeclass approach (§2–§3) encodes everything within GHC's type
system. The Backpack approach lifts overlay composition to the build
system level, which may be more natural for large overlay stacks.

## §6 Bootstrap from Seed

Nixpkgs and Guix bootstrap a full toolchain from a minimal trusted
seed through a sequence of stages. Each stage uses the previous stage's
tools to build better tools. This is overlay stacking.

### 6.1 Stages as Overlays

```haskell
-- The seed: pre-built bootstrap binaries, no dependencies
data Seed

instance Has "bootstrap-gcc"    Seed
instance Has "bootstrap-glibc"  Seed
instance Has "bootstrap-bash"   Seed

instance Package "bootstrap-gcc" Seed where
  type Deps "bootstrap-gcc" Seed = ()
  type Output "bootstrap-gcc" = MkDerivationArgs -> Drv
  callPackage = ... -- fetches pre-built binary

-- Stage 1: build a real gcc using the bootstrap tools
data Stage1 pkgs

instance {-# OVERLAPPABLE #-} Has name pkgs => Has name (Stage1 pkgs)
instance {-# OVERLAPPING #-}  Has "gcc" (Stage1 Seed)
instance {-# OVERLAPPING #-}  Has "stdenv" (Stage1 Seed)

instance Package "gcc" (Stage1 Seed) where
  type Deps "gcc" (Stage1 Seed) =
    (Has "bootstrap-gcc" Seed, Has "bootstrap-glibc" Seed)
  callPackage =
    let cc   = callPackage @"bootstrap-gcc" @Seed
        libc = callPackage @"bootstrap-glibc" @Seed
    in Derivation $ buildGcc cc libc

instance Package "stdenv" (Stage1 Seed) where
  type Deps "stdenv" (Stage1 Seed) = Has "gcc" (Stage1 Seed)
  callPackage = \args ->
    let gcc = callPackage @"gcc" @(Stage1 Seed)
    in buildWithStdenv gcc args

-- Stage 2: rebuild glibc using Stage 1's gcc
data Stage2 pkgs

instance {-# OVERLAPPABLE #-} Has name pkgs => Has name (Stage2 pkgs)
instance {-# OVERLAPPING #-}  Has "glibc" (Stage2 (Stage1 Seed))

instance Package "glibc" (Stage2 (Stage1 Seed)) where
  type Deps "glibc" (Stage2 (Stage1 Seed)) =
    Has "stdenv" (Stage1 Seed)
  callPackage =
    let mkDrv = callPackage @"stdenv" @(Stage1 Seed)
    in Derivation $ mkDrv MkDerivationArgs { name = "glibc", ... }

-- Final: full package set built with proper tools
type Nixpkgs = Final (Stage2 (Stage1 Seed))
```

The type `Stage2 (Stage1 Seed)` records the full provenance of every
package — which stage built it, all the way back to the seed. GHC
resolves `Has "gcc" (Stage2 (Stage1 Seed))` by falling through
Stage2 (no override) → Stage1 (provides gcc) → done.

### 6.2 The Nixpkgs/Guix Bootstrap Mapping

```
Nixpkgs stage             Derix type
─────────────────────── ─────────────────────────────
bootstrap-tools           Seed
  (pre-built binaries)
stdenvLinux-boot0         Stage1 Seed
  (gcc built from seed)
stdenvLinux-boot1         Stage2 (Stage1 Seed)
  (glibc rebuilt)
stdenvLinux-boot2         Stage3 (Stage2 (Stage1 Seed))
  (gcc rebuilt with new glibc)
stdenvLinux               Final (Stage3 (Stage2 (Stage1 Seed)))
  (everything rebuilt)
```

Each stage overrides a few packages. Everything else falls through via
`OVERLAPPABLE`. The `prev:` parameter provides access to the previous
stage's tools for building.

### 6.3 Backpack for Stages

Each bootstrap stage becomes a Cabal component:

```cabal
library stage0-seed
  exposed-modules: Pkgs
  -- Provides: bootstrap-gcc, bootstrap-glibc, bootstrap-bash

library stage1
  signatures: BasePkgs
  exposed-modules: Pkgs
  -- Requires: seed tools from BasePkgs
  -- Provides: gcc, stdenv (built from seed)

library stage2
  signatures: BasePkgs
  exposed-modules: Pkgs
  -- Requires: stage1 tools from BasePkgs
  -- Provides: glibc (rebuilt with stage1 gcc)

library final
  build-depends: stage0-seed, stage1, stage2
  mixins:
    stage0-seed  (Pkgs as BasePkgs)   -- seed into stage1
    stage1       (Pkgs as BasePkgs)   -- stage1 into stage2
    stage2       (Pkgs as Pkgs)       -- stage2 is the final set
```

Each stage compiles independently against its `BasePkgs` signature.
The `mixins` wire them into a pipeline. Adding a new stage is adding
a new library + one `mixins` entry.

### 6.4 Guix's Explicit Bootstrap

Guix goes further than Nixpkgs: the seed is a ~357-byte binary
(``hex0``), and the entire toolchain is built from source through
~20 stages. The overlay pattern handles this — it's just more layers
of type nesting. The `OVERLAPPABLE` fallthrough means each stage only
declares what it *changes*; everything else is inherited.

## §7 Open Questions

- **Associated type families and `OVERLAPPABLE`**: GHC may reject
  overlappable instances with associated types. Need to verify this
  works in practice.
- **Backpack + typeclasses**: Can the two approaches coexist? Backpack
  provides the module structure, typeclasses provide the resolution
  within each module.
- **Performance**: Deep overlay stacks mean deep instance chains. Does
  GHC handle this efficiently?
- **Orphan instances**: Overlays that add `Package` instances for names
  defined elsewhere are orphan instances. Backpack may help contain
  these within components.
