# Derix: GHC Encoding Design Space

Historical exploration of how Haskell's type system features map to
package management concepts. The current concrete encoding is in
[`callpackage.md`](callpackage.md). This document records the design
space that was explored to arrive there.

## §1 GHC Encodings

Two approaches to compiling package declarations into GHC typeclasses:

- **Multi-class**: one class per package (`class Zlib`, `class Openssl`).
  Superclass context encodes dependencies. Per-package superclass gives
  propagated dependencies for free. Downside: no uniform `callPackage`.
- **Indexed class**: one class indexed by `Symbol` (`class Package name`).
  Uniform `callPackage` method, but loses per-package superclass.
  This is the direction taken in `callpackage.md`.

## §2 Superclass Context

`class (Zlib pkgs) => Openssl pkgs` makes GHC enforce that any package
set with openssl must also have zlib. This is automatic propagated
dependencies — but requires one class per package (multi-class encoding).

## §3 Default Methods

Default method implementations allow "inheriting" build logic. A base
package class provides a default `build` that derived packages can
override. Maps to build system concepts like `stdenv.mkDerivation`
providing a default configure/build/install pipeline.

## §4 DataKinds

Promoted data types allow type-level package names (`"zlib" :: Symbol`),
version numbers (`'V 3 1 0 :: Version`), and version predicates
(`'AtLeast ('V 3 0 0)`). These appear in `callpackage.md` as `Deps`
constraints.

## §5 Bootstrap

Staged self-reference for breaking circular dependencies. The bootstrap
compiler/stdenv is a pre-built binary that doesn't go through `pkgs`.
Maps to Nix's bootstrap stages where early packages use a fixed stdenv.

## §6 Associated Types

`type Output name :: Type` — each package declares what it produces.
Enables heterogeneous package sets: fetchers return functions, packages
return derivations. Adopted directly in `callpackage.md`.

## §7 Deriving

GHC's deriving mechanisms could auto-generate package boilerplate.
`DerivingVia` for sharing build strategies, `DeriveAnyClass` for
opt-in package set membership.

## §8 Quantified Constraints

`forall a. C a => D (F a)` — properties of entire package sets rather
than individual packages. Could express "every package in this set
satisfies some property" without enumerating them.

## §9 Backpack

GHC's module-level abstraction. A package recipe becomes a Backpack
signature; a package set becomes a module that implements all
signatures. Provides separate compilation but limited to module
boundaries.

## §10 ConstraintKinds

`type Deps name pkgs :: Constraint` — dependencies as first-class
types that can be stored, composed, and inspected. Adopted directly
in `callpackage.md` as the associated constraint family.

## §11 Type Families

Type-level computation for version resolution, dependency propagation,
and constraint simplification. `type family Propagated name pkgs`
appears in `callpackage.md` §11.3.

## §12 GADTs

Type-indexed evidence terms. A `Evidence c` GADT indexed by the
constraint it proves could make resolution proofs inspectable and
composable. Existential version hiding (`SomeVersion`) wraps packages
with unknown versions.

## §13 Open Questions

- **Backtracking**: current design commits to one choice (like GHC);
  backtracking (like Scala implicits or SAT solvers) is more complex
- **Semantic versioning as type change**: major bump = incompatible
  evidence type, minor = extension, patch = same interface
- **Build configuration**: associated types for build-time parameters
- **Proof search strategies**: depth-first (GHC), breadth-first,
  tabled resolution (XSB Prolog)
