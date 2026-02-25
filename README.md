# Derix

**Managing packages with class.**

*Typeclass instance resolution as package dependency resolution.*

Derix explores a structural correspondence: package resolution and typeclass
resolution are the same problem. Both are proof search in a lazy evidence
environment. A resolved package set (nixpkgs) and a resolved dictionary
environment (GHC) are both lazy fixed points of a self-referential record
of evidence thunks.

The name is a triple pun: Nix **derivation** (build recipe), logical
**derivation** (proof tree), Haskell **deriving** (automatic instance
generation) — all the same object, in the N**ix** ecosystem.

## The Encoding

The current design is in [`doc/callpackage.md`](doc/callpackage.md).

`callPackage` is a class method. `pkgs` is a phantom type. Dependencies
are constraints. Instance resolution IS dependency resolution.

```haskell
class Package (name :: Symbol) where
  type Deps name (pkgs :: Type) :: Constraint
  type Output name :: Type
  type Output name = Derivation name
  callPackage :: forall pkgs -> Deps name pkgs => Output name
```

Core types:

- `Drv` — the raw `.drv` store derivation (what the Nix daemon builds)
- `Derivation name` — existential + phantom-tagged wrapper (carries
  extra info, prevents accidental swaps at compile time)
- `Has name pkgs` — package set membership witness
- `HasDrv` — class bridging rich types to raw `Drv`

```
Nix                              Haskell
────────────────────────────────  ────────────────────────────────
{ stdenv, zlib }:                type Deps "openssl" pkgs =
  stdenv.mkDerivation { ... }      (Has "stdenv" pkgs, Has "zlib" pkgs)
callPackage ./openssl.nix {}     callPackage @"openssl" @Nixpkgs
pkgs.stdenv.mkDerivation         callPackage @"stdenv" @Nixpkgs
  (a function in pkgs)             :: MkDerivationArgs -> Drv
missing argument error           No instance for Has "foo" Nixpkgs
  (at eval time)                   (at compile time)
```

## Background

- [`doc/calculus.md`](doc/calculus.md) — formal resolution calculus
  (syntax, inference rules, call-by-need semantics, lazy fixed points,
  overlays, coherence, metatheory)
- [`doc/encoding.md`](doc/encoding.md) — survey of GHC features mapped
  to package management concepts

## Status

**Actively exploring the design space.**

## License

MIT
