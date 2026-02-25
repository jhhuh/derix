# Derix

**derive + Nix.** Typeclass instance resolution as package dependency resolution.

The name captures the triple pun: Nix **derivation** (build recipe), logical
**derivation** (proof tree), Haskell **deriving** (automatic instance generation) —
all the same object, viewed from the **-ix** (Nix) ecosystem.

## Core Thesis

Package resolution is **proof search in a lazy evidence environment**. A resolved
package set (nixpkgs) and a resolved dictionary environment (GHC) are both **lazy
fixed points** of a self-referential record of evidence thunks.

The design piggybacks on GHC's own typeclass resolution — no separate resolver
implementation needed. GHC IS the package resolver.

## Key Documents

- `doc/encoding.md` — the concrete GHC encoding (primary design doc)
- `doc/calculus.md` — formal resolution calculus
- `doc/design-space.md` — survey of GHC features mapped to package management

## Project Structure

```
doc/                — formal documents
  encoding.md       — the callPackage encoding (primary)
  calculus.md       — formal calculus specification
  design-space.md   — GHC encoding design space survey
artifacts/          — plans, devlog, skill files
  devlog.md         — append-only decision journal
  skills/           — reusable patterns discovered during work
```

## Status

Design documents only. No implementation — GHC's typeclass machinery is the implementation.
