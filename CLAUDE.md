# Derix

**derive + Nix.** Typeclass instance resolution as package dependency resolution.

The name captures the triple pun: Nix **derivation** (build recipe), logical
**derivation** (proof tree), Haskell **deriving** (automatic instance generation) —
all the same object, viewed from the **-ix** (Nix) ecosystem.

## Core Thesis

Package resolution is **proof search in a lazy evidence environment**. A resolved
package set (nixpkgs) and a resolved dictionary environment (GHC) are both **lazy
fixed points** of a self-referential record of evidence thunks.

The calculus operates at the **evidence level**: constraints are "types", evidence
terms (derivations / dictionaries) are "terms", resolution is proof search.

## Key Document

- `doc/calculus.md` — the formal calculus (syntax, rules, semantics, metatheory)

## Project Structure

```
doc/                — formal documents
  calculus.md       — the Derix calculus specification
artifacts/          — plans, devlog, skill files
  devlog.md         — append-only decision journal
  skills/           — reusable patterns discovered during work
src/                — Haskell prototype (Lean formalization later)
  Resolve/          — core modules: Syntax, Resolve, Fix, Overlay, Coherence, Pretty
  Main.hs           — worked examples demonstrating the calculus
```

## Status

Formal calculus designed. Haskell prototype working (builds, runs examples).
