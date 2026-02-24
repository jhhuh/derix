# Typelevel Package Manager

Research project: typeclass instance resolution IS package dependency resolution.

## Core Thesis

Both are **proof search in a lazy evidence environment**. A resolved package set
(nixpkgs) and a resolved dictionary environment (GHC) are both **lazy fixed points**
of a self-referential record of evidence thunks. Laziness is what makes the
self-referential fixed point computable.

The calculus operates at the **evidence level**: constraints are "types", evidence
terms (derivations / dictionaries) are "terms", resolution is proof search.

## Key Document

- `doc/calculus.md` — the formal calculus (syntax, rules, semantics, metatheory)

## Project Structure

```
doc/                — formal documents
  calculus.md       — the core calculus specification
artifacts/          — plans, devlog, skill files
  plan_*.md         — task plans
  devlog.md         — append-only decision journal
  skills/           — reusable patterns discovered during work
src/                — implementation (Haskell prototype, then Lean formalization)
```

## Status

Formal calculus designed (pen-and-paper). Next: Haskell prototype.
