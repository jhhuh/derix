# Typelevel Package Manager

Research project exploring the correspondence between typeclass instance resolution
and package dependency resolution.

## Core Thesis

Package resolution is proof search. A "resolved install plan" is a dictionary of
evidence witnessing that all constraints are satisfiable. By framing package
management in type-theoretic terms, we can leverage known results about coherence,
decidability, and compositionality from PL theory.

## Project Structure

```
artifacts/          — plans, devlog, skill files
  plan_*.md         — task plans
  devlog.md         — append-only decision journal
  skills/           — reusable patterns discovered during work
src/                — implementation (language TBD)
```

## Status

Early research / design phase.
