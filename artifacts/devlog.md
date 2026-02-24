# Devlog — Typelevel Package Manager

## 2026-02-25 — Project inception

**Idea**: Typeclass instance resolution ≈ package dependency resolution. Both are
constraint-satisfaction / proof-search problems. The project explores this correspondence
to find better designs for package management.

**Key insight**: A "resolved package set" is analogous to a "dictionary of evidence"
that a type checker builds when solving typeclass constraints. If we formalize
packages as instances and dependencies as constraints, we inherit decades of PL
research on coherence, termination, and compositionality.

**Open design axes**:
- Coherence policy: global (Haskell-style, one version per package) vs. local (Scala implicits, multiple versions allowed)
- Proof relevance: does it matter *which* version satisfies a dep, or just that *some* version does?
- Search strategy: depth-first with backtracking? BFS? SAT/SMT reduction?
- Language for constraints: simple version ranges? Type-level predicates? Quantified constraints?

**Status**: Empty repo created. Next: set up Nix flake, write CLAUDE.md, begin prototyping.
