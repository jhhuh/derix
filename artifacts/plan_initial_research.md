# Plan: Initial Research — Typeclass Resolution as Package Resolution

## Core Idea

Package dependency resolution and typeclass instance resolution share deep structural
similarities. Both are essentially *constraint satisfaction / proof search* problems:

| Package Management | Typeclass Resolution |
|-|-|
| Package `A` depends on `B ≥ 2.0` | Instance for `Ord A` requires `Eq A` |
| Version constraints | Type-level constraints / predicates |
| A resolved dependency set | A resolved dictionary / evidence |
| Conflict (two incompatible versions) | Overlapping / incoherent instances |
| Optional dependencies | Default instances / superclass defaults |
| Virtual/provided packages | Type families / associated types |

## Design Space to Explore

1. **Resolution as proof search**
   - Packages ↔ typeclass instances
   - Dependencies ↔ superclass constraints
   - A valid install plan ↔ a well-typed evidence term (dictionary)
   - Backtracking / coherence strategies

2. **Coherence vs. flexibility trade-offs**
   - Haskell's global coherence (at most one instance per type) ↔ "exactly one version of each package"
   - Scala's local implicits ↔ allowing multiple versions (diamond deps)
   - Rust's orphan rules ↔ who may publish a package providing an interface

3. **Overlapping instances / version ranges**
   - `OverlappingInstances` ↔ preference policies (newest, pinned, etc.)
   - Instance priority ↔ solver priority / strategy

4. **Type families / associated types ↔ build configurations**
   - A package may expose different "shapes" depending on config
   - Associated types as build-time type-level configuration

5. **Higher-kinded / quantified constraints ↔ parameterized packages**
   - `∀ a. Eq a => Eq [a]` ↔ "for any version of `base`, this package works"

6. **Proof-relevant vs. proof-irrelevant resolution**
   - Proof-relevant: the *which* instance matters (different code for different versions)
   - Proof-irrelevant: only *existence* matters (any satisfying version is fine)

## Research Questions

- Can we get *better* solver properties (termination, completeness, compositionality)
  by restricting the package description language to something with known decidability?
- Does the typeclass perspective give us a natural notion of "semantic versioning"
  (breaking change = changed instance head)?
- Can we leverage proof assistants (Agda, Lean) to verify resolution correctness?
- What does "instance search depth limit" correspond to in the package world?

## Next Steps

1. Write an initial design document / devlog entry.
2. Set up a Nix flake for the dev environment.
3. Prototype a minimal resolution engine (likely in Haskell or Lean).
4. Formalize the correspondence with small examples.
