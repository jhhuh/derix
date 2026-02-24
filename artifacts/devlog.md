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

## 2026-02-25 — Core calculus: framing shift

**Key realization**: The calculus should be formulated at the **evidence level**,
not the term level. "λ-pkg" was a misleading name — the calculus is not about
lambda terms computing over values. It's about **evidence resolution**: constraints
are the "types", evidence terms (dictionaries / derivations) are the "terms", and
the central operation is proof search.

The user pointed out that instance declarations have the shape `context ⇒ head`,
where the context part resembles lambda binding. This is exactly right: in the
Curry-Howard reading, an instance `(C₁, ..., Cₙ) => C T` is a dependent function
`λ(e₁ : C₁). ... λ(eₙ : Cₙ). evidence`. But this function is at the evidence
level, not the value level.

**Formal calculus written**: `doc/calculus.md` defines the complete calculus:
- Syntax (constraints, evidence, declarations, environments, package sets with thunks)
- Resolution rules as inference rules
- Operational semantics with call-by-need (thunk states: ○ → ● → e)
- Lazy fixed-point construction of the package set
- Overlays as environment extension with self-referential fixed points
- Coherence conditions and selection policies
- Metatheory: soundness, coherence, termination, fixed-point existence
- Worked examples showing sharing and cycle detection

**Naming**: Dropped "λ-pkg". The calculus is unnamed for now — let the name
emerge from the formalism. Candidates: Ev (evidence), Res (resolution).

**Implementation plan**: Haskell prototype first (laziness built-in), then Lean 4
formalization for metatheory proofs.

**Relationship to proof theory**: GHC's instance resolution is essentially
SLD-resolution (Prolog-style depth-first search over Horn clauses). Each instance
is a Horn clause, each constraint is a goal. Our calculus makes this explicit and
adds the lazy fixed-point / overlay structure from Nix. Related formalisms include
hereditary Harrop formulas (λProlog), logical frameworks (LF/Twelf), and focused
proof search. The theoretical connections are rich but the implementation can
start from the GHC-style model we understand well.
