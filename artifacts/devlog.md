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

## 2026-02-25 — Haskell prototype + calculus deepening + naming

**Haskell prototype built** (`src/`). Key insight confirmed: Haskell's own laziness
IS the calculus's operational semantics. No IORef, no explicit thunk management.
A recursive `let` is the fixed point. `Map.Lazy` entries are thunks. GHC's runtime
gives us sharing, black-hole detection, and demand-driven evaluation for free.

Modules: Syntax, Resolve, Fix, Overlay, Coherence, Pretty, Main.
The prototype runs all worked examples from the calculus doc:
- Basic resolution with sharing (zlib shared between openssl and python)
- Diamond dependencies (text shared between parsec and aeson, verified with ==)
- Overlay cascade (bump openssl → python auto-picks up the new version)
- Coherence checking (non-overlapping condition)
- Cycle detection (acyclic dependency graph check)

**Calculus deepened**:
- Added §1.1 "Derivations All the Way Down" — the triple pun (Nix derivation,
  logical derivation, Haskell deriving) is not metaphorical, it's structural.
- Added §6.5-6.6 on overlay propagation and the two roles of `prev`
- Added §9.4-9.6: diamond deps, version conflicts, overlay cascade examples
- Expanded §11.1 on backtracking: design space table, interaction with laziness

**Naming research**. Top candidates grouped by theme:

*Triple pun*: **deriv**, **derivo** (Latin "I derive")
*Evidence/proof*: **evince** ("to make evident" — contains evidence + instance),
  **edict** (puns on "e-dict" = electronic dictionary), **entail**
*Lazy/fixed-point*: **knot** (tying the knot), **thunk**
*Wordplay*: **typix** (type + Nix)

Favorites: **evince** (strongest semantics — the system evinces satisfiability),
**derivo** (captures the triple pun), **knot** (captures the lazy fixed-point insight).
No decision made yet — leaving it open.

## 2026-02-25 — Named "derix", Haskell-like syntax, GHC piggybacking idea

**Named "derix"**: After considering derivix, evince, derivo, entail, settled on
**derix** — short, easy to type, captures derive + Nix.

**Haskell-like syntax**: Rewrote `doc/calculus.md` and the Pretty printer to use
Haskell-like notation throughout:
- `instance C => p@v` instead of `instance p@v given C`
- `(C₁, C₂)` instead of `C₁ ∧ C₂`
- `()` instead of `⊤`
- `Thunk C / BlackHole / Value e` instead of `○(C) / ● / e`

**GHC piggybacking idea**: The user had previously thought about a language system
that is "mere syntax sugar over Haskell" — Derix declarations would compile into
actual GHC typeclass instances, and GHC's constraint solver would perform the
resolution. This is the strongest validation of the thesis: if the two problems are
truly isomorphic, you can literally use one as the other. The compilation would be:

```
-- Derix:   instance zlib@≥1.0 => openssl@3.1.0
-- GHC:     instance (Resolve "zlib", AtLeast (Ver "zlib") '(1,0,0) ~ 'True)
--            => Resolve "openssl" where type Ver "openssl" = '(3,1,0)
```

Tricky parts: version predicates need type-level comparison (type families),
selection policies need OVERLAPPING pragmas or closed type families. But the
basic architecture maps directly. This could be a concrete next milestone.

## 2026-02-25 — The `callPackage` encoding (doc/encoding.md)

**Core encoding designed**: `doc/callpackage.md` (later renamed `doc/encoding.md`).
The encoding piggybacks directly on GHC's typeclass machinery — no separate
resolver needed.

Key design elements:

- `Package (name :: Symbol)` class with associated `Deps`, `Output`, `callPackage`
- Visible `forall pkgs ->` makes the package set a first-class type argument
- `Has name pkgs` class for package set membership witness
- `Deps` is an associated constraint family returning `Constraint`
- `Output` is an associated type family — heterogeneous package sets (fetchers
  return functions, stdenv returns `MkDerivationArgs -> Drv`, packages return
  `Derivation name`)

**Type evolution** (multiple iterations in one session):

1. Started with `Derivation` as a raw struct + `AnyDerivation` existential
2. Added `Drv name` phantom-tagged derivations to prevent accidental swaps
3. Merged phantom tag with existential: `Derivation name = forall a. HasDrv a => Derivation a`
4. **Final naming**: `Drv` = raw `.drv` store derivation (ATerm format, what
   nix-daemon builds). `Derivation name` = existential + phantom-tagged wrapper
   (rich type, carries extra info, compile-time safety)

**stdenv as Package**: `Output "stdenv" = MkDerivationArgs -> Drv`. mkDerivation
is no longer a magic top-level function — it's resolved from `pkgs` like everything
else.

**`Pkg` data family** (§11.5, ergonomic pattern, not core spec):
- Open data family for per-package record types
- `HasPkg` class with `pkgMeta :: forall pkgs -> Has name pkgs => Output name -> Pkg name`
- Takes `callPackage` result as input — captures both input and output info
- RecordWildCards brings `mkDerivation`, `headers`, `version` etc. into scope
- Self-referential `pkgMeta` via recursive `let`
- DuplicateRecordFields for plain field names

## 2026-02-25 — Document cleanup and restructuring

**Minimized docs**: calculus.md (931 → 128 lines), old encoding.md (3731 → 80 lines).
Kept only the essential formal content.

**File renames**:
- `doc/callpackage.md` → `doc/encoding.md` (primary design doc)
- old `doc/encoding.md` → `doc/design-space.md` (GHC feature survey)

**Removed term-level resolver implementation**: Deleted `src/`, `derix.cabal`,
and Haskell build deps from `flake.nix`. GHC's typeclass machinery IS the
resolver — a separate implementation is the wrong approach. The project is
design documents only.

## 2026-03-01 — Overlays and override patterns (doc/overlay.md)

**Overlay encoding designed**: `doc/overlay.md` created. Index `Package` by
both `name :: Symbol` and `pkgs :: Type`. Overlays are phantom types
(`data MyOverlay pkgs`) with `OVERLAPPABLE` inheritance for `Has` and `Package`.
Non-overridden packages fall through to the base; overridden packages get
`OVERLAPPING` instances. Type nesting (`Overlay2 (Overlay1 Nixpkgs)`) = stacking.

**`final:` and `prev:`**: In `Package "x" (MyOverlay pkgs)`, the overlay type
(`MyOverlay pkgs`) is `final:` — deps resolve against it. The wrapped `pkgs` is
`prev:` — the overlay can reach back to the base recipe.

**Bootstrap stages**: Mapped Nixpkgs/Guix bootstrap (Seed → Stage1 → Stage2 →
Final) to overlay stacking. Each stage overrides key packages (gcc, glibc, stdenv),
everything else inherits via `OVERLAPPABLE`. The type records full provenance.

**Backpack for composition**: Overlay composition via Cabal `mixins` with `as`.
Each overlay is a Cabal component with a `requires BasePkgs` signature. Provides
separate compilation — overlays compile against signatures, wiring at link time.

**Override patterns — extensive exploration → convergence**:

Explored 7+ approaches to encoding Nix's `makeOverridable` in Haskell:
explicit wrapper, implicit params, reflection, ReaderT, Backpack, extensible
records (vinyl/row-types), `HasField`, `dependent-map`. User rejected value-level
approaches (DMap) in favor of staying type-level.

**Key insight**: Recipe (the `.nix` file) and `callPackage` (auto-wiring) are
separate concerns. `recipe` is a class method — the build logic. `callPackage`
is derived — resolve deps + apply recipe + seal in existential `Derivation`.

**Existential constraint**: Override must happen BEFORE `callPackage` seals the
result in `Derivation name = forall a. HasDrv a => Derivation a`. Once sealed,
the inner type is unknown and can't be modified.

**Final override design — `Overridable` + `withDict`**:

Since `Overridable` is a single-method class (`func -> func`), its dictionary
IS a function pointer. `withDict` (GHC 9.4) installs arbitrary functions as
dictionaries at runtime. This gives dynamic, composable overrides:

```haskell
class Overridable func where
  mkOverridable :: func -> func    -- default: id (blanket instance)

override :: (func -> func) -> (Overridable func => r) -> (Overridable func => r)
override advice body =
  let composedAdvice = mkOverridable . advice   -- existing wraps new
  in override0 composedAdvice body              -- override0 = withDict
```

Composition order `mkOverridable . advice`: new advice applied first, then
existing override wraps it. Chaining `override a2 $ override a1 $ body` gives
`a2 (a1 recipe)` — outer overrides wrap inner ones. Matches Emacs `:around`
advice semantics.

**Emacs advice analogy**: Override is `advice-add` with `:around`. `:filter-args`
= pre-compose, `:filter-return` = post-compose, `:around` = full control.
`:around` subsumes the others — just higher-order functions.

**Overlay.md rewritten**: Replaced the 570-line §4 (7 exploratory options) with
157-line §4 (single converged design). Sections: Recipe vs callPackage, Overridable
class, withDict magic, composable override, Emacs advice analogy, interaction with
overlays.
