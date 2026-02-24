# Derix: A Resolution Calculus for Package Management

## §1 Motivation

Package dependency resolution and typeclass instance resolution are the same
problem viewed from different domains. Both are **proof search in a lazy evidence
environment**: given a set of rules (instance declarations / package definitions)
and a goal (a constraint to satisfy / a package to build), find evidence (a
dictionary / a derivation) witnessing that the goal is achievable.

The structural correspondence:

| Typeclass World | Package World |
|---|---|
| Class declaration (`class Eq a`) | Interface / capability specification |
| Instance declaration (`instance ... => Eq T`) | Package version (build recipe) |
| Superclass constraint (`Eq a =>`) | Dependency |
| Instance resolution (proof search) | Dependency resolution |
| Dictionary (evidence term) | Derivation (build plan) |
| Coherence (one instance per type) | One version per package |
| Overlapping instances | Version conflicts / priority policies |
| The dictionary environment | The package set (nixpkgs) |

The deeper connection: **both the dictionary environment and the package set are
lazy fixed points**. In Nix, `pkgs = fix (self: { ... })` where each attribute
is a thunk resolved on demand. In GHC, dictionaries form a lazy record where
superclass evidence is a thunk forced only when needed. Laziness is not an
optimization — it is what makes the self-referential fixed point computable.

This document defines a **resolution calculus** that makes this correspondence
precise. The calculus operates at the **evidence level**: its "types" are
constraints, its "terms" are evidence, and its central operation is proof search.

### 1.1 Remark: Derivations All the Way Down

The word "derivation" appears independently in three traditions, and its three
senses collapse into one in this calculus:

1. **Nix derivation.** In Nix, a *derivation* is a build recipe — a description
   of how to *derive* a package from its source and dependencies. The function
   `mkDerivation` takes inputs and produces a store path. The word comes from
   "deriving one thing from another."

2. **Logical derivation.** In proof theory, a *derivation* is a proof tree — a
   sequence of rule applications that establishes a conclusion from premises.
   When we resolve `python (>= 3.12)`, we build a derivation tree where each
   node is an application of an inference rule to its sub-derivations.

3. **Haskell `deriving`.** The `deriving` keyword asks the compiler to
   automatically *derive* a typeclass instance — i.e., to perform proof search
   for evidence of the constraint. `deriving Eq` means "find a way to construct
   evidence for `Eq` from the structure of this type."

These are not merely analogous. In our calculus, they are the same object:

> **Resolving a package's dependencies = deriving a derivation = building a proof tree.**

The evidence term `inst(python, 3.12, inst(openssl, 3.1, ...), inst(zlib, 1.3, ...))` is
simultaneously a Nix derivation (a build plan), a logical derivation (a proof tree),
and the output of a `deriving` procedure (automated instance construction). The
triple pun is the correspondence made manifest.


## §2 Syntax

### 2.1 Names and Versions

```
Package names    p, q, r
Versions         v ∈ (ℕ × ℕ × ℕ)     (major.minor.patch, totally ordered)
```

### 2.2 Version Predicates

```haskell
Predicate   π  ::=  (== v)            -- exact version
                  |  (>= v)            -- at least v
                  |  (>= v1, < v2)     -- range
                  |  *                  -- any version
```

Version satisfaction: `v ⊨ π` is defined in the obvious way.

### 2.3 Constraints (the "types" of the calculus)

A constraint is a proposition asserting that certain packages are available.
We use Haskell's typeclass constraint syntax:

```haskell
Constraint  C  ::=  p π               -- "package p at a version satisfying π"
                 |  (C₁, C₂)          -- constraint tuple (conjunction)
                 |  ()                 -- trivially satisfied (unit constraint)
```

Examples, side by side with Haskell:

```haskell
-- Haskell                         -- Derix
Eq a                               zlib (>= 1.0)
(Eq a, Ord a)                      (openssl (>= 3.0), zlib (>= 1.0))
()                                 ()
```

Constraints are the **goals** of proof search. They correspond to typeclass
constraints like `(Eq a, Ord a) =>` in Haskell.

### 2.4 Evidence (the "terms" of the calculus)

Evidence witnesses that a constraint is satisfied:

```haskell
Evidence    e  ::=  inst(p, v, ē)    -- "p@v, built with sub-evidence ē"
                 |  (e₁, e₂)         -- tuple evidence
                 |  ()                -- trivial evidence
```

where `ē = e₁, ..., eₙ` is a sequence of evidence terms.

Evidence terms are **proof-relevant**: they record not just *that* a constraint
is satisfiable, but *how* — which specific version was chosen and what its
dependencies resolved to. This is essential for packages (the build plan matters)
and distinguishes our calculus from proof-irrelevant typeclass systems.

### 2.5 Instance Declarations (the "rules" of the calculus)

An instance declaration introduces a new inference rule:

```haskell
Declaration  d  ::=  instance C => p@v
                  |  instance p@v          -- when C = ()
```

Read: "Package `p` at version `v` is available, given that constraint `C` is
satisfied." The constraint `C` captures the package's dependencies.

This IS Haskell's instance declaration syntax:

```haskell
-- Haskell
instance (Eq a, Show a) => Ord (MyType a) where ...

-- Derix
instance (eq (>= 1.0), show (>= 2.0)) => mytype@1.0
```

The **context** (`C =>`) corresponds to the premises above the inference rule
line. The **head** (`p@v`) corresponds to the conclusion. The declaration as a
whole is a **dependent function** at the evidence level:

```haskell
\(e₁ :: C₁) ... (eₙ :: Cₙ) -> inst(p, v, e₁, ..., eₙ)
```

This is the sense in which the context part "resembles lambda binding" — it binds
evidence variables that flow into the constructed evidence.

### 2.6 Instance Environment

```haskell
Environment   Γ  ::=  []
                    |  d : Γ           -- environment extended with declaration d
```

An environment is a finite collection of instance declarations. It is the analogue
of:
- The set of typeclass instances in scope (GHC)
- The set of all package definitions (nixpkgs before evaluation)

### 2.7 Package Set (Resolved Evidence Record)

```haskell
Package Set   Σ  ::=  { p₁ ↦ θ₁, ..., pₙ ↦ θₙ }
```

where each `θᵢ` is a **thunk state**:

```haskell
Thunk State   θ  ::=  Thunk C           -- unevaluated (suspended resolution of C)
                    |  BlackHole         -- currently being forced
                    |  Value e           -- evaluated (memoized evidence)
```

The package set is the analogue of:
- The `pkgs` attribute set in nixpkgs (a lazy record)
- The dictionary environment in GHC (a lazy record of evidence)


## §3 Resolution Rules

Resolution is the central judgment of the calculus:

```haskell
Γ; Σ ⊢ C ⇝ e
```

Read: "In environment Γ with package set Σ, constraint C resolves to evidence e."

### 3.1 Trivial

```
─────────────────── [TRIV]
Γ; Σ ⊢ () ⇝ ()
```

### 3.2 Conjunction

```
Γ; Σ ⊢ C₁ ⇝ e₁      Γ; Σ ⊢ C₂ ⇝ e₂
──────────────────────────────────────── [CONJ]
Γ; Σ ⊢ (C₁, C₂) ⇝ (e₁, e₂)
```

### 3.3 Instance Resolution (the core rule)

```
(instance C_req => p@v) ∈ Γ
v ⊨ π
select(Γ, p, π) = v                     (coherence: v is the selected version)
Γ; Σ ⊢ C_req ⇝ ē
──────────────────────────────────────── [INST]
Γ; Σ ⊢ p π ⇝ inst(p, v, ē)
```

The rule says: to resolve `p π`, find a declaration for `p` whose version
satisfies `π`, recursively resolve its requirements, and construct evidence.

The `select` function embodies the **coherence policy** — it determines which
version to pick when multiple declarations match. See §7.

### 3.4 Lookup (resolution via the package set)

When resolving within the context of a fixed-point package set, we can look up
already-resolved (or in-progress) evidence:

```
Σ(p) = Value e       (already forced and memoized)
v(e) ⊨ π             (the memoized version satisfies the predicate)
──────────────────────────────────────── [LOOKUP]
Γ; Σ ⊢ p π ⇝ e
```

```
Σ(p) = Thunk C       (not yet forced)
──────────────────────────────────────── [FORCE]
force p in Σ, then apply [LOOKUP] or [INST]
```

```
Σ(p) = BlackHole     (currently being forced)
──────────────────────────────────────── [CYCLE]
ERROR: cyclic dependency on p
```

The interplay between [INST], [LOOKUP], and [FORCE] is where laziness lives.
Resolution can proceed without fully evaluating the package set — it only forces
the thunks it actually needs.


## §4 Operational Semantics: Call-by-Need Resolution

### 4.1 Machine State

The abstract machine state is a triple:

```haskell
(Σ, F, C)
```

- `Σ` — the package set (mutable: thunks get updated to values)
- `F` — the forcing set (set of package names currently being forced)
- `C` — the current goal (constraint being resolved)

### 4.2 Transition Rules

**Force a thunk:**

```
Σ(p) = Thunk C_req      p ∉ F
────────────────────────────────────
(Σ, F, p π)
  ⟶  Σ[p ↦ BlackHole]                     (mark as black hole)
      (Σ[p ↦ BlackHole], F ∪ {p}, C_req)  (resolve the requirement)
      then  Σ[p ↦ Value e]                 (memoize result)
      then  (Σ[p ↦ Value e], F \ {p}, p π) (continue with [LOOKUP])
```

**Black hole detection:**

```
Σ(p) = BlackHole    or    p ∈ F
────────────────────────────────────
(Σ, F, p π)  ⟶  ERROR(cycle, p)
```

**Cache hit:**

```
Σ(p) = Value e      v(e) ⊨ π
────────────────────────────────────
(Σ, F, p π)  ⟶  e               (return memoized evidence)
```

**Conjunction (independent sub-goals):**

```
(Σ, F, (C₁, C₂))
  ⟶  let e₁ = (Σ, F, C₁)
         e₂ = (Σ, F, C₂)            (Σ may be updated by forcing in C₁)
     in  (e₁, e₂)
```

Note: conjunction evaluation is **left-to-right and sequential** in this
presentation. An alternative is to allow parallel forcing of independent
sub-goals (both entries are thunks with no data dependency). The sequential
presentation is simpler and matches GHC's behavior.

### 4.3 The Evaluation Invariant

At all times, the package set `Σ` satisfies:

> For each `p ∈ dom(Σ)`, exactly one of:
> 1. `Σ(p) = Thunk C` — unevaluated (has never been demanded)
> 2. `Σ(p) = BlackHole` — currently being forced (appears in `F`)
> 3. `Σ(p) = Value e` — fully resolved (will never change again)

This is the **monotonicity** property: thunks can only transition
`Thunk → BlackHole → Value`, never backwards. It ensures that the fixed point
is well-defined and that sharing is sound.


## §5 The Package Set as a Lazy Fixed Point

### 5.1 Construction

Given an environment `Γ`, the **initial package set** is:

```haskell
Σ₀ = Map.fromList [ (p, Thunk (constraint p)) | p <- pkgnames Γ ]
```

where `constraint p` determines the requirement for `p` based on the selected
declaration (see §7 for the selection policy).

Every entry is an unevaluated thunk. The package set is "complete" in the
sense that it has an entry for every package name in the environment, but
no resolution has occurred yet.

### 5.2 Demand-Driven Evaluation

To obtain the evidence for package `p`, we force `Σ₀(p)`:

```haskell
force Σ₀ p = (Σ₀, {}, p *) ⟶* e
```

This triggers a chain of forcings — exactly the packages transitively
required by `p` get resolved. Everything else remains as thunks.

**This is the key property that makes the calculus lazy.** The full resolution
of all 80,000+ packages in nixpkgs does not happen. Only the packages you
demand get resolved.

### 5.3 Fixed-Point Reading

The package set `Σ` is a fixed point of the function:

```haskell
f self = Map.fromList [ (p, resolve Γ self (constraint p)) | p <- pkgnames Γ ]
```

Under call-by-need, `Σ = fix f` is computed incrementally:
- Start with `Σ₀ = ⊥` (all thunks)
- Each forcing step computes one entry, potentially triggering others
- By monotonicity (thunks only move forward), this converges
- By the Kleene fixed-point theorem, the result is the least fixed point

The domain-theoretic reading: `f` is a continuous function on the CPO of
partial package sets (ordered by information content, with
`Thunk < BlackHole < Value`). Laziness computes exactly the finite
approximation you need.

### 5.4 Sharing

Because thunks are memoized (`Thunk → Value`, read thereafter), each package
is resolved **at most once**. If both `curl` and `python` depend on `openssl`,
the second one to force `Σ(openssl)` gets the cached result. This is the
call-by-need sharing guarantee.

In typeclass terms: GHC creates one dictionary per instance and shares it
across all call sites. Same mechanism, same reason.


## §6 Overlays

### 6.1 Definition

An **overlay** is a function that, given access to the final (post-overlay)
and previous (pre-overlay) package sets, produces a set of replacement entries:

```haskell
type Overlay = PkgSet -> PkgSet -> PkgSet

-- Nix equivalent:  final: prev: { ... }
```

This matches the Nix overlay signature `final: prev: { ... }`.

### 6.2 Application

Applying overlay `o` to base set `base`:

```haskell
result = let final = Map.union (o final base) base
         in  final
```

where `Map.union` is left-biased (overlay entries shadow base entries).

The critical feature: `final` (the self-reference) sees the **post-overlay**
world. So if the overlay replaces `openssl`, then every package that depends
on `openssl` — even packages NOT mentioned in the overlay — automatically
picks up the new version through the lazy fixed point. No explicit re-wiring
needed.

### 6.3 Overlay Composition

Multiple overlays compose by chaining:

```haskell
applyOverlays []       base = base
applyOverlays (o : os) base =
  let prev  = applyOverlays os base
      final = Map.union (o final prev) prev
  in  final
```

### 6.4 Overlay as Instance Replacement

In typeclass terms, an overlay corresponds to **replacing an instance
declaration** in the environment:

```haskell
-- Before:  instance (zlib (>= 1.0)) => openssl@3.1
-- After:   instance (zlib (>= 1.0)) => openssl@3.2
```

and recomputing the fixed point. Haskell forbids this (instances are global
and irrevocable). Nix embraces it (overlays are the primary extension
mechanism). The calculus supports both — coherence (§7) determines whether
replacement is permitted.

### 6.5 Overlay Propagation: Why Laziness Matters

Consider why overlays compose well in Nix but would be painful in a strict
system. In a strict resolver, replacing `openssl` requires:

1. Identify all transitive dependents of `openssl`
2. Topologically sort them
3. Re-resolve each in order

This is an explicit, eager graph traversal. In our lazy calculus, none of
this is needed. The overlay simply replaces the entry and re-ties the fixed
point. Dependents are re-resolved *automatically* when forced, because they
reference `final ! "openssl"` (the self-reference), which now points to the
new version. The propagation is implicit in the fixed-point semantics.

This is the same reason GHC doesn't need to explicitly propagate dictionary
changes — if a superclass instance changes, all subclass dictionaries that
reference it through the lazy record automatically pick up the new version.

### 6.6 The Two Roles of `prev`

In Nix overlays, `prev` serves two distinct purposes:

1. **Fallback**: Packages not mentioned in the overlay come from `prev`.
   In `Map.union (o final prev) prev`, the union provides this.

2. **Old value access**: The overlay can inspect what it's replacing.
   E.g., `prev.python.overrideAttrs (old: { ... })` modifies the old
   derivation rather than building from scratch.

Our calculus captures (1) via the union. For (2), the overlay function
receives `prev` and can inspect its entries. In typeclass terms, this is
like defining a new instance in terms of the old one — a form of instance
inheritance that Haskell lacks but that is natural here.


## §7 Coherence

### 7.1 The Coherence Property

**Definition.** Resolution is **coherent** if, for every constraint `C`, there
is at most one evidence `e` such that `Γ; Σ ⊢ C ⇝ e`.

Equivalently: the resolved package set `Σ` is uniquely determined by `Γ` and
the selection policy. There is exactly one valid build plan.

### 7.2 Selection Policies

The `select(Γ, p, π)` function from rule [INST] embodies the selection policy.
Different policies correspond to different typeclass resolution strategies:

**Policy 1: Non-overlapping (Haskell default)**

For each package name `p`, at most one declaration exists in `Γ`. Then
`select` is trivial — there is zero or one candidate.

Coherence holds trivially. This is the strictest policy.

**Policy 2: Most-specific (Haskell with `OVERLAPPING`)**

Multiple declarations may exist. `select` picks the one with the most
specific (highest) version satisfying `π`:

```haskell
select Γ p π = maximum [ v | (instance _ => p@v) <- Γ, v ⊨ π ]
```

Coherence holds when this maximum is unique (no ties).

**Policy 3: User-pinned**

An explicit pinning `pin :: PkgName -> Maybe Version` overrides selection:

```haskell
select Γ p π = case pin p of
  Just v | v ⊨ π -> v
  _              -> maximum [ v | (instance _ => p@v) <- Γ, v ⊨ π ]
```

This corresponds to explicit version pinning in lock files.

### 7.3 Non-Overlapping Condition

**Definition.** Environment `Γ` is **non-overlapping** if for each package
name `p` and version predicate `π`, at most one declaration in `Γ` has a
version satisfying `π`.

A sufficient (stronger) condition: for each `p`, at most one declaration
exists in `Γ` regardless of version. This is the Haskell model.

### 7.4 Coherence with Overlays

An overlay may break coherence by introducing a second declaration for the
same package. We restore coherence by requiring that overlays **replace**
(shadow) rather than **add alongside**:

**Invariant.** After applying an overlay, the effective environment `Γ'` has
at most one active declaration per package name (per the selection policy).


## §8 Metatheory

We state the key properties. Full proofs are deferred to the formalization
phase (Lean 4).

### 8.1 Soundness

**Theorem (Soundness).** If `Γ; Σ ⊢ C ⇝ e`, then `e` is well-formed
evidence for `C`: for every sub-term `inst(p, v, ē)` in `e`, we have
`(instance C_req => p@v) ∈ Γ` and `ē` is well-formed evidence for `C_req`.

*Proof sketch.* By induction on the derivation of `Γ; Σ ⊢ C ⇝ e`. Each
rule constructs evidence from sub-evidence by applying a declaration, and the
side conditions ensure version satisfaction. □

### 8.2 Coherence (Determinism)

**Theorem (Coherence).** If `Γ` is non-overlapping, then for all `C`, if
`Γ; Σ ⊢ C ⇝ e₁` and `Γ; Σ ⊢ C ⇝ e₂`, then `e₁ = e₂`.

*Proof sketch.* By induction on `C`. The non-overlapping condition ensures
that rule [INST] has at most one applicable declaration at each step, so
the derivation is forced. □

### 8.3 Termination of Resolution

**Definition.** The **dependency graph** of `Γ` is the directed graph where
`p → q` iff some declaration for `p` in `Γ` has `q π` in its constraint.

**Theorem (Termination).** If the dependency graph of `Γ` is acyclic, then
resolution of any constraint `C` terminates.

*Proof sketch.* The dependency depth (longest path from `p` to a leaf in the
dependency graph) is a well-founded measure. Each recursive call to [INST]
resolves a package of strictly smaller depth. □

Note: acyclicity of the *dependency graph* (declaration level) does not
preclude runtime self-reference through the lazy fixed point. A package can
reference itself in its *build recipe* (e.g., bootstrapping compilers) as
long as the *resolution* of its *constraints* doesn't cycle.

### 8.4 Fixed-Point Existence

**Theorem (Fixed-Point Existence).** If resolution terminates for all
reachable constraints, then the package set `Σ = fix f` exists and is
well-defined (as a value in the Scott domain of partial package sets).

*Proof sketch.* The function `f self = { p ↦ resolve Γ self ... }` is
monotone on the domain of partial records (ordered by definedness). By the
Kleene fixed-point theorem, `fix f = ⊔ₙ fⁿ(⊥)` exists. Under call-by-need
evaluation, each `force` operation computes one step of this ascending chain.
Termination of resolution ensures that forcing any entry reaches a value in
finite steps. □

### 8.5 Overlay Monotonicity

**Theorem (Overlay Stability).** If `o` only replaces declarations with
versions that satisfy all pre-existing constraints on the replaced package,
then `let final = Map.union (o final base) base in final` is well-defined
whenever `base` is.

*Proof sketch.* The replacement preserves the termination conditions
(dependency graph acyclicity is maintained if the new declaration has
dependencies that are a subset of the old). The fixed-point construction
then follows from §8.4. □


## §9 Worked Examples

### 9.1 Basic Resolution with Sharing

**Environment:**

```haskell
Γ = [ instance zlib@1.3
    , instance (zlib (>= 1.0)) => openssl@3.1
    , instance (openssl (>= 3.0), zlib (>= 1.0)) => curl@8.5
    , instance (openssl (>= 3.0), zlib (>= 1.2)) => python@3.12
    ]
```

**Initial package set:**

```haskell
Σ₀ = { zlib    ↦ Thunk ()
      , openssl ↦ Thunk (zlib (>= 1.0))
      , curl    ↦ Thunk (openssl (>= 3.0), zlib (>= 1.0))
      , python  ↦ Thunk (openssl (>= 3.0), zlib (>= 1.2))
      }
```

**Resolving `python`** (trace of the abstract machine):

```
Step 1:  force python
         Σ₀(python) = Thunk (...)  →  mark BlackHole
         Σ₁ = { ..., python ↦ BlackHole }
         Goal: (openssl (>= 3.0), zlib (>= 1.2))

Step 2:  resolve openssl (>= 3.0)
         force openssl
         Σ₁(openssl) = Thunk (...)  →  mark BlackHole
         Σ₂ = { ..., openssl ↦ BlackHole, python ↦ BlackHole }
         Goal: zlib (>= 1.0)

Step 3:  resolve zlib (>= 1.0)
         force zlib
         Σ₂(zlib) = Thunk ()  →  mark BlackHole
         Σ₃ = { zlib ↦ BlackHole, openssl ↦ BlackHole, python ↦ BlackHole }
         Goal: ()
         Result: ()
         Memoize: zlib ↦ Value (inst(zlib, 1.3, ()))
         Σ₄ = { zlib ↦ Value (inst(zlib, 1.3, ())), openssl ↦ BlackHole, ... }

Step 4:  back in openssl resolution
         Evidence for zlib obtained: inst(zlib, 1.3, ())
         Memoize: openssl ↦ Value (inst(openssl, 3.1, inst(zlib, 1.3, ())))
         Σ₅ = { zlib    ↦ Value (inst(zlib, 1.3, ()))
              , openssl ↦ Value (inst(openssl, 3.1, inst(zlib, 1.3, ())))
              , python  ↦ BlackHole }

Step 5:  back in python resolution
         Evidence for openssl obtained.
         Now resolve zlib (>= 1.2):
         force zlib  →  CACHE HIT (already Value ...)  ✓ sharing!
         1.3 ⊨ (>= 1.2)  ✓

Step 6:  Memoize: python ↦ Value (inst(python, 3.12, inst(openssl, ...), inst(zlib, ...)))
         Σ₆ = { zlib    ↦ Value (inst(zlib, 1.3, ()))
              , openssl ↦ Value (inst(openssl, 3.1, ...))
              , curl    ↦ Thunk (...)                      ← never forced!
              , python  ↦ Value (inst(python, 3.12, ...)) }
```

**Key observations:**
1. `zlib` was forced once, shared by both `openssl` and `python` (Step 5).
2. `curl` was **never resolved** — its thunk remains `Thunk`. This is laziness.
3. The resolution order was demand-driven: `python → openssl → zlib`, not
   topologically sorted in advance.

### 9.2 Cycle Detection

**Environment (pathological):**

```haskell
Γ = [ instance (b (>= 1.0)) => a@1.0
    , instance (a (>= 1.0)) => b@1.0
    ]
```

**Resolving `a`:**

```
Step 1:  force a  →  mark BlackHole, goal: b (>= 1.0)
Step 2:  force b  →  mark BlackHole, goal: a (>= 1.0)
Step 3:  force a  →  Σ(a) = BlackHole  →  ERROR(cycle, a)
```

The black-hole mechanism catches the cycle. In Nix, this produces
`error: infinite recursion encountered`. In GHC, `<<loop>>`.

### 9.3 Overlay

Starting from the resolved set in §9.1, apply an overlay that bumps openssl:

```haskell
overlay final prev = Map.fromList
  [ ("openssl", inst(openssl, 3.2, final ! "zlib")) ]
```

The new fixed point:

```haskell
result = let final = Map.union (overlay final Σ₆) Σ₆
         in  final

-- final = { zlib    ↦ inst(zlib, 1.3, ())              (from base)
--         , openssl ↦ inst(openssl, 3.2, final ! "zlib") (from overlay)
--         , curl    ↦ Thunk (...)                         (from base)
--         , python  ↦ Thunk (...)                         (from base, RE-THUNKED)
--         }
```

Note: `python` must be re-thunked because it depends on `openssl`, which changed.
When `python` is next forced, it will pick up `openssl@3.2` through `final ! "openssl"`.
This happens automatically — no explicit re-wiring.

In practice, the re-thunking granularity is a design choice: conservative
(re-thunk everything) vs. precise (only re-thunk transitive dependents of
changed packages). The calculus supports both.


### 9.4 Diamond Dependencies (Coherent)

The "diamond dependency" is the classic package management challenge: two
packages depend on a common dependency, and we need them to agree on the
version.

**Environment:**

```haskell
Γ = [ instance base@4.18
    , instance (base (>= 4.0)) => text@2.0
    , instance (base (>= 4.0), text (>= 1.0)) => parsec@3.1
    , instance (base (>= 4.0), text (>= 2.0)) => aeson@2.2
    , instance (parsec (>= 3.0), aeson (>= 2.0)) => myapp@1.0
    ]
```

The diamond: `myapp → parsec → text` and `myapp → aeson → text`.

**Resolution of `myapp`:**

```
force myapp
  → force parsec
      → force base  →  inst(base, 4.18, ())           [memoize]
      → force text
          → lookup base  →  CACHE HIT                  [sharing]
          →  inst(text, 2.0, inst(base, ...))          [memoize]
      →  inst(parsec, 3.1, inst(base, ...), inst(text, ...))  [memoize]
  → force aeson
      → lookup base  →  CACHE HIT                     [sharing]
      → lookup text  →  CACHE HIT                     [sharing!]
      →  inst(aeson, 2.2, inst(base, ...), inst(text, ...))  [memoize]
  →  inst(myapp, 1.0, inst(parsec, ...), inst(aeson, ...))
```

Both `parsec` and `aeson` get the **same** `text` and `base` evidence, because
the package set has exactly one entry per name and sharing is guaranteed. The
diamond is resolved coherently by construction — no solver needed.

This is the power of the typeclass model: coherence (one instance per type)
is exactly the property that prevents diamond dependency conflicts.

### 9.5 Version Conflict (Resolution Failure)

**Environment:**

```haskell
Γ = [ instance openssl@1.1
    , instance openssl@3.1
    , instance (openssl (>= 3.0)) => curl@8.0
    , instance (openssl (>= 1.0, < 2.0)) => legacy@1.0
    , instance (curl (>= 8.0), legacy (>= 1.0)) => myapp@1.0
    ]
```

Under the most-specific policy (`select` picks the highest matching version):
- `curl` needs `openssl (>= 3.0)` → selects `openssl@3.1`
- `legacy` needs `openssl (>= 1.0, < 2.0)` → selects `openssl@1.1`
- But coherence requires ONE version of `openssl` in `Σ`!

Resolution fails because `select Γ "openssl" *` picks `openssl@3.1` (highest),
but then `legacy`'s constraint `openssl (>= 1.0, < 2.0)` is unsatisfied:
`3.1 ⊭ (>= 1.0, < 2.0)`.

In the typeclass world, this is an **incoherence**: two call sites require
incompatible instances of the same class. In the package world, it's a version
conflict. The calculus detects it statically — no need to discover it at build time.

**Resolution with backtracking** (§15.1 extension): a backtracking resolver
could try `openssl@1.1` instead, but then `curl`'s constraint fails. The
conflict is genuine — no solution exists under single-version coherence.

**Resolution with local scoping** (Scala model, §10.3): allow `curl` and `legacy`
to see different versions of `openssl`. This breaks coherence but resolves the
diamond. This is what `node_modules` duplication does.

### 9.6 Overlay Cascade

Demonstrating how an overlay triggers automatic re-resolution through the
lazy fixed point.

**Base environment** (from §9.1):

```haskell
base = { zlib    ↦ inst(zlib, 1.3, ())
       , openssl ↦ inst(openssl, 3.1, inst(zlib, 1.3, ()))
       , curl    ↦ inst(curl, 8.5, inst(openssl, ...), inst(zlib, ...))
       , python  ↦ inst(python, 3.12, inst(openssl, ...), inst(zlib, ...))
       }
```

(Assume all packages were forced in a prior session.)

**Overlay**: replace `zlib` with a patched version:

```haskell
overlay final prev = Map.fromList
  [ ("zlib", inst(zlib, 1.3.1, ())) ]
```

Because the fixed point is re-tied, `final ! "zlib"` now returns the patched
`zlib@1.3.1`. Every package that transitively depends on `zlib` — `openssl`,
`curl`, `python` — will see the new version when forced. The cascade is
automatic:

```
force python in final
  → resolve openssl (>= 3.0)
      → force openssl in final
          → resolve zlib (>= 1.0)
              → force zlib in final  →  inst(zlib, 1.3.1, ())  [new!]
          →  inst(openssl, 3.1, inst(zlib, 1.3.1, ()))         [rebuilt with new zlib]
  → resolve zlib (>= 1.2)
      → force zlib in final  →  CACHE HIT: inst(zlib, 1.3.1, ())
  →  inst(python, 3.12, inst(openssl, ...), inst(zlib, 1.3.1, ()))
```

One line in the overlay (`"zlib" ↦ ...`) caused `openssl`, `curl`, and `python`
to all pick up the patched zlib. This is the compositionality that the lazy
fixed point provides.


## §10 Relationship to Existing Systems

### 10.1 Haskell (GHC)

GHC's instance resolution is a special case of our calculus with:
- Non-overlapping selection policy (by default)
- No overlays (instances are global and permanent)
- Proof-irrelevant evidence (coherence makes the specific dictionary immaterial)
- Termination ensured by the Paterson conditions (a syntactic restriction on
  instance declarations that implies dependency graph acyclicity)

Our calculus generalizes by adding overlays, proof relevance, and explicit
version predicates.

### 10.2 Nix / nixpkgs

Nix's package evaluation is a special case with:
- `callPackage` as the resolution mechanism (inspects function args, fills from set)
- `fix` + overlays for the lazy fixed point
- No explicit constraint language (dependencies are implicit in the Nix code)
- No static coherence checking (conflicts discovered at evaluation time)

Our calculus adds an explicit constraint language and static coherence analysis
to the Nix model.

### 10.3 Scala Implicits

Scala's implicit resolution allows **local scope** — different call sites may
resolve the same typeclass differently. In our calculus, this corresponds to
parameterizing the environment `Γ` per resolution context:

```haskell
Γ₁; Σ₁ ⊢ json (>= 1.0) ⇝ e₁    -- resolves to json@2.0
Γ₂; Σ₂ ⊢ json (>= 1.0) ⇝ e₂    -- resolves to json@1.5
```

This models the "multiple versions coexist" scenario (like `node_modules`).
Coherence is lost, but flexibility is gained.

### 10.4 Rust Trait Resolution

Rust enforces coherence via **orphan rules**: an impl of trait `T` for type `A`
may only appear in the crate that defines `T` or the crate that defines `A`. In
package terms:

> A declaration `instance C => p@v` may only appear in the package that
> defines the interface `C`'s class, or in package `p` itself.

This prevents "action at a distance" — no third party can change how `p`
satisfies `C`. It is a **locality condition** that could be added to our
calculus as a well-formedness rule on environments.

### 10.5 SAT/SMT-based Solvers

Traditional package solvers (apt, cargo) reduce resolution to SAT/SMT. Our
calculus offers a different perspective:
- SAT solving is **eager** — compute the full solution before building
- Our resolution is **lazy** — resolve on demand, share results
- SAT is proof-irrelevant — it outputs "satisfiable" + a model
- Our resolution is proof-relevant — it outputs the evidence term (build plan)
- SAT handles backtracking naturally (DPLL)
- Our calculus commits eagerly (no backtracking, like Haskell); backtracking
  is a possible extension


## §11 GHC Encodings: Piggybacking the Constraint Solver

The isomorphism is not just theoretical. Derix declarations can be **compiled
into actual GHC typeclass instances**, letting GHC's constraint solver perform
the resolution. If the compilation is faithful, then typeclass resolution
literally IS package resolution.

We present two encodings. Both use this common infrastructure:

```haskell
data Derivation = Drv String String [Derivation]
  deriving Show

mkDrv :: String -> String -> [Derivation] -> Derivation
mkDrv = Drv
```

### 11.1 Encoding A: One Class per Package

Each package becomes its own typeclass, parameterized by a type variable `pkgs`
that represents the package set. Dependencies become constraints on `pkgs`.

```haskell
{-# LANGUAGE AllowAmbiguousTypes #-}

class HasZlib pkgs where
  zlib :: Derivation

class HasOpenssl pkgs where
  openssl :: Derivation

class HasCurl pkgs where
  curl :: Derivation

class HasPython pkgs where
  python :: Derivation
```

Instance declarations translate directly — the Derix context becomes the GHC
constraint, the Derix head becomes the GHC class:

```haskell
-- Derix: instance () => zlib@1.3
instance HasZlib pkgs where
  zlib = mkDrv "zlib" "1.3" []

-- Derix: instance zlib (>= 1.0) => openssl@3.1
instance HasZlib pkgs => HasOpenssl pkgs where
  openssl = mkDrv "openssl" "3.1" [zlib @pkgs]

-- Derix: instance (openssl (>= 3.0), zlib (>= 1.0)) => curl@8.5
instance (HasOpenssl pkgs, HasZlib pkgs) => HasCurl pkgs where
  curl = mkDrv "curl" "8.5" [openssl @pkgs, zlib @pkgs]

-- Derix: instance (openssl (>= 3.0), zlib (>= 1.2)) => python@3.12
instance (HasOpenssl pkgs, HasZlib pkgs) => HasPython pkgs where
  python = mkDrv "python" "3.12" [openssl @pkgs, zlib @pkgs]
```

The package set is a concrete type with no runtime content — it exists purely
for instance selection:

```haskell
data Nixpkgs

main = print (python @Nixpkgs)
```

**How it works.** The `pkgs` type variable IS the self-reference. When the
instance body says `zlib @pkgs`, it means "get zlib from whatever package set
I'm being resolved in" — exactly `self.zlib` in Nix. GHC resolves
`HasPython Nixpkgs` by chasing the constraint chain:

```
HasPython Nixpkgs
  → needs HasOpenssl Nixpkgs, HasZlib Nixpkgs
  → HasOpenssl Nixpkgs needs HasZlib Nixpkgs
  → HasZlib Nixpkgs: found (base instance)
```

**Sharing.** GHC creates one dictionary per (class, type) pair. `HasZlib Nixpkgs`
produces one dictionary, shared by `openssl` and `python`.

**Laziness.** Dictionary fields are thunks. If nobody asks for `curl @Nixpkgs`,
its dictionary is never built.

**Overlays** map to newtype wrappers with instance overrides:

```haskell
-- An overlay that bumps openssl to 3.2
data PkgsV2

-- Override openssl:
instance HasOpenssl PkgsV2 where
  openssl = mkDrv "openssl" "3.2" [zlib @PkgsV2]

-- Forward unchanged packages:
instance HasZlib PkgsV2 where
  zlib = zlib @Nixpkgs
```

Now `python @PkgsV2` automatically picks up `openssl@3.2`. The generic instance
`(HasOpenssl pkgs, HasZlib pkgs) => HasPython pkgs` matches with `pkgs = PkgsV2`,
and `HasOpenssl PkgsV2` returns the overlay's version. The cascade is automatic —
exactly how Nix overlays propagate through the fixed point.

### 11.2 Encoding B: Single Indexed Class

A single typeclass indexed by a type-level package name (`Symbol`):

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, AllowAmbiguousTypes #-}
import GHC.TypeLits (Symbol)

class Resolve (name :: Symbol) where
  resolve :: Derivation

instance Resolve "zlib" where
  resolve = mkDrv "zlib" "1.3" []

instance Resolve "zlib" => Resolve "openssl" where
  resolve = mkDrv "openssl" "3.1" [resolve @"zlib"]

instance (Resolve "openssl", Resolve "zlib") => Resolve "python" where
  resolve = mkDrv "python" "3.12" [resolve @"openssl", resolve @"zlib"]
```

Usage: `resolve @"python"` triggers the full resolution chain.

**Version predicates** fit naturally with an associated type:

```haskell
class Resolve (name :: Symbol) where
  type Ver name :: (Nat, Nat, Nat)
  resolve :: Derivation

instance Resolve "zlib" where
  type Ver "zlib" = '(1, 3, 0)
  resolve = mkDrv "zlib" "1.3" []

instance (Resolve "zlib", CmpVersion (Ver "zlib") '(1,0,0) ~ 'GTE)
  => Resolve "openssl" where
  type Ver "openssl" = '(3, 1, 0)
  resolve = mkDrv "openssl" "3.1" [resolve @"zlib"]
```

where `CmpVersion` is a closed type family performing version comparison at the
type level. GHC checks the version constraint statically — a dependency on
`zlib (>= 2.0)` when only `zlib@1.3` is available becomes a **compile-time type
error**.

**Overlays** are harder in this encoding. You cannot have two instances for
`Resolve "openssl"` — GHC rejects duplicate instances for the same head. Options:

1. **Add an overlay index**: `class Resolve (layer :: Nat) (name :: Symbol)`
   where higher layers shadow lower ones.
2. **Generate fresh modules**: each overlay produces a new Haskell module where
   changed instances are redefined, importing unchanged ones.
3. **Use backpack**: GHC's module-level parameterization can model package set
   abstraction.

None is as clean as Encoding A's newtype-per-overlay approach.

### 11.3 Encoding C: Hybrid with OverloadedLabels

Encoding B uses string literals (`@"zlib"`) which are noisy. Encoding A avoids
them but generates one class per package. The hybrid uses a single indexed class
(like B) but with `OverloadedLabels` for clean syntax:

```haskell
{-# LANGUAGE DataKinds, OverloadedLabels, AllowAmbiguousTypes,
             FlexibleInstances, UndecidableInstances #-}

class Has (name :: Symbol) pkgs where
  dep :: Derivation

-- A single catch-all IsLabel instance — no per-package boilerplate:
newtype Dep pkgs = Dep Derivation

instance Has name pkgs => IsLabel name (Dep pkgs) where
  fromLabel = Dep (dep @name @pkgs)
```

Now instance declarations use `#labels` for dependencies:

```haskell
instance Has "zlib" pkgs where
  dep = mkDrv "zlib" "1.3" []

instance Has "zlib" pkgs => Has "openssl" pkgs where
  dep = mkDrv "openssl" "3.1" [#zlib]

instance (Has "openssl" pkgs, Has "zlib" pkgs) => Has "wget" pkgs where
  dep = mkDrv "wget" "1.21" [#zlib, #openssl]

instance (Has "openssl" pkgs, Has "zlib" pkgs, Has "pcre2" pkgs)
  => Has "wget" pkgs where
  dep = mkDrv "wget" "1.21" [#zlib, #pcre2, #openssl]
```

`#zlib` desugars to `fromLabel @"zlib" :: Dep pkgs`, which calls
`dep @"zlib" @pkgs` through the catch-all `IsLabel` instance. The `pkgs`
type variable is inferred from context — it flows through automatically.

**Fixed point and overlays** work exactly as in Encoding A:

```haskell
data Nixpkgs

-- Base instances resolve against Nixpkgs:
main = print (dep @"wget" @Nixpkgs)

-- Overlay:
data PkgsV2

instance Has "openssl" PkgsV2 where
  dep = mkDrv "openssl" "3.2" [#zlib]    -- #zlib resolves via Has "zlib" PkgsV2

instance Has "zlib" PkgsV2 where
  dep = dep @"zlib" @Nixpkgs              -- forward to base
```

`dep @"wget" @PkgsV2` picks up `openssl@3.2` automatically through the
generic instance — same cascade as Encoding A.

**Version predicates** use an associated type, as in Encoding B:

```haskell
class Has (name :: Symbol) pkgs where
  type Ver name :: (Nat, Nat, Nat)
  dep :: Derivation
```

This encoding combines the best of A and B: one class (uniform), `pkgs`
parameter (self-reference + overlays), labels (no string literals in
dependency lists).

### 11.4 Comparison

| Aspect | A (Multi-class) | B (Indexed) | C (Hybrid) |
|---|---|---|---|
| Classes generated | One per package | One total | One total |
| Dependency syntax | `zlib @pkgs` | `resolve @"zlib"` | `#zlib` |
| Self-reference | `pkgs` variable | Implicit | `pkgs` variable |
| Version predicates | Per-class assoc. type | One assoc. type | One assoc. type |
| Overlays | Newtype + override | Extra machinery | Newtype + override |
| Resembles | nixpkgs | Package registry | nixpkgs + labels |
| GHC extensions | `AllowAmbiguous` | + `DataKinds`, `TypeFamilies` | + `OverloadedLabels` |

**Encoding A** is closest to the Nix model but generates many classes.

**Encoding B** is uniform but has noisy syntax and difficult overlays.

**Encoding C** combines uniform representation (one class) with clean syntax
(`#zlib`) and natural overlays (`pkgs` parameter). It is the recommended
encoding for a Derix-to-GHC compiler.

### 11.4 What GHC Provides for Free

Under either encoding, GHC's runtime and type checker automatically provide:

| Calculus concept | GHC mechanism |
|---|---|
| Proof search | Instance resolution |
| Sharing | One dictionary per (class, type) pair |
| Cycle detection | `<<loop>>` (runtime) or unsolvable constraints (compile time) |
| Laziness | Dictionary thunks forced on demand |
| Coherence checking | Overlapping instance rejection |
| Error reporting | "No instance for ..." = unsatisfied dependency |

What GHC does NOT provide:

- **Backtracking**: GHC commits on first match (no DPLL-style search)
- **Human-readable errors**: "No instance for `HasOpenssl PkgsV2`" needs
  translation to "package openssl not found in overlay"
- **Lock file generation**: the resolved set is implicit in the instance graph
- **Version range solving**: needs type-level encoding (Encoding B) or external tooling


## §12 Superclass Context: Package Interfaces

In the GHC encodings of §11, constraints appear only on **instances** —
each version of a package declares its own dependencies. But Haskell class
declarations can also carry constraints — **superclass contexts**:

```haskell
class Eq a => Ord a where ...
```

This says: `Ord` *structurally requires* `Eq`. Not "this instance happens to
need `Eq`" but "the concept of `Ord` is meaningless without `Eq`." Every
`Ord` instance automatically has `Eq` available; every consumer of `Ord`
automatically gets `Eq`.

Applied to packages, the class declaration becomes a **package interface** —
a specification of what the package provides and what it always requires,
independent of any specific version. This is a level of abstraction that
Nix currently lacks: in nixpkgs, the interface and the build recipe are
conflated in the same `callPackage` expression.

This section explores five uses of superclass context in the Encoding A
(multi-class) style. Encoding C adaptations are noted where relevant.

### 12.1 Automatic Constraint Propagation

A superclass constraint is automatically available to any code that holds the
subclass constraint. In package terms: if `HasOpenssl` has `HasZlib` as a
superclass, then anything depending on openssl automatically gets zlib without
listing it:

```haskell
-- Without superclass (all deps explicit):
class HasZlib pkgs where
  zlib :: Derivation

class HasOpenssl pkgs where
  openssl :: Derivation

instance (HasOpenssl pkgs, HasZlib pkgs) => HasCurl pkgs where
  curl = mkDrv "curl" [#openssl, #zlib]
```

```haskell
-- With superclass (zlib propagated through openssl):
class HasZlib pkgs where
  zlib :: Derivation

class HasZlib pkgs => HasOpenssl pkgs where
  openssl :: Derivation

instance HasOpenssl pkgs => HasCurl pkgs where
  curl = mkDrv "curl" [#openssl, #zlib]
  -- #zlib is available because HasOpenssl implies HasZlib
```

In the second version, `HasCurl` only lists `HasOpenssl` in its constraint,
but `#zlib` works because GHC knows `HasOpenssl pkgs` entails `HasZlib pkgs`.

This is **transitive dependency propagation**: the superclass chain
`HasCurl => HasOpenssl => HasZlib` means that any consumer of `curl` can use
`zlib` without explicitly depending on it. In Nix terms, this corresponds to
`propagatedBuildInputs` — dependencies that are visible to downstream packages.

### 12.2 Package Interface vs. Package Version

The class declaration defines **what a package is**. The instance defines
**how a specific version is built**. Separating these allows:

```haskell
-- The interface: openssl provides a library and headers, and always needs zlib.
class HasZlib pkgs => HasOpenssl pkgs where
  openssl        :: Derivation
  opensslHeaders :: Path
  opensslVersion :: Version

-- Version 3.1:
instance HasOpenssl Nixpkgs where
  openssl = mkDrv "openssl" "3.1" [#zlib, #perl]
  opensslHeaders = "/include/openssl"
  opensslVersion = v 3 1 0

-- Version 3.2 (different build recipe, same interface):
instance HasOpenssl NixpkgsUnstable where
  openssl = mkDrv "openssl" "3.2" [#zlib, #perl, #cmake]
  opensslHeaders = "/include/openssl"
  opensslVersion = v 3 2 0
```

The class enforces that **every version** of openssl provides `openssl`,
`opensslHeaders`, and `opensslVersion`, and that `HasZlib` is always satisfied.
Individual instances may have additional build-time dependencies (`perl`, `cmake`)
that don't appear in the interface.

This is the package-world analogue of **interface vs. implementation**: the class
is the `.h` file, the instance is the `.c` file.

**Multiple methods as multiple outputs.** A Nix derivation can produce multiple
outputs (`out`, `dev`, `lib`, `doc`). In the class encoding, each output is a
class method:

```haskell
class HasZlib pkgs => HasOpenssl pkgs where
  opensslLib  :: Derivation    -- runtime library
  opensslDev  :: Derivation    -- headers + pkg-config
  opensslDoc  :: Derivation    -- man pages
```

Consumers pick which outputs they need: `opensslDev` for compilation,
`opensslLib` for runtime, `opensslDoc` for documentation. Each is a separate
thunk — unused outputs are never built (laziness).

### 12.3 Virtual Packages and Package Groups

A class with only superclasses and no methods is a **virtual package** — it
provides nothing itself but demands that several packages are present:

```haskell
class (HasOpenssl pkgs, HasCurl pkgs, HasWget pkgs)
  => HasNetworkStack pkgs

-- No methods. The constraint IS the package.
-- Any pkgs satisfying all three automatically satisfies the group.

instance HasNetworkStack Nixpkgs
  -- GHC checks that Nixpkgs has HasOpenssl, HasCurl, and HasWget.
  -- If any is missing, compile-time error.
```

This maps to several real-world concepts:

| Virtual package pattern | Example |
|---|---|
| Package group / meta-package | `build-essential` in Debian |
| Feature set | `HasNetworkStack` bundles networking tools |
| Platform constraint | `HasPosix` requires POSIX-compatible packages |
| Test suite dependencies | `HasTestDeps` groups test frameworks |

Virtual packages compose naturally through the superclass hierarchy:

```haskell
class (HasNetworkStack pkgs, HasCompiler pkgs, HasBuildTools pkgs)
  => HasDevEnvironment pkgs
```

`HasDevEnvironment pkgs` in a constraint guarantees the full development
environment is available, without listing dozens of individual packages.

### 12.4 Propagated vs. Non-Propagated Dependencies

In Nix, a package's dependencies fall into categories based on visibility:

| Nix concept | Visibility | Superclass encoding |
|---|---|---|
| `propagatedBuildInputs` | Visible to downstream | Superclass context on class |
| `buildInputs` | Visible at build time only | Constraint on instance |
| `nativeBuildInputs` | Build-time, host-only tools | Constraint on instance |

The class/instance split maps this precisely:

```haskell
-- Superclass: zlib is propagated (consumers of openssl see it in headers/linking)
class HasZlib pkgs => HasOpenssl pkgs where
  openssl :: Derivation

-- Instance: perl and cmake are build-time only (not propagated)
instance (HasPerl pkgs, HasCmake pkgs) => HasOpenssl pkgs where
  openssl = mkDrv "openssl" [#zlib, #perl, #cmake]
```

A downstream package depending on openssl:

```haskell
instance HasOpenssl pkgs => HasCurl pkgs where
  curl = mkDrv "curl" [#openssl, #zlib]
  -- #zlib works: propagated via HasOpenssl's superclass
  -- #perl would NOT work: it's only on the instance, not the class
```

This enforces a real invariant: build-time-only dependencies cannot leak into
downstream packages. The type system prevents the mistake of accidentally using
a build-time tool from a dependency — if it's not in the superclass chain, it's
not in scope.

### 12.5 Feature Flags as Class Hierarchy

A package may come in several configurations. Each configuration is a separate
class with its own superclass requirements:

```haskell
-- Base python — no optional features
class HasZlib pkgs => HasPython pkgs where
  python :: Derivation

-- Python with SSL support
class (HasPython pkgs, HasOpenssl pkgs) => HasPythonSSL pkgs where
  pythonSSL :: Derivation

-- Python with Tk GUI support
class (HasPython pkgs, HasTk pkgs) => HasPythonTk pkgs where
  pythonTk :: Derivation

-- Python with everything
class (HasPythonSSL pkgs, HasPythonTk pkgs) => HasPythonFull pkgs where
  pythonFull :: Derivation
```

The hierarchy makes dependencies explicit:
- `HasPythonSSL` requires `HasOpenssl` — you can't have SSL without openssl.
- `HasPythonFull` requires both SSL and Tk — the full build needs everything.
- A consumer can depend on exactly the configuration it needs.

In Nix, feature flags are typically handled by function arguments
(`python.override { sslSupport = true; }`). The class hierarchy makes these
**statically checked**: if you depend on `HasPythonSSL`, GHC guarantees at
compile time that openssl is available. No runtime "feature not enabled" errors.

This also models **conditional dependencies** — the pattern where a package
needs different deps depending on configuration. Each configuration is a
different path through the class hierarchy.

### 12.6 Encoding C Adaptation

In Encoding C (single `Has` class with labels), superclass context is not
directly available — there is only one class. Instead, the same effects
can be achieved with **constraint synonyms** and **type families**:

```haskell
-- Constraint synonym for "openssl and its propagated deps":
type NeedsOpenssl pkgs = (Has "openssl" pkgs, Has "zlib" pkgs)

-- Or with ConstraintKinds and a type family:
type family Propagated (name :: Symbol) pkgs :: Constraint
type instance Propagated "openssl" pkgs = Has "zlib" pkgs
type instance Propagated "curl" pkgs = (Has "openssl" pkgs, Has "zlib" pkgs)
```

This is less elegant than true superclass propagation — the constraint must
be explicitly expanded at use sites or bundled into type synonyms. It is a
trade-off: Encoding A's multi-class approach gives superclass context naturally,
while Encoding C's single-class approach requires encoding it.

A hybrid is possible: use Encoding C (`Has` class) for the uniform registry,
but introduce additional multi-class interfaces for packages that need
superclass propagation:

```haskell
-- Uniform registry:
class Has (name :: Symbol) pkgs where ...

-- Rich interface for packages that need it:
class Has "zlib" pkgs => OpensslI pkgs where
  opensslLib :: Derivation
  opensslDev :: Derivation
```

### 12.7 Summary: What Superclass Context Buys

| Feature | Without superclass | With superclass |
|---|---|---|
| Transitive deps | Listed explicitly everywhere | Propagated automatically |
| Package interface | Implicit in build recipe | Explicit in class declaration |
| Multiple outputs | Single derivation | Multiple class methods |
| Virtual packages | Not expressible | Superclass-only classes |
| Dep visibility | All deps are equal | Propagated vs. build-only |
| Feature flags | Runtime/override | Static class hierarchy |
| Correctness | Deps can be forgotten | Type checker enforces completeness |

The superclass context turns the type checker into a **dependency auditor**:
if a propagated dependency is missing, it is a compile-time error, not a
runtime build failure.


## §13 Default Methods: Inherited Build Recipes

Haskell typeclasses can provide **default method implementations** in the class
declaration. Instances inherit the default unless they override it:

```haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)    -- default: free for all instances
```

An `Eq` instance only needs to define `(==)` and gets `(/=)` automatically.
Applied to packages, default methods become **default build recipes** — the
class declaration says "here is the standard way to build this package" and
specific package sets override only when they need something different.

### 13.1 Default Build Recipes

The class provides a default derivation. Instances get it for free:

```haskell
class HasZlib pkgs => HasOpenssl pkgs where
  openssl :: Derivation
  openssl = mkDrv "openssl" "3.1" [#zlib]

  opensslHeaders :: Path
  opensslHeaders = "/include/openssl"

  opensslVersion :: Version
  opensslVersion = v 3 1 0
```

A package set that satisfies `HasZlib` gets a working openssl without writing
any instance body:

```haskell
data Nixpkgs
instance HasZlib Nixpkgs where ...
instance HasOpenssl Nixpkgs             -- uses all defaults
```

This is **`callPackage` as a language feature**. In Nix, `callPackage` inspects
a function's arguments and fills them from the package set. Here, the default
method body references superclass methods (`#zlib`), and GHC fills them from
the instance's superclass dictionary. Same mechanism, made explicit.

### 13.2 DefaultSignatures: Conditional Build Dependencies

GHC's `DefaultSignatures` extension allows a default method to have a **more
constrained type** than the class method:

```haskell
{-# LANGUAGE DefaultSignatures #-}

class HasZlib pkgs => HasOpenssl pkgs where
  openssl :: Derivation

  default openssl :: HasPerl pkgs => Derivation
  openssl = mkDrv "openssl" "3.1" [#zlib, #perl]
```

The class says: "openssl requires zlib (always)." The default says: "if you
also have perl, here's a default build that uses it." This creates two tiers:

```haskell
-- Package set WITH perl: gets the default for free
data Nixpkgs
instance HasPerl Nixpkgs where ...
instance HasOpenssl Nixpkgs             -- default kicks in, uses perl

-- Package set WITHOUT perl: must provide explicit build
data MinimalPkgs
instance HasOpenssl MinimalPkgs where
  openssl = mkDrv "openssl-lite" "3.1" [#zlib]   -- no perl, custom build
```

GHC enforces this at compile time. If `MinimalPkgs` lacks `HasPerl` and doesn't
provide an explicit `openssl`, you get a type error — not a runtime build
failure. The compiler tells you exactly which packages need manual builds in
your configuration.

This maps to Nix's concept of **optional dependencies**: perl is used for
openssl's tests and some scripts, but you can build openssl without it if
you provide an alternative recipe. The `DefaultSignatures` encoding makes
this statically checked.

### 13.3 Minimal Complete Definition

Haskell's `{-# MINIMAL #-}` pragma specifies which methods an instance must
define — everything else has usable defaults:

```haskell
class (HasZlib pkgs, HasPerl pkgs) => HasOpenssl pkgs where
  opensslSrc     :: SourceTree       -- no default: must specify source
  opensslVersion :: Version          -- no default: must specify version
  opensslPatches :: [Patch]          -- default: no patches
  opensslPatches = []
  openssl        :: Derivation       -- default: standard build from src
  openssl = standardBuild (opensslSrc @pkgs) (opensslPatches @pkgs)
    [#zlib, #perl]
  {-# MINIMAL opensslSrc, opensslVersion #-}
```

An instance must provide the source tree and version — these are per-version
facts with no sensible default. Everything else (patches, build recipe) can
be inherited. To bump openssl, you only change what actually changes:

```haskell
instance HasOpenssl Nixpkgs where
  opensslSrc = fetchTarball "https://openssl.org/source/openssl-3.1.tar.gz"
  opensslVersion = v 3 1 0
  -- openssl and opensslPatches use defaults

instance HasOpenssl NixpkgsUnstable where
  opensslSrc = fetchTarball "https://openssl.org/source/openssl-3.2.tar.gz"
  opensslVersion = v 3 2 0
  opensslPatches = [cve_2024_1234]    -- override: this version needs a patch
  -- openssl build recipe still uses default
```

This separates **what varies per version** (source, version, patches) from
**what is stable across versions** (the build procedure). The `MINIMAL` pragma
documents this split explicitly.

### 13.4 Package Set Inheritance

Default methods enable a form of **package set inheritance**: define a base
set with all defaults, then derive variants by overriding only what changes.

```haskell
-- Base: standard package set, everything uses defaults
data Nixpkgs
instance HasZlib Nixpkgs where ...
instance HasOpenssl Nixpkgs               -- default build
instance HasCurl Nixpkgs                  -- default build
instance HasPython Nixpkgs                -- default build

-- Variant: security-hardened set, only overrides what changes
data NixpkgsHardened
instance HasZlib NixpkgsHardened where
  zlib = zlib @Nixpkgs                    -- forward: unchanged

instance HasOpenssl NixpkgsHardened where
  openssl = mkDrv "openssl" "3.2"         -- override: bumped version
    [#zlib, #perl]

instance HasCurl NixpkgsHardened           -- default: auto-picks up new openssl
instance HasPython NixpkgsHardened         -- default: auto-picks up new openssl
```

`HasCurl NixpkgsHardened` and `HasPython NixpkgsHardened` use the default
build recipes. Those recipes reference `#openssl`, which resolves to
`HasOpenssl NixpkgsHardened` — the overridden version. The cascade is
automatic, driven by the `pkgs` type variable.

This is a **typed overlay**: instead of Nix's `final: prev: { openssl = ...; }`,
you write a new type with overridden instances. The compiler verifies that all
dependencies are satisfied. Unlike Nix overlays, where a missing dependency is
a runtime `attribute 'foo' missing` error, here it is a compile-time
`No instance for HasFoo NixpkgsHardened`.

### 13.5 Cross-Compilation as Instance Override

The default method is the **native build**. Cross-compilation overrides the
build recipe while preserving the package interface:

```haskell
class HasZlib pkgs => HasOpenssl pkgs where
  openssl :: Derivation
  openssl = nativeBuild "openssl" [#zlib]       -- default: native

  opensslHeaders :: Path
  opensslHeaders = "/include/openssl"            -- same for all targets
```

```haskell
data CrossAarch64
instance HasOpenssl CrossAarch64 where
  openssl = crossBuild Aarch64 "openssl" [#zlib]  -- override: cross-compile
  -- opensslHeaders uses default: headers are target-independent
```

Only the build step changes. The interface (headers path, version, etc.)
remains the same. Consumers of `HasOpenssl pkgs` don't know or care whether
they're using a native or cross-compiled openssl — the type is the same.

This models Nix's `pkgsCross.aarch64-multiplatform` pattern, where
cross-compiled package sets override build logic but preserve the attribute
structure.

### 13.6 Comparison with Nix Patterns

| Default method concept | Nix equivalent |
|---|---|
| Default build recipe | `callPackage ./pkg.nix {}` (standard recipe) |
| Instance override | `.override { ... }` / `.overrideAttrs` |
| `DefaultSignatures` | Optional deps (`configureFlags = lib.optional ...`) |
| `{-# MINIMAL #-}` | Which args `callPackage` requires vs. has defaults for |
| Package set inheritance | Overlays (`final: prev: { ... }`) |
| Cross-compilation override | `pkgsCross` attribute sets |

The key difference: in Nix, all of these are dynamic (checked at evaluation
time). With default methods, they are **static** (checked at compile time).
A missing dependency, an incomplete override, or a broken cross-compilation
recipe is caught by the type checker before any package is built.

### 13.7 Encoding C Adaptation

In Encoding C (single `Has` class), default methods cannot vary per package —
there is only one class with one set of methods. However, the same effect can
be achieved by layering a **default instance** atop the base class:

```haskell
-- Base class: no default
class Has (name :: Symbol) pkgs where
  dep :: Derivation

-- Default recipes as a separate class:
class HasDefault (name :: Symbol) pkgs where
  defaultDep :: Derivation

-- Fallback: if HasDefault exists, use it
instance {-# OVERLAPPABLE #-} HasDefault name pkgs => Has name pkgs where
  dep = defaultDep @name @pkgs
```

Package-specific overrides use `{-# OVERLAPPING #-}` to shadow the default.
This works but sacrifices the clean coherence story — overlapping instances
are the Encoding C cost of defaults. Encoding A handles this naturally through
per-class default methods.


## §14 DataKinds: Type-Level Package Management

GHC's `DataKinds` extension promotes data constructors to types and type
constructors to kinds. This lifts package management concepts — names,
versions, predicates, dependency graphs, overlays — into the type level,
where GHC's type checker enforces correctness statically.

Throughout this section, we assume:

```haskell
{-# LANGUAGE DataKinds, TypeFamilies, TypeOperators,
             UndecidableInstances, PolyKinds #-}

import GHC.TypeLits (Symbol, Nat, CmpNat, TypeError, ErrorMessage(..))
```

### 14.1 Type-Level Versions

Versions are promoted triples of `Nat`:

```haskell
-- At the value level:
data Version = V Nat Nat Nat

-- DataKinds promotes this to the kind level:
-- kind Version, with promoted constructor 'V :: Nat -> Nat -> Nat -> Version
```

Type-level version comparison via closed type families:

```haskell
type family CmpVersion (a :: Version) (b :: Version) :: Ordering where
  CmpVersion ('V a1 a2 a3) ('V b1 b2 b3) =
    CmpField (CmpNat a1 b1) a2 a3 b2 b3

type family CmpField (o :: Ordering) a2 a3 b2 b3 :: Ordering where
  CmpField 'LT _ _ _ _ = 'LT
  CmpField 'GT _ _ _ _ = 'GT
  CmpField 'EQ a2 a3 b2 b3 = CmpField2 (CmpNat a2 b2) a3 b3

type family CmpField2 (o :: Ordering) a3 b3 :: Ordering where
  CmpField2 'LT _ _ = 'LT
  CmpField2 'GT _ _ = 'GT
  CmpField2 'EQ a3 b3 = CmpNat a3 b3
```

Now version comparison is computed at compile time:

```haskell
-- These are type-level truths, checked by GHC:
type Check1 = CmpVersion ('V 3 1 0) ('V 1 0 0) ~ 'GT   -- 3.1.0 > 1.0.0
type Check2 = CmpVersion ('V 1 3 0) ('V 1 3 0) ~ 'EQ   -- reflexivity
```

### 14.2 Type-Level Version Predicates

Version predicates become promoted types, with satisfaction checked by a
type family:

```haskell
data VerPred
  = Exact Version
  | AtLeast Version
  | Range Version Version      -- [lo, hi)
  | Any

type family Satisfies (v :: Version) (p :: VerPred) :: Bool where
  Satisfies v ('Exact v)        = 'True
  Satisfies v ('Exact _)        = 'False
  Satisfies v ('AtLeast lo)     = CmpVersion v lo /= 'LT
  Satisfies v ('Range lo hi)    = CmpVersion v lo /= 'LT
                                  && CmpVersion v hi == 'LT
  Satisfies v 'Any              = 'True
```

A constraint that a version satisfies a predicate:

```haskell
type Requires v p = Satisfies v p ~ 'True
```

Used in instance declarations:

```haskell
class Has (name :: Symbol) pkgs where
  type Ver name pkgs :: Version
  dep :: Derivation

instance Has "zlib" pkgs where
  type Ver "zlib" pkgs = 'V 1 3 0
  dep = mkDrv "zlib" "1.3" []

instance ( Has "zlib" pkgs
         , Requires (Ver "zlib" pkgs) ('AtLeast ('V 1 0 0))
         ) => Has "openssl" pkgs where
  type Ver "openssl" pkgs = 'V 3 1 0
  dep = mkDrv "openssl" "3.1" [#zlib]
```

If the package set provides `zlib@0.9`, the constraint `Requires (Ver "zlib"
pkgs) ('AtLeast ('V 1 0 0))` reduces to `'False ~ 'True` — a compile-time
type error. The version predicate is checked before any code runs.

**Custom type errors** make the failure readable:

```haskell
type family Requires (v :: Version) (p :: VerPred) :: Constraint where
  Requires v p =
    If (Satisfies v p)
      (() :: Constraint)
      (TypeError ('Text "Version " ':<>: 'ShowType v
                  ':<>: 'Text " does not satisfy " ':<>: 'ShowType p))
```

GHC reports: `Version 'V 0 9 0 does not satisfy AtLeast ('V 1 0 0)`.

### 14.3 Type-Level Package Set Descriptors

Instead of bare phantom types (`data Nixpkgs`), package sets can carry a
**type-level manifest** — a promoted list describing their contents:

```haskell
data PkgEntry = PkgE Symbol Version

-- The manifest: a type-level list of (name, version) pairs
type NixpkgsManifest =
  '[ 'PkgE "zlib"    ('V 1 3 0)
   , 'PkgE "openssl" ('V 3 1 0)
   , 'PkgE "curl"    ('V 8 5 0)
   , 'PkgE "python"  ('V 3 12 0)
   ]

-- A package set tagged with its manifest:
data PkgSet (manifest :: [PkgEntry])

type Nixpkgs = PkgSet NixpkgsManifest
```

Type-level lookup:

```haskell
type family LookupVer (name :: Symbol) (m :: [PkgEntry]) :: Version where
  LookupVer name ('PkgE name ver ': _)  = ver
  LookupVer name (_             ': rest) = LookupVer name rest
  LookupVer name '[] =
    TypeError ('Text "Package " ':<>: 'ShowType name ':<>: 'Text " not found")
```

This enables **static completeness checking**: if code demands a package not
in the manifest, GHC reports the missing package at compile time. No runtime
"attribute missing" errors.

**Package set comparison** becomes a type-level operation:

```haskell
-- Type-level diff: which packages changed between two manifests?
type family Diff (m1 :: [PkgEntry]) (m2 :: [PkgEntry]) :: [PkgEntry] where
  Diff '[] _ = '[]
  Diff ('PkgE name ver ': rest) m2 =
    If (LookupVer name m2 == ver)
      (Diff rest m2)                                   -- same: skip
      ('PkgE name ver ': Diff rest m2)                 -- changed: include
```

### 14.4 Type-Level Dependency Graphs

The dependency structure can be promoted to a type-level graph:

```haskell
data DepEdge = Edge Symbol Symbol    -- Edge "curl" "openssl" = curl depends on openssl

type ExampleGraph =
  '[ 'Edge "openssl" "zlib"
   , 'Edge "curl" "openssl"
   , 'Edge "curl" "zlib"
   , 'Edge "python" "openssl"
   , 'Edge "python" "zlib"
   ]
```

**Static cycle detection** via type-level DFS:

```haskell
type family HasCycle (graph :: [DepEdge]) :: Bool where
  HasCycle graph = HasCycleFrom graph (AllNodes graph) '[]

type family HasCycleFrom (graph :: [DepEdge]) (nodes :: [Symbol])
                         (visited :: [Symbol]) :: Bool where
  HasCycleFrom _     '[]          _       = 'False
  HasCycleFrom graph (n ': rest) visited =
    DetectFrom graph n '[] visited || HasCycleFrom graph rest visited

type family DetectFrom (graph :: [DepEdge]) (node :: Symbol)
                       (path :: [Symbol]) (visited :: [Symbol]) :: Bool where
  DetectFrom graph node path visited =
    If (Elem node path)
      'True                                              -- cycle!
      (If (Elem node visited)
        'False                                           -- already checked
        (CheckSuccessors graph (Successors graph node) (node ': path) visited))
```

Assert acyclicity as a constraint:

```haskell
type Acyclic graph = HasCycle graph ~ 'False
```

A package environment with a cycle fails at compile time:

```haskell
type CyclicGraph = '[ 'Edge "a" "b", 'Edge "b" "a" ]
-- type NoCycle = Acyclic CyclicGraph
-- GHC error: 'True ~ 'False (could be improved with TypeError)
```

**Transitive dependency computation:**

```haskell
type family TransDeps (name :: Symbol) (graph :: [DepEdge]) :: [Symbol] where
  TransDeps name graph = TransDepsGo graph (Successors graph name) '[] '[]

type family Successors (graph :: [DepEdge]) (name :: Symbol) :: [Symbol] where
  Successors '[] _ = '[]
  Successors ('Edge name dep ': rest) name = dep ': Successors rest name
  Successors (_ ': rest) name = Successors rest name
```

This enables type-level queries like "what are all transitive dependencies of
curl?" — answered at compile time.

### 14.5 Type-Level Overlay Composition

Overlays can be tracked as type-level tags, with GHC ensuring correct
composition:

```haskell
data Overlay = OL Symbol [Symbol]     -- overlay name + packages it modifies

type BumpOpenssl = 'OL "bump-openssl" '["openssl"]
type PatchZlib   = 'OL "patch-zlib"   '["zlib"]

-- Package set parameterized by base manifest and applied overlays:
data PkgSet (base :: [PkgEntry]) (overlays :: [Overlay])
```

**Prevent duplicate overlays:**

```haskell
type family NotApplied (o :: Overlay) (os :: [Overlay]) :: Constraint where
  NotApplied o '[] = ()
  NotApplied ('OL name _) ('OL name _ ': _) =
    TypeError ('Text "Overlay " ':<>: 'ShowType name ':<>: 'Text " already applied")
  NotApplied o (_ ': rest) = NotApplied o rest
```

**Apply an overlay with type-level tracking:**

```haskell
type family ApplyOverlay (o :: Overlay) (m :: [PkgEntry]) :: [PkgEntry]

type instance ApplyOverlay ('OL "bump-openssl" _) m =
  SetVer "openssl" ('V 3 2 0) m

type family SetVer (name :: Symbol) (ver :: Version)
                   (m :: [PkgEntry]) :: [PkgEntry] where
  SetVer name ver ('PkgE name _ ': rest) = 'PkgE name ver ': rest
  SetVer name ver (entry       ': rest)  = entry ': SetVer name ver rest
```

**Track overlay provenance in the type:**

```haskell
type Nixpkgs = PkgSet NixpkgsManifest '[]

type NixpkgsPatched = PkgSet
  (ApplyOverlay BumpOpenssl NixpkgsManifest)
  '[BumpOpenssl]

type NixpkgsFullyPatched = PkgSet
  (ApplyOverlay PatchZlib (ApplyOverlay BumpOpenssl NixpkgsManifest))
  '[PatchZlib, BumpOpenssl]
```

The type of the package set records exactly which overlays have been applied
and in what order. A function requiring a specific overlay can state this in
its constraint:

```haskell
-- This function requires the openssl bump to have been applied:
type family HasOverlay (name :: Symbol) (os :: [Overlay]) :: Constraint where
  HasOverlay name ('OL name _ ': _)  = ()
  HasOverlay name (_ ': rest)        = HasOverlay name rest
  HasOverlay name '[] =
    TypeError ('Text "Required overlay " ':<>: 'ShowType name
              ':<>: 'Text " not applied")

buildSecureServer :: HasOverlay "bump-openssl" overlays
                  => PkgSet manifest overlays -> Derivation
```

### 14.6 Putting It Together

Combining all five features, a fully type-level package declaration looks like:

```haskell
-- The manifest declares what exists and at what versions:
type MyManifest =
  '[ 'PkgE "zlib"    ('V 1 3 0)
   , 'PkgE "openssl" ('V 3 1 0)
   , 'PkgE "curl"    ('V 8 5 0)
   ]

-- The dependency graph declares the structure:
type MyGraph =
  '[ 'Edge "openssl" "zlib"
   , 'Edge "curl" "openssl"
   , 'Edge "curl" "zlib"
   ]

-- The package set, with static guarantees:
type MyPkgs = PkgSet MyManifest '[]

-- At compile time, GHC verifies:
type CheckMyPkgs =
  ( Acyclic MyGraph                                        -- no cycles
  , Requires (LookupVer "zlib" MyManifest) 'Any            -- zlib exists
  , Requires (LookupVer "zlib" MyManifest)                 -- openssl's dep
      ('AtLeast ('V 1 0 0))                                --   is satisfied
  )
```

Every check is performed by the type checker. A malformed package set —
missing dependency, version conflict, cyclic graph, duplicate overlay — is
a **compile-time type error**, not a runtime failure.

### 14.7 Trade-offs

| Benefit | Cost |
|---|---|
| Version predicates checked at compile time | Complex type family machinery |
| Missing packages caught statically | Slow type checking on large sets |
| Cycle detection before evaluation | `UndecidableInstances` required |
| Overlay provenance tracked in types | Verbose type signatures |
| Package set diffing and comparison | GHC error messages can be opaque |

The type-level approach is most valuable for **critical infrastructure** where
catching errors early justifies the complexity. For rapid prototyping or small
package sets, the simpler encodings of §11 may be preferable.

`DataKinds` does not replace the runtime fixed-point construction (§5) — it
adds a **static verification layer** on top. The runtime still uses lazy
thunks for sharing and demand-driven evaluation. The type level ensures that
the runtime will not encounter version mismatches or missing packages.


## §15 Open Questions and Extensions

### 15.1 Backtracking

The current calculus commits to `select`'s choice. If resolution of the
selected declaration's dependencies fails, the whole resolution fails. An
extension could add backtracking (try the next candidate), corresponding to
Scala's implicit search or DPLL's conflict-driven clause learning.

**The design space for backtracking:**

| Strategy | Typeclass analogue | Package analogue | Coherence |
|---|---|---|---|
| No backtracking | GHC default | Nix (one version per attr) | Preserved |
| Backtracking on failure | Scala implicits | apt/cargo solver | Weakened |
| DPLL / CDCL | — | SAT-based solvers | N/A (different model) |
| Tabled resolution | Prolog tabling (XSB) | Memoize failed branches | Preserved if deterministic |

**Backtracking and laziness interact non-trivially.** In the current calculus,
the package set `Σ` is monotone (thunks only move forward: `Thunk → BlackHole → Value`).
With backtracking, a failed resolution might need to *undo* a memoized result and
try a different version. This breaks monotonicity and complicates the fixed-point
semantics.

Possible approaches:
1. **Backtrack before memoization**: only commit to `Σ` after the full
   resolution succeeds. This preserves monotonicity but loses sharing during
   the search phase.
2. **Versioned memoization**: each backtrack point gets a version counter;
   memoized results are invalidated on backtrack. More complex but preserves
   some sharing.
3. **Two-phase resolution**: first phase does backtracking search (without
   memoization) to find a consistent assignment; second phase builds the lazy
   fixed point with the determined versions. This separates "which versions"
   (search) from "build them lazily" (fixed point).

Approach (3) is essentially what SAT-based package managers do, but the second
phase (lazy building) is what our calculus adds beyond them.

### 15.2 Quantified Constraints

Haskell's `QuantifiedConstraints` extension allows:

```haskell
instance (forall a. Eq a => Eq (f a)) => Eq (Compose f g a)
```

In our calculus, this would be:

```haskell
instance (forall π. inner π => wrapper π) => compose@1.0
```

This requires extending constraints with universal quantification, moving us
from Horn clauses to hereditary Harrop formulas. The resolution algorithm
becomes significantly more complex (essentially λProlog).

### 15.3 Semantic Versioning as Type Change

A breaking change (major version bump) corresponds to a change in the
"evidence type" — the package now provides a different interface. A minor
bump adds new evidence while preserving the old. This could be formalized
with structural subtyping on evidence:

```haskell
-- minor bump:   e_old :: C_old   and   e_new :: (C_old, C_extra)
-- patch bump:   same evidence type, different implementation
-- major bump:   C_new is incompatible with C_old
```

### 15.4 Build-Time Configuration as Associated Types

A package parameterized by build configuration (e.g., Python with/without SSL)
corresponds to a typeclass with **associated types**:

```haskell
class Package p where
  type Config p :: *
  build :: Config p -> Derivation
```

In our calculus, this could be modeled by indexing evidence by a configuration
parameter, making constraints richer.

### 15.5 Proof Search Strategies

The current calculus uses depth-first resolution (like GHC). Alternatives:
- Breadth-first (complete but slower)
- Iterative deepening (complete, better space)
- Tabled resolution (like XSB Prolog — memoize failed branches)

The choice of strategy affects termination behavior on non-terminating
inputs but does not affect the set of solvable constraints when resolution
does terminate.
