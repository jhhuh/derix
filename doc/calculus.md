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

**Resolution with backtracking** (§12.1 extension): a backtracking resolver
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


## §12 Open Questions and Extensions

### 12.1 Backtracking

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

### 12.2 Quantified Constraints

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

### 12.3 Semantic Versioning as Type Change

A breaking change (major version bump) corresponds to a change in the
"evidence type" — the package now provides a different interface. A minor
bump adds new evidence while preserving the old. This could be formalized
with structural subtyping on evidence:

```haskell
-- minor bump:   e_old :: C_old   and   e_new :: (C_old, C_extra)
-- patch bump:   same evidence type, different implementation
-- major bump:   C_new is incompatible with C_old
```

### 12.4 Build-Time Configuration as Associated Types

A package parameterized by build configuration (e.g., Python with/without SSL)
corresponds to a typeclass with **associated types**:

```haskell
class Package p where
  type Config p :: *
  build :: Config p -> Derivation
```

In our calculus, this could be modeled by indexing evidence by a configuration
parameter, making constraints richer.

### 12.5 Proof Search Strategies

The current calculus uses depth-first resolution (like GHC). Alternatives:
- Breadth-first (complete but slower)
- Iterative deepening (complete, better space)
- Tabled resolution (like XSB Prolog — memoize failed branches)

The choice of strategy affects termination behavior on non-terminating
inputs but does not affect the set of solvable constraints when resolution
does terminate.
