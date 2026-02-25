# Derix: A Resolution Calculus for Package Management

A pen-and-paper resolution calculus where package resolution IS
typeclass instance resolution. The concrete GHC encoding is in
[`encoding.md`](encoding.md). This document provides the formal
foundation.

## §1 The Correspondence

| Typeclass World | Package World |
|---|---|
| Instance declaration (`instance ... => Eq T`) | Package version (build recipe) |
| Superclass constraint (`Eq a =>`) | Dependency |
| Instance resolution (proof search) | Dependency resolution |
| Dictionary (evidence term) | Derivation (build plan) |
| Coherence (one instance per type) | One version per package |
| The dictionary environment | The package set (nixpkgs) |

The deeper connection: both the dictionary environment and the package
set are **lazy fixed points**. Laziness is not an optimization — it is
what makes the self-referential fixed point computable.

The word "derivation" collapses three senses: Nix derivation (build
recipe), logical derivation (proof tree), Haskell deriving (automated
instance construction). They are the same object in this calculus.

## §2 Syntax

```
Package names    p, q, r
Versions         v ∈ (ℕ × ℕ × ℕ)
Predicate   π  ::=  (== v) | (>= v) | (>= v₁, < v₂) | *

Constraint  C  ::=  p π | (C₁, C₂) | ()
Evidence    e  ::=  inst(p, v, ē) | (e₁, e₂) | ()
Declaration d  ::=  instance C => p@v
Environment Γ  ::=  [] | d : Γ
```

The package set is a record of thunks:

```
Package Set  Σ  ::=  { p₁ ↦ θ₁, ..., pₙ ↦ θₙ }
Thunk State  θ  ::=  Thunk C | BlackHole | Value e
```

## §3 Resolution Rules

Judgment: `Γ; Σ ⊢ C ⇝ e` — "constraint C resolves to evidence e."

```
─────────────── [TRIV]        Γ; Σ ⊢ C₁ ⇝ e₁   Γ; Σ ⊢ C₂ ⇝ e₂
Γ; Σ ⊢ () ⇝ ()               ──────────────────────────────────── [CONJ]
                               Γ; Σ ⊢ (C₁, C₂) ⇝ (e₁, e₂)

(instance C_req => p@v) ∈ Γ    v ⊨ π    select(Γ, p, π) = v
Γ; Σ ⊢ C_req ⇝ ē
──────────────────────────────────────── [INST]
Γ; Σ ⊢ p π ⇝ inst(p, v, ē)

Σ(p) = Value e    v(e) ⊨ π              Σ(p) = BlackHole
──────────────────────────── [LOOKUP]    ──────────────────── [CYCLE]
Γ; Σ ⊢ p π ⇝ e                          ERROR: cycle on p
```

## §4 Operational Semantics

Call-by-need with black-hole cycle detection. Machine state: `(Σ, F, C)`
where `F` is the set of names currently being forced.

- **Force**: `Σ(p) = Thunk C_req, p ∉ F` → mark `BlackHole`, resolve
  `C_req`, memoize as `Value e`
- **Cycle**: `Σ(p) = BlackHole` → error
- **Cache hit**: `Σ(p) = Value e, v(e) ⊨ π` → return `e`

Monotonicity: thunks transition `Thunk → BlackHole → Value`, never
backwards. Each package is resolved at most once (sharing).

## §5 The Lazy Fixed Point

```
Σ = fix (λself → { p ↦ resolve Γ self (constraint p) | p ∈ pkgnames Γ })
```

Start with all thunks. Force on demand. By Kleene's theorem, the least
fixed point exists when resolution terminates. Only demanded packages
get resolved — this is what makes 80,000+ package sets tractable.

## §6 Overlays

```
overlay :: PkgSet → PkgSet → PkgSet    -- Nix: final: prev: { ... }

result = let final = Map.union (overlay final base) base in final
```

`final` (self-reference) sees the post-overlay world. Replacing
`openssl` in an overlay automatically propagates to all transitive
dependents through the lazy fixed point — no explicit re-wiring.

## §7 Coherence

Resolution is **coherent** if each constraint has at most one evidence.
The `select` function determines version choice:

- **Non-overlapping** (Haskell default): one declaration per package
- **Most-specific** (with `OVERLAPPING`): highest matching version
- **User-pinned**: explicit lock file overrides

## §8 Metatheory

1. **Soundness**: if `Γ; Σ ⊢ C ⇝ e`, then `e` is well-formed evidence
2. **Coherence**: under non-overlapping, resolution is deterministic
3. **Termination**: acyclic dependency graph implies termination
4. **Fixed-point existence**: termination implies `Σ = fix f` exists
   (Kleene's theorem)

## §9 Relationship to Existing Systems

- **GHC**: non-overlapping, no overlays, proof-irrelevant evidence,
  Paterson conditions for termination
- **Nix**: `callPackage` + `fix` + overlays, no explicit constraint
  language, no static coherence checking
- **Scala**: local implicit scope, allows incoherence (multiple versions
  coexist, like `node_modules`)
- **Rust**: orphan rules as a locality condition on declarations
- **SAT solvers**: eager + proof-irrelevant; our calculus is lazy +
  proof-relevant
