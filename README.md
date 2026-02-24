# Derix

**Managing packages with class.**

*Typeclass instance resolution as package dependency resolution.*

Derix explores a structural correspondence: package resolution and typeclass
resolution are the same problem. Both are proof search in a lazy evidence
environment. A resolved package set (nixpkgs) and a resolved dictionary
environment (GHC) are both lazy fixed points of a self-referential record
of evidence thunks.

The name is a triple pun: Nix **derivation** (build recipe), logical
**derivation** (proof tree), Haskell **deriving** (automatic instance
generation) — all the same object, in the N**ix** ecosystem.

## The Correspondence

```
Typeclass World                    Package World
─────────────────────────────────  ─────────────────────────────────
instance (Eq a) => Ord a           openssl@3.1 depends on zlib≥1.0
Instance resolution (proof search) Dependency resolution
Dictionary (evidence term)         Derivation (build plan)
Coherence (one instance per type)  One version per package
The dictionary environment         The package set (nixpkgs)
```

The key shared property: both environments are defined as a **lazy fixed point**,
and demand-driven evaluation resolves only what is needed.

## What's Here

### Formal Calculus ([`doc/calculus.md`](doc/calculus.md))

A pen-and-paper resolution calculus where packages are instance declarations,
dependencies are constraints, and resolution is proof search. Covers:

- Syntax (constraints, evidence terms, declarations, thunk states)
- Inference rules for resolution
- Call-by-need operational semantics with black-hole cycle detection
- The package set as a lazy fixed point (Kleene's theorem)
- Overlays as environment extension (the Nix `final: prev:` pattern)
- Coherence conditions and selection policies
- Metatheory: soundness, determinism, termination, fixed-point existence

### Haskell Prototype ([`src/`](src/))

A working implementation demonstrating that **Haskell's laziness IS the
calculus's operational semantics**. No `IORef`, no explicit thunk management.
A recursive `let` is the fixed point:

```haskell
buildPkgSet env =
  let pkgSet = Map.fromList
        [ (name, resolveEntry name)
        | name <- allNames env
        ]
      resolveEntry name = ... resolve env pkgSet (deps name) ...
  in pkgSet
```

`pkgSet` references itself — that's the `fix`. GHC's runtime provides sharing
(each package resolved at most once), black-hole detection (cycles produce
`<<loop>>`), and demand-driven evaluation (unneeded packages stay as thunks).

#### Example Output

```
instance () => zlib@1.3.0
instance zlib@≥1.0.0 => openssl@3.1.0
instance (openssl@≥3.0.0, zlib@≥1.2.0) => python@3.12.0

Forcing python:
python@3.12.0
  openssl@3.1.0
    zlib@1.3.0
  zlib@1.3.0
```

The prototype demonstrates:
- **Sharing**: `zlib` resolved once, reused by `openssl` and `python`
- **Laziness**: undemanded packages (`curl`) never resolved
- **Diamond coherence**: `text from parsec == text from aeson: True`
- **Overlay cascade**: bump `openssl` and `python` picks it up automatically
- **Cycle detection**: circular dependencies caught statically

## Building

Requires [Nix](https://nixos.org/) with flakes enabled:

```sh
nix develop -c cabal run derix
```

## Status

**Actively exploring the design space.** Nothing here is settled. The formal
calculus document (`doc/calculus.md`) is a living sketchbook — sections may
contradict each other as we explore different encodings and GHC features.
The Haskell prototype in `src/` demonstrates the core lazy-fixed-point idea
but does not track the document's evolving design.

## License

MIT
