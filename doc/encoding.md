# Derix: GHC Encoding Design Space

Exploration of how Haskell's type system features map to package management
concepts. Each section takes a GHC extension or language feature and shows
how it can encode some aspect of package resolution, dependency management,
or package set construction.

This is a design space exploration — sections may present alternative or
even contradictory encodings. See [`calculus.md`](calculus.md) for the
formal resolution calculus that these encodings implement, and
[`callpackage.md`](callpackage.md) for a ground-up redesign centered on
`callPackage` as a first-class class method.

## §1 GHC Encodings: Piggybacking the Constraint Solver

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

### 1.1 Encoding A: One Class per Package

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

### 1.2 Encoding B: Single Indexed Class

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

### 1.3 Encoding C: Hybrid with OverloadedLabels

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

### 1.4 Comparison

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

### 1.4 What GHC Provides for Free

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


## §2 Superclass Context: Package Interfaces

In the GHC encodings of §1, constraints appear only on **instances** —
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

### 2.1 Automatic Constraint Propagation

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

### 2.2 Package Interface vs. Package Version

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

### 2.3 Virtual Packages and Package Groups

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

### 2.4 Propagated vs. Non-Propagated Dependencies

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

### 2.5 Feature Flags as Class Hierarchy

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

### 2.6 Encoding C Adaptation

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

### 2.7 Summary: What Superclass Context Buys

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


## §3 Default Methods: Inherited Build Recipes

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

### 3.1 Default Build Recipes

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

### 3.2 DefaultSignatures: Conditional Build Dependencies

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

### 3.3 Minimal Complete Definition

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

### 3.4 Package Set Inheritance

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

### 3.5 Cross-Compilation as Instance Override

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

### 3.6 Comparison with Nix Patterns

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

### 3.7 Encoding C Adaptation

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


## §4 DataKinds: Type-Level Package Management

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

### 4.1 Type-Level Versions

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

### 4.2 Type-Level Version Predicates

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

### 4.3 Type-Level Package Set Descriptors

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

### 4.4 Type-Level Dependency Graphs

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

### 4.5 Type-Level Overlay Composition

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

### 4.6 Putting It Together

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

### 4.7 Trade-offs

| Benefit | Cost |
|---|---|
| Version predicates checked at compile time | Complex type family machinery |
| Missing packages caught statically | Slow type checking on large sets |
| Cycle detection before evaluation | `UndecidableInstances` required |
| Overlay provenance tracked in types | Verbose type signatures |
| Package set diffing and comparison | GHC error messages can be opaque |

The type-level approach is most valuable for **critical infrastructure** where
catching errors early justifies the complexity. For rapid prototyping or small
package sets, the simpler encodings of §1 may be preferable.

`DataKinds` does not replace the runtime fixed-point construction (calculus §5) — it
adds a **static verification layer** on top. The runtime still uses lazy
thunks for sharing and demand-driven evaluation. The type level ensures that
the runtime will not encounter version mismatches or missing packages.


## §5 Bootstrap: Staged Self-Reference

### 5.1 The Bootstrap Problem

A compiler must be compiled by a compiler. A C library must be linked by a
linker that was itself linked by a C library. This is the **bootstrap problem**:
the tools needed to build the system are part of the system being built.

In nixpkgs, the bootstrap chain for the standard environment looks like:

```
bootstrap-tools    (stage 0: binary seed, downloaded)
  → stdenv-stage1  (stage 1: minimal gcc built by stage 0)
    → stdenv-stage2  (stage 2: glibc rebuilt by stage 1's gcc)
      → stdenv-stage3  (stage 3: gcc rebuilt with stage 2's glibc)
        → stdenv       (stage 4: final, self-hosting)
```

Each stage uses the previous stage's tools to build the next stage's tools.
The final stdenv can build itself — it is a **fixed point** of the bootstrap
function.

Nixpkgs expresses this transparently because of two properties:
1. **Purity**: each stage is a deterministic function of its inputs (the
   previous stage), with no ambient state leaking in.
2. **Laziness**: the bootstrap chain is only traversed when needed. If you
   just want a pre-built `hello` package, you never force the early stages.

Both properties are central to our calculus. This section shows how the
typeclass encoding captures bootstrap naturally.

### 5.2 Bootstrap as Staged Package Sets

Each bootstrap stage is a different `pkgs` type. Cross-stage dependencies
are expressed as instance constraints:

```haskell
-- Stage 0: binary seed (no dependencies — these are pre-built)
data Stage0

instance HasGcc Stage0 where
  gcc = binarySeed "/nix/store/...-bootstrap-gcc"

instance HasGlibc Stage0 where
  glibc = binarySeed "/nix/store/...-bootstrap-glibc"

instance HasBinutils Stage0 where
  binutils = binarySeed "/nix/store/...-bootstrap-binutils"
```

```haskell
-- Stage 1: rebuild gcc using stage 0's tools
data Stage1

instance HasGlibc Stage1 where
  glibc = glibc @Stage0                      -- forward: still using seed glibc

instance HasBinutils Stage1 where
  binutils = binutils @Stage0                -- forward: still using seed binutils

instance HasGcc Stage1 where
  gcc = compile (gcc @Stage0)                -- BUILD gcc using stage 0's gcc
    (Src "gcc" "13.2")
    [glibc @Stage1, binutils @Stage1]
```

```haskell
-- Stage 2: rebuild glibc using stage 1's gcc
data Stage2

instance HasGcc Stage2 where
  gcc = gcc @Stage1                          -- forward: use stage 1's gcc

instance HasBinutils Stage2 where
  binutils = binutils @Stage0                -- forward

instance HasGlibc Stage2 where
  glibc = compile (gcc @Stage1)              -- BUILD glibc using stage 1's gcc
    (Src "glibc" "2.38")
    [binutils @Stage2]
```

```haskell
-- Stage 3: rebuild gcc using stage 2's glibc
data Stage3

instance HasGlibc Stage3 where
  glibc = glibc @Stage2                      -- forward: use rebuilt glibc

instance HasBinutils Stage3 where
  binutils = compile (gcc @Stage2)           -- rebuild binutils too
    (Src "binutils" "2.41")
    [glibc @Stage3]

instance HasGcc Stage3 where
  gcc = compile (gcc @Stage2)                -- BUILD gcc using rebuilt glibc
    (Src "gcc" "13.2")
    [glibc @Stage3, binutils @Stage3]
```

```haskell
-- Final: the self-hosting package set
data Final

-- Everything forwards to stage 3:
instance HasGcc Final where gcc = gcc @Stage3
instance HasGlibc Final where glibc = glibc @Stage2
instance HasBinutils Final where binutils = binutils @Stage3

-- All other packages are built using the final toolchain:
instance (HasGcc Final, HasGlibc Final) => HasZlib Final where
  zlib = compile (gcc @Final) (Src "zlib" "1.3") [glibc @Final]

instance (HasZlib Final, HasGcc Final) => HasOpenssl Final where
  openssl = compile (gcc @Final) (Src "openssl" "3.1")
    [zlib @Final, glibc @Final]
```

### 5.3 Why This Works: Laziness + Purity

**Laziness.** If you ask for `openssl @Final`, GHC traces:

```
openssl @Final
  → gcc @Final → gcc @Stage3
      → gcc @Stage2
          → gcc @Stage1
              → gcc @Stage0 (binary seed)
          → glibc @Stage2
              → gcc @Stage1 → ... (sharing: already forced)
              → binutils @Stage0 (binary seed)
      → glibc @Stage3 → glibc @Stage2 → ... (sharing)
      → binutils @Stage3 → ...
  → zlib @Final → gcc @Final → ... (sharing: already forced)
  → glibc @Final → glibc @Stage2 → ... (sharing)
```

The full bootstrap chain is traversed — but only because `openssl` needs it.
If you ask for a package that only needs `Final`'s toolchain and all stages
are already forced, you get cache hits immediately. And if you only need
something from Stage 1, Stage 2 and 3 are never computed.

**Purity.** Each stage is a pure function of the previous stage. The type
system makes this explicit: `HasGcc Stage1` can only reference `Stage0`'s
instances. There is no way for Stage 1 to accidentally use Stage 3's gcc —
the types prevent it.

**Sharing.** `gcc @Stage1` is built once and shared by everything in Stage 2
that needs it. The typeclass dictionary mechanism provides this automatically.

### 5.4 The Self-Hosting Fixed Point

The bootstrap terminates when a stage can build itself. In Nix, this means
the final stdenv's gcc, when used to compile gcc from source, produces a
bit-identical binary. In our encoding:

```haskell
-- Self-hosting property (conceptual):
-- compile (gcc @Final) (Src "gcc" "13.2") [...] ≡ gcc @Final
```

This is a **fixed-point equation**: the final gcc IS the result of building
gcc with itself. Our calculus's fixed-point construction (calculus §5) captures exactly
this:

```haskell
Σ = fix (σ → { gcc ↦ compile (σ ! "gcc") (Src "gcc" ...) [...], ... })
```

The difference from normal package resolution is that bootstrap introduces
**multiple stages** of the fixed point. Each stage is a partial fixed point
(self-consistent but using external seeds), and the final stage is the full
fixed point (self-hosting).

### 5.5 Bootstrap in Encoding C

In Encoding C (single `Has` class), stages are encoded as different `pkgs`
types, exactly as above. The `#label` syntax works across stages:

```haskell
instance Has "gcc" Stage0 where
  dep = binarySeed "gcc"

instance Has "gcc" Stage0 => Has "gcc" Stage1 where
  dep = compile (#gcc @Stage0) (Src "gcc" "13.2") [#glibc, #binutils]
```

Note `#gcc` in the dependency list resolves against `Stage1` (the current
`pkgs`), while `#gcc @Stage0` explicitly references the previous stage. The
`pkgs` type variable disambiguates which stage a label resolves in.

### 5.6 With DataKinds: Type-Level Stage Tracking

Stages can be promoted to a type-level natural, making the bootstrap
structure visible in types:

```haskell
data Stage = S Nat | Final

data PkgSet (stage :: Stage)

instance Has "gcc" (PkgSet ('S 0)) where
  dep = binarySeed "gcc"

instance Has "gcc" (PkgSet ('S 0)) => Has "gcc" (PkgSet ('S 1)) where
  dep = compile (dep @"gcc" @(PkgSet ('S 0))) (Src "gcc" "13.2") [#glibc]
```

A type family can enforce stage ordering:

```haskell
type family PrevStage (s :: Stage) :: Stage where
  PrevStage ('S 0)   = TypeError ('Text "Stage 0 has no predecessor")
  PrevStage ('S n)   = 'S (n - 1)
  PrevStage 'Final   = 'S 3        -- or parameterize the final stage number
```

This prevents the impossible: referencing a future stage from an earlier one.

### 5.7 The Correspondence

| Bootstrap concept | Calculus concept | Typeclass encoding |
|---|---|---|
| Binary seed | Axiom (no premises) | Instance with no constraints |
| Build stage | Intermediate fixed point | `pkgs` type per stage |
| "Built by stage N" | Resolution in env N | Constraint on `StageN` |
| Tool forwarding | Evidence reuse | Instance delegates to prior stage |
| Self-hosting | Full fixed point | Final stage = `fix` |
| Stage isolation | Environment scoping | Type prevents cross-stage leaks |
| Bootstrap chain | Proof tree depth | Instance resolution depth |

The bootstrap process is not a special case that needs special machinery.
It is an instance of the general pattern: staged fixed-point construction
with cross-stage evidence references. The calculus handles it because it
was designed around exactly these primitives — laziness, sharing, and
self-referential evidence environments.


## §6 Associated Types: Typed Package Interfaces

Haskell typeclasses support **associated types** — type families declared
inside a class, with each instance providing its own definition:

```haskell
class Container c where
  type Elem c :: *
  empty :: c
  insert :: Elem c -> c -> c

instance Container [a] where
  type Elem [a] = a
  ...
```

The associated type `Elem` varies per instance — it is *determined by* the
class parameter. Applied to packages, this means: the type of a package's
output, its configuration, its platform target, or even its dependency set
can vary per package set. This is a level of polymorphism that Nix's
untyped attribute sets cannot express.

### 6.1 Typed Package Outputs

In Nix, every package output is a store path — an untyped string. A C library
and a Python interpreter have the same type. With associated types, each
package declares *what kind of thing it produces*:

```haskell
class HasZlib pkgs where
  type ZlibOutput pkgs :: *
  zlib :: ZlibOutput pkgs

data CLib = CLib
  { libPath     :: Path
  , headerPath  :: Path
  , pkgConfig   :: Path
  }

data HeaderOnly = HeaderOnly { headerPath :: Path }

instance HasZlib Nixpkgs where
  type ZlibOutput Nixpkgs = CLib               -- full C library
  zlib = CLib "/lib/libz.so" "/include/zlib.h" "/lib/pkgconfig/zlib.pc"

instance HasZlib MinimalPkgs where
  type ZlibOutput MinimalPkgs = HeaderOnly     -- headers only (e.g. WASM target)
  zlib = HeaderOnly "/include/zlib.h"
```

A consumer that needs pkg-config:

```haskell
needsPkgConfig :: (HasZlib pkgs, ZlibOutput pkgs ~ CLib) => ...
```

This fails at compile time for `MinimalPkgs` — you get a type error, not
a runtime "file not found." The associated type makes the package interface
**structurally typed**: you can only use what the package set actually provides.

**Multiple outputs.** Nix packages can have multiple outputs (`out`, `dev`,
`lib`, `doc`). Associated types make each output's type explicit:

```haskell
class HasOpenssl pkgs where
  type OpensslLib pkgs :: *         -- runtime: .so files
  type OpensslDev pkgs :: *         -- development: headers + .a
  type OpensslDoc pkgs :: *         -- documentation: man pages
  opensslLib :: OpensslLib pkgs
  opensslDev :: OpensslDev pkgs
  opensslDoc :: OpensslDoc pkgs
```

Each output can have a different type. A build that only needs headers
constrains `OpensslDev pkgs` but says nothing about `OpensslDoc pkgs` —
documentation is never forced.

### 6.2 Dependency Injection

The classic use of associated types: **abstract over which implementation**
satisfies a capability. The associated type records the choice:

```haskell
class HasTls pkgs where
  type TlsImpl pkgs :: Symbol       -- "openssl", "libressl", or "boringssl"
  tlsLib     :: Derivation
  tlsHeaders :: Path
  tlsVersion :: Version
```

Different package sets plug in different backends:

```haskell
instance HasTls Nixpkgs where
  type TlsImpl Nixpkgs = "openssl"
  tlsLib = openssl @Nixpkgs
  ...

instance HasTls LibrePkgs where
  type TlsImpl LibrePkgs = "libressl"
  tlsLib = libressl @LibrePkgs
  ...
```

Code that works with any TLS backend:

```haskell
buildCurl :: HasTls pkgs => Derivation
buildCurl = mkDrv "curl" [tlsLib @pkgs]
```

Code that requires a specific backend:

```haskell
-- Uses OpenSSL-specific engine API, won't work with LibreSSL:
buildNginxWithEngines :: (HasTls pkgs, TlsImpl pkgs ~ "openssl") => Derivation
```

The second function type-checks against `Nixpkgs` but not `LibrePkgs`. The
dependency injection is verified at compile time.

In Nix, swapping openssl for libressl is done via an overlay, but nothing
prevents code from using openssl-specific features that libressl doesn't have.
The associated type catches this statically.

### 6.3 Platform and Cross-Compilation

The build/host/target triple as associated types of the package set:

```haskell
data Platform = X86_64Linux | Aarch64Linux | Aarch64Darwin | Wasm32 | ...

class HasStdenv pkgs where
  type BuildPlatform  pkgs :: Platform     -- where the build runs
  type HostPlatform   pkgs :: Platform     -- where the result executes
  type TargetPlatform pkgs :: Platform     -- what the result targets (for compilers)
  stdenv :: Derivation
```

Native compilation:

```haskell
instance HasStdenv Nixpkgs where
  type BuildPlatform  Nixpkgs = 'X86_64Linux
  type HostPlatform   Nixpkgs = 'X86_64Linux
  type TargetPlatform Nixpkgs = 'X86_64Linux
```

Cross-compilation to ARM:

```haskell
instance HasStdenv CrossAarch64 where
  type BuildPlatform  CrossAarch64 = 'X86_64Linux
  type HostPlatform   CrossAarch64 = 'Aarch64Linux
  type TargetPlatform CrossAarch64 = 'Aarch64Linux
```

Type-safe platform constraints:

```haskell
-- Only available on Linux targets:
buildSystemd :: (HasStdenv pkgs, HostPlatform pkgs ~ 'X86_64Linux) => Derivation

-- Works on any POSIX host:
type family IsPosix (p :: Platform) :: Bool where
  IsPosix 'X86_64Linux   = 'True
  IsPosix 'Aarch64Linux  = 'True
  IsPosix 'Aarch64Darwin = 'True
  IsPosix 'Wasm32        = 'False

buildCoreutils :: (HasStdenv pkgs, IsPosix (HostPlatform pkgs) ~ 'True)
               => Derivation
```

Attempting to build `coreutils` for WASM is a compile-time error. In Nix,
platform-specific packages are guarded by runtime `assert` or `meta.platforms`
checks. Here, the type system prevents the mistake before evaluation begins.

### 6.4 Version-Dependent Interfaces

The associated type can change based on the package version, modeling **API
evolution**:

```haskell
data ApiLevel = V1Api | V3Api

class HasOpenssl pkgs where
  type OpensslApi pkgs :: ApiLevel
  openssl :: Derivation
```

```haskell
-- Old package set with OpenSSL 1.1:
instance HasOpenssl LegacyPkgs where
  type OpensslApi LegacyPkgs = 'V1Api

-- Current package set with OpenSSL 3.x:
instance HasOpenssl Nixpkgs where
  type OpensslApi Nixpkgs = 'V3Api
```

```haskell
-- Code that needs the v3 API (new OSSL_PROVIDER interface):
useProviderApi :: (HasOpenssl pkgs, OpensslApi pkgs ~ 'V3Api) => ...

-- Code that works with either API level:
useTlsGeneric :: HasOpenssl pkgs => ...
```

This encodes semver at the type level: a major version bump changes the
associated type (breaking the API contract), a minor bump doesn't. Code
that depends on `OpensslApi pkgs ~ 'V3Api` explicitly documents its API
requirement — and the compiler enforces it.

### 6.5 Associated Constraints

With `ConstraintKinds`, the associated type can be a **constraint itself**:

```haskell
{-# LANGUAGE ConstraintKinds #-}

class HasPython pkgs where
  type PythonDeps pkgs :: Constraint
  python :: PythonDeps pkgs => Derivation
```

Different package sets can wire different dependencies:

```haskell
-- Full Python: depends on openssl, zlib, readline, ...
instance HasPython Nixpkgs where
  type PythonDeps Nixpkgs = (HasOpenssl Nixpkgs, HasZlib Nixpkgs,
                             HasReadline Nixpkgs, HasSqlite Nixpkgs)
  python = mkDrv "python" "3.12" [#openssl, #zlib, #readline, #sqlite]

-- Minimal Python: only zlib
instance HasPython MinimalPkgs where
  type PythonDeps MinimalPkgs = HasZlib MinimalPkgs
  python = mkDrv "python-minimal" "3.12" [#zlib]
```

This is strictly more flexible than superclass context (§2): superclass
constraints are fixed per class, but associated constraints vary per
instance. The dependency set is part of the **implementation**, not the
**interface**.

The trade-off: with superclass context, consumers of `HasPython pkgs`
automatically get `HasZlib pkgs`. With associated constraints, they don't —
the deps are opaque. Use superclass context for **always-propagated** deps
(§2.4) and associated constraints for **implementation-specific** deps.

### 6.6 Content-Addressed Outputs

The content hash as an associated type captures **binary reproducibility**
in the type system:

```haskell
class HasZlib pkgs where
  type ZlibHash pkgs :: Symbol
  zlib :: Derivation

instance HasZlib Nixpkgs where
  type ZlibHash Nixpkgs = "sha256-abc123..."
  zlib = mkDrv "zlib" "1.3" []

instance HasZlib NixpkgsReproduced where
  type ZlibHash NixpkgsReproduced = "sha256-abc123..."   -- same hash!
  zlib = mkDrv "zlib" "1.3" []
```

Two package sets producing the same hash are type-level witnesses of
reproducibility:

```haskell
-- Prove two builds are identical:
type Reproducible name pkgs1 pkgs2 =
  ZlibHash pkgs1 ~ ZlibHash pkgs2

-- A function that requires reproducibility:
deployToProduction :: Reproducible "zlib" StagingPkgs ProdPkgs => ...
```

This captures Nix's content-addressed store paths at the type level. A
deployment pipeline can require, as a type constraint, that staging and
production use bit-identical packages. The compiler verifies it.

### 6.7 Build System Abstraction

The build system as an associated type — different package sets can use
different build infrastructure:

```haskell
data BuildSys = MakeStyle | CMakeStyle | MesonStyle | CargoStyle

class HasOpenssl pkgs where
  type OpensslBuildSys pkgs :: BuildSys
  openssl :: Derivation

instance HasOpenssl Nixpkgs where
  type OpensslBuildSys Nixpkgs = 'MakeStyle       -- ./configure && make
  openssl = makeBasedBuild "openssl" ...

instance HasOpenssl ModernPkgs where
  type OpensslBuildSys ModernPkgs = 'CMakeStyle    -- cmake
  openssl = cmakeBasedBuild "openssl" ...
```

Build infrastructure that depends on the build system:

```haskell
-- Generic build wrapper:
class BuildWith (sys :: BuildSys) where
  configure :: Derivation -> Derivation
  compile   :: Derivation -> Derivation
  install   :: Derivation -> Derivation

-- A function that only works with cmake-based packages:
addCMakeFlag :: (HasOpenssl pkgs, OpensslBuildSys pkgs ~ 'CMakeStyle)
             => String -> Derivation
```

### 6.8 Summary

| Associated type use | What varies per `pkgs` | Static guarantee |
|---|---|---|
| Output types (§6.1) | What the package produces | Consumers match the output shape |
| Dependency injection (§6.2) | Which impl is used | Backend-specific code is guarded |
| Platform (§6.3) | Build/host/target triple | Platform-specific code is guarded |
| API level (§6.4) | Version-dependent interface | API requirements documented in types |
| Associated constraints (§6.5) | The dependency set itself | Flexible wiring per package set |
| Content hash (§6.6) | The output identity | Reproducibility as a type-level fact |
| Build system (§6.7) | How the package is built | Build-system-specific code is guarded |

The common thread: associated types let the **package set** determine not just
*which* packages are available (the class instances) but *what shape* each
package has (its output types, API level, platform, dependencies). The package
set becomes a **type-level configuration** that propagates through the entire
build, with the compiler checking consistency at every point.

In Nix, these properties are dynamic: you discover at evaluation time that a
package doesn't support WASM, or that libressl lacks an openssl-specific API.
With associated types, they are static: the type checker catches mismatches
before any package is built.


## §7 Deriving: Automated Package Definitions

Haskell's `deriving` mechanism automatically generates typeclass instances
from the structure of a type. In our encoding, instances ARE package
definitions. So `deriving` becomes **automated package definition generation**
— the compiler writes build recipes for you.

There are four deriving strategies, each mapping to a different pattern of
package automation.

### 7.1 GeneralizedNewtypeDeriving: Zero-Cost Overlays

`GeneralizedNewtypeDeriving` (GND) lets a newtype inherit all instances from
its underlying type. Applied to package sets, this solves the **overlay
forwarding problem**.

Without GND, an overlay must manually forward every unchanged package:

```haskell
data PkgsV2
instance HasZlib PkgsV2    where zlib    = zlib    @Nixpkgs   -- forward
instance HasPerl PkgsV2    where perl    = perl    @Nixpkgs   -- forward
instance HasCurl PkgsV2    where curl    = curl    @Nixpkgs   -- forward
instance HasPython PkgsV2  where python  = python  @Nixpkgs   -- forward
-- ... hundreds more ...
instance HasOpenssl PkgsV2 where
  openssl = mkDrv "openssl" "3.2" [#zlib]                     -- override
```

With GND, one `deriving` clause inherits the entire package set:

```haskell
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

newtype Patched = Patched Nixpkgs
  deriving ( HasZlib, HasPerl, HasCurl, HasPython
           , HasBinutils, HasCmake, HasNinja, HasGit
           -- ... every package in nixpkgs
           )

-- Override only what changes:
instance HasOpenssl Patched where
  openssl = mkDrv "openssl" "3.2" [#zlib]
```

GND works by coercing the dictionary: since `Patched` is representationally
identical to `Nixpkgs`, the `HasZlib Nixpkgs` dictionary can be reused as
`HasZlib Patched` with zero runtime cost. This is exactly what Nix overlays
do — unchanged packages are shared, not rebuilt.

The critical consequence: `#zlib` inside the overridden `openssl` resolves
against `Patched`, finding the inherited `HasZlib` instance (same as
`Nixpkgs`). But `#openssl` in any inherited package that depends on openssl
now finds the `Patched` override. The cascade is automatic.

**Composing overlays** is newtype stacking:

```haskell
newtype SecurityPatched = SecurityPatched Nixpkgs
  deriving (HasZlib, HasPerl, HasCurl, HasPython, ...)

instance HasOpenssl SecurityPatched where
  openssl = mkDrv "openssl" "3.2" [#zlib]        -- security bump

newtype FullyPatched = FullyPatched SecurityPatched
  deriving (HasZlib, HasPerl, HasOpenssl, HasPython, ...)
                                -- ^ inherits the bumped openssl!

instance HasCurl FullyPatched where
  curl = mkDrv "curl" "8.6" [#openssl, #zlib]    -- also bump curl
```

`FullyPatched` has `openssl@3.2` (from `SecurityPatched`) and `curl@8.6`
(overridden). Everything else inherits through the newtype chain. This is
overlay composition — each layer is a newtype.

**Limitation.** GND cannot derive classes with associated types (§6) because
associated type instances cannot be coerced. For classes with associated
types, use `DerivingVia` or `StandaloneDeriving` instead.

### 7.2 DerivingVia: Build Pattern Templates

`DerivingVia` derives an instance by specifying a **representation type**
that already has the instance. In the package world, this is **build pattern
templates** — the equivalent of `buildPythonPackage`, `buildRustPackage`,
`mkDerivation` in Nix.

Define a build pattern as a newtype:

```haskell
{-# LANGUAGE DerivingVia #-}

-- A cmake-based package: give it a name, version, and deps, get a build.
newtype CMakePackage (name :: Symbol) (ver :: Symbol) pkgs =
  CMakePackage pkgs

instance (HasCmake pkgs, HasNinja pkgs, HasGcc pkgs)
  => HasBuild (CMakePackage name ver pkgs) where
  type BuildResult (CMakePackage name ver pkgs) = Derivation
  build = cmakeConfigure name ver >> ninjaCompile >> ninjaInstall
```

Now derive package instances by stating which build pattern to use:

```haskell
-- "openssl is a cmake package"
deriving via (CMakePackage "openssl" "3.1" pkgs)
  instance (HasCmake pkgs, HasNinja pkgs, HasGcc pkgs) => HasOpenssl pkgs

-- "json-c is a cmake package"
deriving via (CMakePackage "json-c" "0.17" pkgs)
  instance (HasCmake pkgs, HasNinja pkgs, HasGcc pkgs) => HasJsonC pkgs
```

Both packages get cmake-based builds without writing any build logic.
The build pattern is defined once and reused.

More build patterns:

```haskell
-- Autotools (./configure && make && make install)
newtype AutotoolsPackage (name :: Symbol) (ver :: Symbol) pkgs =
  AutotoolsPackage pkgs

-- Rust/Cargo
newtype CargoPackage (name :: Symbol) (ver :: Symbol) pkgs =
  CargoPackage pkgs

-- Python (pip/setuptools)
newtype PythonPackage (name :: Symbol) (ver :: Symbol) pkgs =
  PythonPackage pkgs

-- Haskell (cabal)
newtype CabalPackage (name :: Symbol) (ver :: Symbol) pkgs =
  CabalPackage pkgs
```

```haskell
deriving via (AutotoolsPackage "zlib" "1.3" pkgs)
  instance HasGcc pkgs => HasZlib pkgs

deriving via (CargoPackage "ripgrep" "14.1" pkgs)
  instance HasCargo pkgs => HasRipgrep pkgs

deriving via (PythonPackage "requests" "2.31" pkgs)
  instance HasPython pkgs => HasRequests pkgs

deriving via (CabalPackage "pandoc" "3.1" pkgs)
  instance HasGhc pkgs => HasPandoc pkgs
```

Each `deriving via` line is a complete package definition. The build
pattern encapsulates the configure/build/install logic; the `deriving`
line says "this package follows that pattern." This is exactly how Nix's
language-specific builders work — but the pattern matching is verified
by the type checker.

**Customization via associated types.** Build patterns can expose
configuration knobs through associated types:

```haskell
class HasBuild a where
  type BuildResult a :: *
  type BuildFlags a :: [Symbol]
  type BuildFlags a = '[]                  -- default: no extra flags
  build :: BuildResult a
```

```haskell
-- openssl with custom flags:
newtype OpensslCustom pkgs = OpensslCustom pkgs

instance HasBuild (OpensslCustom pkgs) where
  type BuildResult (OpensslCustom pkgs) = Derivation
  type BuildFlags (OpensslCustom pkgs) = '["no-ssl3", "no-weak-ssl-ciphers"]
  build = cmakeBuild (flagsToArgs @(BuildFlags (OpensslCustom pkgs)))

deriving via (OpensslCustom pkgs)
  instance HasOpenssl pkgs
```

### 7.3 DeriveAnyClass: Default-Method Packages

`DeriveAnyClass` generates an empty instance, relying entirely on default
methods (§3). For packages where the class provides a complete default
build recipe, just declare the instance:

```haskell
{-# LANGUAGE DeriveAnyClass #-}

class HasZlib pkgs where
  zlibSrc :: SourceTree
  zlibSrc = fetchUrl "https://zlib.net/zlib-1.3.tar.gz"

  zlib :: Derivation
  zlib = standardBuild (zlibSrc @pkgs) []

-- DeriveAnyClass: empty instance body, all defaults apply
data Nixpkgs
  deriving anyclass (HasZlib)
```

This is the simplest form of package definition — the class author wrote
the build recipe, and the package set just opts in. It corresponds to
packages in Nix where `callPackage ./pkg.nix {}` needs no overrides.

**Bulk package set definition.** Combine `DeriveAnyClass` with multiple
classes:

```haskell
data Nixpkgs
  deriving anyclass
    ( HasZlib, HasOpenssl, HasCurl, HasPython
    , HasGit, HasNinja, HasCmake, HasPerl
    -- ... every package with a complete default build
    )
```

One `data` declaration + one `deriving` clause defines an entire package set.
Packages that need customization get explicit instances; everything else
uses defaults.

### 7.4 StandaloneDeriving: Conditional Package Inclusion

`StandaloneDeriving` separates the instance derivation from the type
definition, enabling conditional package inclusion:

```haskell
{-# LANGUAGE StandaloneDeriving #-}

data Nixpkgs

-- Always available:
deriving anyclass instance HasZlib Nixpkgs
deriving anyclass instance HasOpenssl Nixpkgs

-- Only on Linux:
#ifdef linux_HOST_OS
deriving anyclass instance HasSystemd Nixpkgs
deriving anyclass instance HasInotify Nixpkgs
#endif

-- Only if LLVM backend is enabled:
#ifdef ENABLE_LLVM
deriving via (LLVMBackend Nixpkgs) instance HasGhc Nixpkgs
#else
deriving via (NCGBackend Nixpkgs)  instance HasGhc Nixpkgs
#endif
```

This is compile-time conditional compilation of the package set — different
build configurations produce different sets of available packages, all
checked statically.

### 7.5 Generic: Structure-Driven Build Recipes

GHC's `Generic` class + `DefaultSignatures` enables a pattern where the
build recipe is derived from the **structure** of the package description:

```haskell
{-# LANGUAGE DeriveGeneric, DefaultSignatures #-}

class HasBuild a where
  build :: Derivation
  default build :: (Generic a, GBuildable (Rep a)) => Derivation
  build = genericBuild (from (undefined :: a))
```

A package description is a plain data type:

```haskell
data OpensslPkg = OpensslPkg
  { name    :: "openssl"
  , version :: "3.1"
  , src     :: Url "https://openssl.org/source/openssl-3.1.tar.gz"
  , buildSystem :: CMake
  , deps    :: (Zlib, Perl)
  }
  deriving (Generic, HasBuild)
```

The `Generic` representation tells the build system: "this package has a
name, version, source URL, uses cmake, and depends on zlib and perl." The
`genericBuild` function inspects this structure and produces the derivation
automatically.

This is the type-level analogue of Nix's `mkDerivation` — the function
inspects its argument record and dispatches to the appropriate builder.
But here, the "inspection" happens at compile time through `Generic`, and
type mismatches (e.g., listing a nonexistent dependency) are caught statically.

### 7.6 The Full Picture: Package Set as a Derived Type

Combining all strategies, a complete package set looks like:

```haskell
-- The package set type
newtype Nixpkgs = Nixpkgs ()
  deriving anyclass
    ( HasZlib           -- default build (§7.3)
    , HasReadline       -- default build
    , HasSqlite         -- default build
    )

-- Build-pattern-based packages (§7.2):
deriving via (CMakePackage "openssl" "3.1" Nixpkgs)
  instance HasOpenssl Nixpkgs

deriving via (AutotoolsPackage "curl" "8.5" Nixpkgs)
  instance HasCurl Nixpkgs

deriving via (PythonPackage "requests" "2.31" Nixpkgs)
  instance HasRequests Nixpkgs

-- Hand-written instances for packages that need custom logic:
instance HasGhc Nixpkgs where
  ghc = complexBootstrappedBuild ...

-- An overlay: inherits everything, overrides one package (§7.1):
newtype SecurityPatch = SecurityPatch Nixpkgs
  deriving ( HasZlib, HasReadline, HasSqlite, HasCurl
           , HasRequests, HasGhc, ...
           )

deriving via (CMakePackage "openssl" "3.2" SecurityPatch)
  instance HasOpenssl SecurityPatch   -- bumped
```

Three levels of automation:
1. **`anyclass`** — packages with complete defaults (zero config)
2. **`via`** — packages following a known build pattern (one line)
3. **Hand-written** — packages with complex custom logic (full control)

### 7.7 Comparison with Nix Builders

| Deriving strategy | Nix equivalent |
|---|---|
| `DeriveAnyClass` | `callPackage ./pkg.nix {}` (no overrides) |
| `DerivingVia CMakePackage` | `mkDerivation { buildSystem = cmake; }` |
| `DerivingVia PythonPackage` | `buildPythonPackage { }` |
| `DerivingVia CargoPackage` | `buildRustPackage { }` |
| `GeneralizedNewtypeDeriving` | Overlay forwarding (`final: prev: { }`) |
| `StandaloneDeriving` + CPP | Platform-conditional packages |
| `Generic` + `DefaultSignatures` | `mkDerivation` inspecting `src`, `buildInputs` |

The key difference: in Nix, these patterns are conventions — nothing prevents
`buildPythonPackage` from being called on a C project. With `deriving`, the
build pattern is **type-checked**: `DerivingVia PythonPackage` only works for
types that structurally match what `PythonPackage` expects.


## §8 Quantified Constraints: Properties of Entire Package Sets

Haskell's `QuantifiedConstraints` extension allows universally quantified
constraints in instance contexts and type signatures:

```haskell
{-# LANGUAGE QuantifiedConstraints #-}

instance (forall a. Eq a => Eq (f a)) => Eq (Compose f g a) where ...
```

This says: "if `f` preserves `Eq` for ALL types, then `Compose f g` has `Eq`."
The quantifier ranges over all types — a statement about a property holding
*universally*.

Applied to packages, quantified constraints let us state and enforce
properties about **entire package sets**, **all packages at once**, or
**all possible configurations**. This is the level of abstraction where
package management policy lives.

### 8.1 Package Set Compatibility

"Every package available in A is also available in B" — B is a superset of A:

```haskell
-- Encoding A (multi-class):
type Subset a b =
  ( forall pkgs. HasZlib pkgs    => HasZlib pkgs      -- trivial, but illustrates
  , forall pkgs. HasOpenssl pkgs => HasOpenssl pkgs
  , ...
  )
```

More usefully with Encoding C (single `Has` class):

```haskell
-- "For every package name, if A has it, B has it"
type Superset b a = forall name. Has name a => Has name b
```

This enables **migration safety**: before switching from one package set to
another, assert at compile time that nothing is lost:

```haskell
-- This function only compiles if NixpkgsNew has everything NixpkgsOld had:
migrate :: Superset NixpkgsNew NixpkgsOld => Migration
```

If `NixpkgsNew` dropped a package that `NixpkgsOld` had, the quantified
constraint fails — compile-time error.

### 8.2 Overlay Preservation Guarantees

An overlay changes some packages but should preserve others. Quantified
constraints express this:

```haskell
-- "Applying overlay f to any pkgs preserves all Has instances":
class (forall name pkgs. Has name pkgs => Has name (f pkgs))
  => PreservesAll f

-- "Overlay f preserves package p specifically":
class (forall pkgs. Has p pkgs => Has p (f pkgs))
  => Preserves f p
```

A **safe overlay** type:

```haskell
-- An overlay that guarantees it only modifies the declared packages:
data SafeOverlay (modified :: [Symbol]) (f :: * -> *)

class ( forall name pkgs. (Has name pkgs, NotElem name modified)
          => Has name (f pkgs)
      ) => SafeOverlay modified f
```

If the overlay claims it only modifies `"openssl"` but actually breaks
`"curl"`, the quantified constraint catches it: GHC requires a proof that
`Has "curl" (f pkgs)` follows from `Has "curl" pkgs`, and if `f` broke curl,
no such proof exists.

### 8.3 Package Set Transformers

A **transformer** maps one package set to another, uniformly. Quantified
constraints describe what the transformer preserves or changes.

**Hardening transformer** — applies security flags to all packages:

```haskell
newtype Hardened pkgs = Hardened pkgs

class ( forall name. Has name pkgs => Has name (Hardened pkgs)
      ) => Hardenable pkgs

instance Hardenable pkgs => Has name (Hardened pkgs) where
  dep = applyHardeningFlags (dep @name @pkgs)
```

The quantified constraint guarantees: if the base set has a package, the
hardened set has it too. No packages are lost by hardening.

**Instrumentation transformer** — adds profiling/coverage to all builds:

```haskell
newtype Profiled pkgs = Profiled pkgs

class ( forall name. Has name pkgs => Has name (Profiled pkgs)
      ) => Profilable pkgs
```

**Pinning transformer** — freezes all versions:

```haskell
newtype Pinned (lock :: [(Symbol, Version)]) pkgs = Pinned pkgs

class ( forall name. Has name pkgs => Has name (Pinned lock pkgs)
      , forall name. Has name (Pinned lock pkgs)
          => Ver name (Pinned lock pkgs) ~ LookupLock name lock
      ) => ValidLock lock pkgs
```

The second quantified constraint says: for every package in the pinned set,
its version matches the lock file. This is a lock file validity check as a
type-level proof.

### 8.4 Cross-Compilation Universality

"Every native package has a cross-compiled counterpart":

```haskell
class ( forall name. Has name Native => Has name (Cross target)
      ) => FullyCross target
```

This is a strong guarantee: the cross-compilation setup is complete. If even
one package fails to cross-compile, the quantified constraint fails.

**Partial cross-compilation** — only certain packages cross-compile:

```haskell
class ( forall name. (Has name Native, Crossable name)
          => Has name (Cross target)
      ) => PartiallyCross target
```

The `Crossable name` predicate marks which packages support cross-compilation.
Trying to cross-compile a non-crossable package is a type error.

### 8.5 Reproducibility

"Two package sets produce bit-identical outputs for every package":

```haskell
class ( forall name. (Has name A, Has name B)
          => OutputHash name A ~ OutputHash name B
      ) => Reproducible A B
```

This is the type-level encoding of Nix's reproducibility guarantee. A
deployment pipeline can require:

```haskell
deploy :: Reproducible StagingPkgs ProdPkgs => ProdPkgs -> IO ()
```

The function only compiles if staging and production are proven identical.
The quantified constraint ranges over ALL packages — not just the ones you
remember to check.

### 8.6 Higher-Kinded Package Abstractions

Quantified constraints enable **abstractions over package set operations**:

```haskell
-- A "package set functor" — transforms packages uniformly:
class ( forall name pkgs. Has name pkgs => Has name (f pkgs)
      ) => PkgSetFunctor f

-- Compose two functors:
instance (PkgSetFunctor f, PkgSetFunctor g)
  => PkgSetFunctor (Compose f g)
```

Now you can build pipelines of transformations with guaranteed preservation:

```haskell
type Pipeline = Compose Hardened (Compose Profiled (Pinned MyLockFile))
-- Pipeline is a PkgSetFunctor: it preserves all packages
```

**Package set natural transformations.** A function between two package set
transformers that preserves structure:

```haskell
-- "For any package set, converting from f to g preserves all packages":
type NatTrans f g = forall pkgs. (PkgSetFunctor f, PkgSetFunctor g)
                  => f pkgs -> g pkgs
```

### 8.7 Conditional Entailment

"If a package set satisfies property P, then it also satisfies Q":

```haskell
-- Any package set with a full network stack also has TLS:
class ( forall pkgs. HasNetworkStack pkgs => HasTls pkgs
      ) => NetworkImpliesTls
```

This encodes **package policy rules** — invariants about the relationships
between packages that must hold across all configurations. Policy violations
are type errors.

More examples:

```haskell
-- Security policy: any production set must have hardened openssl:
class ( forall pkgs. ProductionReady pkgs
          => (Has "openssl" pkgs, OpensslFlags pkgs ~ 'Hardened)
      ) => SecurityPolicy

-- License policy: any redistributable set has only free packages:
class ( forall name pkgs. (Has name pkgs, Redistributable pkgs)
          => License name pkgs ~ 'Free
      ) => LicensePolicy
```

### 8.8 The `callPackage` Pattern

Nix's `callPackage` fills function arguments from the package set. With
quantified constraints, we can express this generically:

```haskell
-- A type family listing what a package needs:
type family Deps (name :: Symbol) :: [Symbol]
type instance Deps "openssl" = '["zlib", "perl"]
type instance Deps "curl"    = '["openssl", "zlib"]

-- "All dependencies of name are available in pkgs":
type family AllAvailable (deps :: [Symbol]) pkgs :: Constraint where
  AllAvailable '[]       pkgs = ()
  AllAvailable (d ': ds) pkgs = (Has d pkgs, AllAvailable ds pkgs)

-- callPackage: resolve a package given that all its deps are met:
callPackage :: AllAvailable (Deps name) pkgs => Has name pkgs => Derivation
callPackage = dep @name
```

With quantified constraints, we can state a property about `callPackage`
itself:

```haskell
-- "For any package whose deps are all available, callPackage succeeds":
class ( forall name. AllAvailable (Deps name) pkgs => Has name pkgs
      ) => Complete pkgs
```

`Complete pkgs` means the package set is **self-contained**: every package
that *could* be built (all deps available) *is* built. This is the
fixed-point property (calculus §5) expressed as a quantified constraint.

### 8.9 Summary

| Quantified constraint use | What it expresses |
|---|---|
| Subset/superset (§8.1) | Migration safety between sets |
| Overlay preservation (§8.2) | Overlay doesn't break unmodified packages |
| Transformers (§8.3) | Uniform operations preserve completeness |
| Cross-compilation (§8.4) | All (or specific) packages cross-compile |
| Reproducibility (§8.5) | Bit-identical outputs across sets |
| Higher-kinded (§8.6) | Composable package set pipelines |
| Conditional entailment (§8.7) | Policy rules: security, licensing |
| `callPackage` (§8.8) | Self-containedness of the package set |

The common thread: quantified constraints lift reasoning from **individual
packages** to **entire package sets**. Without them, you can say "this
package set has openssl." With them, you can say "this overlay preserves
every package it doesn't modify" or "these two sets are identical for all
packages" — universal properties that hold by construction.

In Nix, these properties are tested empirically (CI builds all packages,
diffoscope checks reproducibility). With quantified constraints, they are
**proven at compile time**.


## §9 Backpack: Module-Level Package Abstraction

GHC's Backpack system provides **module-level parameterization**: a package
can depend on a module **signature** (interface) without committing to a
specific implementation. The implementation is plugged in at configuration
time through **mixins**. This offers an entirely different encoding of
package management — one that avoids most GHC type-level extensions by
operating at the module level instead.

### 9.1 Signatures as Package Interfaces

A module signature (`.hsig` file) declares what a package provides without
saying how:

```haskell
-- Zlib.hsig
signature Zlib where
  data ZlibLib
  zlib :: Derivation
  zlibVersion :: Version
  zlibHeaders :: Path
```

```haskell
-- Openssl.hsig
signature Openssl where
  data OpensslLib
  openssl :: Derivation
  opensslVersion :: Version
  opensslHeaders :: Path
```

These are **package interfaces** — the same role as class declarations in the
typeclass encoding (§2), but at the module level. A signature says "I need
a package that provides these things" without naming a specific version or
build recipe.

### 9.2 Indefinite Packages as Abstract Definitions

A package that depends on signatures but not on concrete implementations is
**indefinite** — it can be instantiated with any compatible module:

```haskell
-- Openssl implementation (indefinite: depends on Zlib signature)
-- file: src/Openssl.hs
module Openssl where

import Zlib (zlib, zlibHeaders)

openssl :: Derivation
openssl = mkDrv "openssl" "3.1" [zlib]

opensslVersion :: Version
opensslVersion = v 3 1 0

opensslHeaders :: Path
opensslHeaders = "/include/openssl"
```

```cabal
-- openssl-indef.cabal
library
  signatures:       Zlib
  exposed-modules:  Openssl
  build-depends:    base, derix-core
```

The `signatures: Zlib` line declares that this package needs a `Zlib`
implementation but doesn't specify which one. The Haskell source code
`import Zlib` works against the signature — type-checked against the
interface, compiled against any implementation.

This is the Backpack analogue of:
```haskell
-- Typeclass encoding:
instance HasZlib pkgs => HasOpenssl pkgs where ...
```

Both say "given zlib, I can produce openssl." The typeclass version uses a
type variable; the Backpack version uses a module signature.

### 9.3 Instantiation as Package Set Construction

The package set is a concrete package that instantiates all signatures:

```cabal
-- nixpkgs.cabal
library
  build-depends:
    , zlib-impl          -- concrete: provides Zlib module
    , openssl-indef      -- indefinite: needs Zlib
    , curl-indef         -- indefinite: needs Openssl, Zlib
    , python-indef       -- indefinite: needs Openssl, Zlib

  mixins:
    , openssl-indef requires (Zlib as Zlib)
    , curl-indef    requires (Openssl as Openssl, Zlib as Zlib)
    , python-indef  requires (Openssl as Openssl, Zlib as Zlib)
```

The `mixins` section wires the dependencies: `openssl-indef` gets its `Zlib`
from `zlib-impl`, `curl-indef` gets its `Openssl` from `openssl-indef`
(itself instantiated with `zlib-impl`), and so on.

This IS the fixed point construction: the package set is defined as a
self-consistent wiring of all signatures to implementations.

### 9.4 Applicative Instantiation = Sharing = Coherence

Backpack uses **applicative instantiation**: if two packages depend on the
same signature and it is instantiated with the same module, they get the
**same instance**. This is a fundamental semantic guarantee.

In our calculus terms:
- `curl-indef` imports `Zlib` → gets `zlib-impl`
- `python-indef` imports `Zlib` → gets `zlib-impl`
- These are the **same module** — same compiled code, same identity

This is exactly the **coherence** property (calculus §7) and the **sharing** property
(CALCcalculus §5.4). In the typeclass encoding, coherence means "one dictionary per type."
In Backpack, applicative instantiation means "one module per signature
instantiation." Same guarantee, different mechanism.

The diamond dependency problem (CALCcalculus §9.4) is solved by construction:
```
curl-indef   → Zlib.hsig → zlib-impl     ← same module!
python-indef → Zlib.hsig → zlib-impl     ← same module!
```

No diamond conflict is possible because applicative instantiation forces
agreement.

### 9.5 Overlays as Re-Instantiation

An overlay swaps an implementation while keeping the rest:

```cabal
-- nixpkgs-patched.cabal
library
  build-depends:
    , zlib-impl          -- same
    , openssl-indef      -- same indefinite package
    , curl-indef
    , python-indef

  mixins:
    -- Re-instantiate openssl with a patched zlib:
    , openssl-indef requires (Zlib as Zlib)
        -- but this time Zlib comes from zlib-patched, not zlib-impl
```

Or bump openssl itself:

```cabal
-- Replace openssl-indef with openssl-v32-indef:
  mixins:
    , openssl-v32-indef requires (Zlib as Zlib)      -- new version
    , curl-indef requires (Openssl as Openssl, Zlib as Zlib)
    , python-indef requires (Openssl as Openssl, Zlib as Zlib)
```

`curl-indef` and `python-indef` are unchanged — they depend on the `Openssl`
signature, and the new instantiation provides a new implementation. The
cascade is automatic: downstream packages see the new openssl because the
mixin wiring changed.

This is the Backpack analogue of Nix's `final: prev:` overlays and the
typeclass encoding's newtype wrappers (§7.1).

### 9.6 Separate Compilation

A key advantage of Backpack over the typeclass encoding: **separate
compilation**. An indefinite package is type-checked against its signatures
independently. It doesn't need to see the implementation. This means:

- `openssl-indef` is compiled once, against `Zlib.hsig`
- It can be reused with `zlib@1.3` or `zlib@1.4` without recompilation
- Only the final linking step resolves signatures to implementations

In the typeclass encoding, GHC must specialize each class method per type,
which can lead to code duplication. Backpack avoids this by delaying
instantiation to link time.

For large package sets (80,000+ packages in nixpkgs), this matters: you
don't want to recompile the world when one package changes.

### 9.7 Cross-Compilation via Signature Sets

Cross-compilation is natural in Backpack: the toolchain is a set of
signatures, and different platforms provide different implementations:

```haskell
-- Toolchain.hsig
signature Toolchain where
  cc      :: Tool
  ld      :: Tool
  ar      :: Tool
  target  :: Platform
```

```cabal
-- native-toolchain provides Toolchain for x86_64
-- cross-aarch64-toolchain provides Toolchain for aarch64

library my-package-native
  mixins: my-package-indef requires (Toolchain from native-toolchain)

library my-package-cross-arm
  mixins: my-package-indef requires (Toolchain from cross-aarch64-toolchain)
```

The same indefinite package, two different instantiations — one native, one
cross-compiled. The package source code doesn't change; only the wiring does.

### 9.8 Bootstrap via Staged Instantiation

The bootstrap chain (§5) maps to **staged instantiation**:

```cabal
-- Stage 0: binary seeds implement the toolchain signature
library stage0-toolchain
  exposed-modules: Toolchain    -- binary gcc, binutils, glibc

-- Stage 1: rebuild gcc using stage0 toolchain
library stage1-gcc
  signatures: Toolchain
  mixins: stage1-gcc-indef requires (Toolchain from stage0-toolchain)

-- Stage 2: rebuild glibc using stage1 gcc
library stage2-glibc
  mixins: stage2-glibc-indef requires (Toolchain from stage1-toolchain)

-- Final: self-hosting
library final-toolchain
  mixins: gcc-indef requires (Toolchain from stage3-toolchain)
```

Each stage instantiates the toolchain signature with the previous stage's
output. The signature is the same at every stage — only the implementation
changes.

### 9.9 Backpack + Typeclasses: Hybrid Encoding

Backpack and typeclasses are complementary. A hybrid encoding uses each
where it is strongest:

- **Backpack** for the coarse-grained structure: which packages exist, how
  they wire together, separate compilation boundaries
- **Typeclasses** for the fine-grained properties: version predicates,
  associated types, superclass propagation, quantified constraints

```haskell
-- Zlib.hsig — Backpack signature with typeclass constraints
signature Zlib where
  import Derix.Types (Derivation, Version)

  class HasZlib pkgs where
    type ZlibVersion pkgs :: Version
    zlib :: Derivation
```

```haskell
-- Openssl.hs — indefinite module using both Backpack and typeclasses
module Openssl where

import Zlib (HasZlib(..), ZlibVersion)
import Derix.Types

class HasZlib pkgs => HasOpenssl pkgs where
  openssl :: Derivation
  openssl = mkDrv "openssl" "3.1" [zlib @pkgs]
```

The Backpack layer handles module structure and separate compilation. The
typeclass layer handles version constraints, associated types, and the rich
constraint language. Each mechanism does what it does best.

### 9.10 Comparison

| Aspect | Typeclass encoding | Backpack encoding |
|---|---|---|
| Abstraction level | Type | Module |
| Package interface | Class declaration | `.hsig` signature |
| Package definition | Instance declaration | Module implementation |
| Package set | Concrete type | Cabal mixin instantiation |
| Overlay | Newtype + GND | Re-instantiation |
| Sharing/coherence | One dict per type | Applicative instantiation |
| Separate compilation | No (specialization) | Yes |
| Version predicates | Type families | Not directly (external) |
| Associated types | Yes | Yes (abstract types in sigs) |
| Superclass propagation | Yes | Module re-exports |
| Quantified constraints | Yes | Not available |
| Deriving | GND, DerivingVia | Not available |
| GHC extensions needed | Many | None (Cabal-level) |
| Composition | Newtype stacking | Mixin composition |
| Closest to | GHC dictionary passing | ML functors / SML modules |

**Backpack excels at**: large-scale structure, separate compilation, simplicity
(no type-level gymnastics), clean overlay model via re-instantiation.

**Typeclasses excel at**: rich constraints, associated types, automated
generation (deriving), quantified properties, fine-grained version checking.

**The hybrid** (§9.9) combines both: Backpack for structure, typeclasses
for properties. This mirrors how real systems work — module systems handle
the coarse structure, type systems handle the fine invariants.


## §10 ConstraintKinds: Dependencies as First-Class Types

GHC's `ConstraintKinds` extension promotes `Constraint` from a special
syntactic category to a first-class kind. Constraints become types that can
be:

- **Aliased** with `type` synonyms
- **Computed** by type families
- **Passed** as type parameters
- **Stored** in data types (via GADTs)
- **Composed** with type-level operations

Since constraints ARE dependencies in our encoding, `ConstraintKinds` makes
**dependency sets into first-class type-level data** — you can name them,
compute them, transform them, and reify them.

```haskell
{-# LANGUAGE ConstraintKinds #-}
import Data.Kind (Constraint)
```

### 10.1 Dependency Group Aliases

The simplest use: name a recurring set of dependencies:

```haskell
type NetworkDeps pkgs =
  (HasOpenssl pkgs, HasCurl pkgs, HasWget pkgs, HasZlib pkgs)

type BuildEssentials pkgs =
  (HasGcc pkgs, HasMake pkgs, HasBinutils pkgs, HasPkgConfig pkgs)

type PythonDevDeps pkgs =
  (HasPython pkgs, HasPip pkgs, HasSetuptools pkgs, HasWheel pkgs)
```

Used in instance declarations:

```haskell
instance (NetworkDeps pkgs, BuildEssentials pkgs) => HasWget pkgs where
  wget = mkDrv "wget" "1.21" [#openssl, #curl, #zlib, #gcc, #make]
```

And in function signatures:

```haskell
buildWebServer :: NetworkDeps pkgs => Derivation
deployApp :: (NetworkDeps pkgs, PythonDevDeps pkgs) => IO ()
```

This is **dependency abstraction**: instead of listing 10 individual
constraints everywhere, name the pattern once. Change the group definition,
and all uses update automatically.

Without `ConstraintKinds`, these aliases are not fully first-class — you
cannot pass them to type families or use them as type parameters. With
`ConstraintKinds`, they are just types of kind `* -> Constraint`.

### 10.2 Computed Dependencies

Type families can return `Constraint`, enabling dependency sets that are
**computed** from configuration:

**Platform-conditional dependencies:**

```haskell
type family PlatformDeps (p :: Platform) pkgs :: Constraint where
  PlatformDeps 'X86_64Linux  pkgs = (HasSystemd pkgs, HasInotify pkgs, HasAlsa pkgs)
  PlatformDeps 'Aarch64Linux pkgs = (HasSystemd pkgs, HasInotify pkgs)
  PlatformDeps 'X86_64Darwin pkgs = (HasLaunchd pkgs, HasFsevents pkgs, HasCoreAudio pkgs)
  PlatformDeps 'Wasm32       pkgs = ()

instance (HasStdenv pkgs, PlatformDeps (HostPlatform pkgs) pkgs)
  => HasMyApp pkgs where ...
```

The dependency set changes based on the target platform — at compile time.
On Linux, `HasMyApp` requires systemd and inotify. On WASM, no platform
deps at all. The type checker ensures each platform's dependencies are met.

**Feature-flag-conditional dependencies:**

```haskell
type family FeatureDeps (features :: [Symbol]) pkgs :: Constraint where
  FeatureDeps '[]            pkgs = ()
  FeatureDeps ("ssl" ': fs)  pkgs = (HasOpenssl pkgs, FeatureDeps fs pkgs)
  FeatureDeps ("gui" ': fs)  pkgs = (HasGtk pkgs, HasCairo pkgs, FeatureDeps fs pkgs)
  FeatureDeps ("db"  ': fs)  pkgs = (HasSqlite pkgs, FeatureDeps fs pkgs)
  FeatureDeps (_     ': fs)  pkgs = FeatureDeps fs pkgs

type PythonFeatures = '["ssl", "db"]

instance FeatureDeps PythonFeatures pkgs => HasPython pkgs where ...
```

Change the feature list, and the dependency set recomputes. Add `"gui"` and
the type checker demands `HasGtk` and `HasCairo`.

### 10.3 The `DepsOf` Pattern

A single type family that maps every package name to its constraint set:

```haskell
type family DepsOf (name :: Symbol) pkgs :: Constraint where
  DepsOf "zlib"    pkgs = ()
  DepsOf "openssl" pkgs = Has "zlib" pkgs
  DepsOf "curl"    pkgs = (Has "openssl" pkgs, Has "zlib" pkgs)
  DepsOf "python"  pkgs = (Has "openssl" pkgs, Has "zlib" pkgs, Has "readline" pkgs)
  DepsOf "wget"    pkgs = (Has "openssl" pkgs, Has "curl" pkgs, Has "zlib" pkgs)
```

This is a **dependency database as a type family**. Now `Has` can use it:

```haskell
class DepsOf name pkgs => Has (name :: Symbol) pkgs where
  dep :: Derivation
```

The constraint `DepsOf name pkgs` is automatically required for every `Has`
instance. The dependency database is defined once (in `DepsOf`) and
enforced everywhere (through the superclass).

**Generic resolution:**

```haskell
-- "Resolve this package given its deps are met":
resolve :: (Has name pkgs, DepsOf name pkgs) => Proxy name -> Derivation
resolve = dep
```

The `DepsOf` pattern centralizes the dependency graph in one place,
separate from the build recipes. Change a dependency, and all affected
packages must be updated — enforced by the type checker.

### 10.4 Constraint Algebra

With constraints as types, you can build an algebra over dependency sets:

**Union** — needs everything from both:

```haskell
class (a pkgs, b pkgs) => Union a b pkgs
instance (a pkgs, b pkgs) => Union a b pkgs

type DevDeps = Union BuildEssentials PythonDevDeps
-- DevDeps pkgs = (HasGcc pkgs, HasMake pkgs, ..., HasPython pkgs, HasPip pkgs, ...)
```

**Implies** — one dependency set entails another (with `QuantifiedConstraints`):

```haskell
type Implies a b = forall pkgs. a pkgs => b pkgs

-- NetworkDeps implies HasOpenssl:
type NetworkHasSSL = Implies NetworkDeps HasOpenssl
```

**Weaken** — drop a dependency from a set:

```haskell
-- Not directly expressible as type family subtraction,
-- but achievable by defining a new alias without the dropped dep:
type NetworkDepsNoWget pkgs = (HasOpenssl pkgs, HasCurl pkgs, HasZlib pkgs)
```

**Conditional** — choose a dependency set based on a flag:

```haskell
type family Choose (b :: Bool) (a :: * -> Constraint) (b' :: * -> Constraint)
    :: * -> Constraint where
  Choose 'True  a _ = a
  Choose 'False _ b = b

type SSLDeps = Choose UseLibreSSL HasLibressl HasOpenssl
```

### 10.5 Dict: Reified Dependencies

The `Dict` GADT captures a constraint as a runtime value:

```haskell
data Dict (c :: Constraint) where
  Dict :: c => Dict c
```

In the package world, `Dict` reifies the question "does this package set
have package X?" into a runtime-inspectable value:

```haskell
-- Check whether openssl is available:
hasOpenssl :: forall pkgs. Maybe (Dict (HasOpenssl pkgs))

-- A package manifest as a list of reified constraints:
type Manifest pkgs =
  [ ("zlib",    Dict (HasZlib pkgs))
  , ("openssl", Dict (HasOpenssl pkgs))
  , ("curl",    Dict (HasCurl pkgs))
  ]
```

**Runtime dependency resolution.** Instead of compile-time resolution only,
`Dict` enables a hybrid model:

```haskell
-- Try to resolve at compile time; fall back to runtime check:
buildOptionalSSL :: Maybe (Dict (HasOpenssl pkgs)) -> Derivation
buildOptionalSSL (Just Dict) = mkDrv "myapp" [#openssl]   -- with SSL
buildOptionalSSL Nothing     = mkDrv "myapp-no-ssl" []     -- without
```

**Constraint entailment at runtime:**

```haskell
-- Given NetworkDeps, produce evidence of HasOpenssl:
networkHasSSL :: Dict (NetworkDeps pkgs) -> Dict (HasOpenssl pkgs)
networkHasSSL Dict = Dict    -- HasOpenssl is part of NetworkDeps
```

This is a **runtime proof** that one dependency set entails another. The
function is total — it always succeeds because the entailment holds by
definition. The type system guarantees it.

### 10.6 Constraint-Parameterized Builders

Build patterns (§7.2) can be parameterized by their dependency constraint:

```haskell
class Builder (deps :: * -> Constraint) where
  buildWith :: deps pkgs => SourceTree -> [Dep pkgs] -> Derivation

-- CMake builder requires cmake + ninja + gcc:
instance Builder CMakeBuildDeps where
  buildWith src extras =
    cmake src >> ninja >> install

-- Autotools builder requires make + gcc:
instance Builder AutotoolsBuildDeps where
  buildWith src extras =
    configure src >> make >> makeInstall
```

Now a package declares which builder it uses **by its dependency constraint**:

```haskell
type instance BuilderFor "openssl" = CMakeBuildDeps
type instance BuilderFor "zlib"    = AutotoolsBuildDeps

-- Generic build:
buildPkg :: (Builder (BuilderFor name), BuilderFor name pkgs)
         => Proxy name -> Derivation
buildPkg name = buildWith @(BuilderFor name) (srcOf name) (depsOf name)
```

The builder is selected by the constraint type. Wrong builder for wrong
package is a type error — you can't use the autotools builder for a cmake
project because the constraint doesn't match.

### 10.7 The Constraint Store Pattern

A package set can be represented as a **constraint store** — a type-level
map from names to constraints:

```haskell
type family Lookup (name :: Symbol) (store :: [(Symbol, * -> Constraint)])
    :: * -> Constraint where
  Lookup name ('(name, c) ': _)    = c
  Lookup name (_          ': rest) = Lookup name rest

type MyStore =
  '[ '("zlib",    HasZlib)
   , '("openssl", HasOpenssl)
   , '("curl",    HasCurl)
   ]

-- Extract a specific constraint:
type ZlibConstraint = Lookup "zlib" MyStore    -- HasZlib
```

**Package set operations as store operations:**

```haskell
-- Add a package to the store:
type Add name c store = '(name, c) ': store

-- Remove a package (filter):
type family Remove (name :: Symbol) (store :: [(Symbol, * -> Constraint)])
    :: [(Symbol, * -> Constraint)] where
  Remove name ('(name, _) ': rest) = rest
  Remove name (entry      ': rest) = entry ': Remove name rest
  Remove name '[] = '[]

-- Merge two stores (overlay):
type family Merge (overlay :: [(Symbol, * -> Constraint)])
                  (base :: [(Symbol, * -> Constraint)])
    :: [(Symbol, * -> Constraint)] where
  Merge '[]              base = base
  Merge ('(n, c) ': rest) base = '(n, c) ': Merge rest (Remove n base)
```

This is a **type-level package set** where the entries are constraints.
Overlays are `Merge` operations. The entire dependency graph exists at the
type level and can be manipulated before any runtime code executes.

### 10.8 Summary

| ConstraintKinds use | What it enables |
|---|---|
| Dependency aliases (§10.1) | Name recurring dependency patterns |
| Computed deps (§10.2) | Platform/feature-conditional dependencies |
| `DepsOf` pattern (§10.3) | Centralized dependency database |
| Constraint algebra (§10.4) | Union, implication, conditional selection |
| Dict reification (§10.5) | Runtime dependency checking + evidence |
| Parameterized builders (§10.6) | Builder selected by constraint type |
| Constraint store (§10.7) | Type-level package set with full algebra |

The common thread: without `ConstraintKinds`, dependencies are syntactic
artifacts — they appear in instance contexts and cannot be manipulated.
With `ConstraintKinds`, dependencies become **type-level data**: named,
computed, stored, composed, reified. The dependency graph itself becomes
a type-level structure that the compiler can reason about.

This is the foundation that makes §6.5 (associated constraints), §8
(quantified constraints), and the `DepsOf` centralized dependency pattern
possible. It is, in a sense, the extension that turns Haskell's constraint
system from a proof checker into a **dependency management system**.


## §11 Type Families: Type-Level Computation for Package Resolution

Type families are **type-level functions** — they compute types from types.
They have appeared throughout this document (§4 `CmpVersion`, §6 associated
types, §10 `DepsOf`), but their full power as a resolution mechanism deserves
dedicated treatment.

The central insight: **type family reduction IS dependency resolution**. When
GHC reduces `Resolve "curl"`, it is literally performing proof search — looking
up the definition, recursively resolving dependencies, and constructing the
evidence. The type checker is the resolver.

```haskell
{-# LANGUAGE TypeFamilies, DataKinds, UndecidableInstances #-}
```

### 11.1 Open vs Closed: Two Package Models

Type families come in two flavors, each modeling a fundamentally different
package management philosophy.

**Open type families** — instances can be added in any module:

```haskell
type family Resolve (name :: Symbol) :: Evidence

-- In module Zlib:
type instance Resolve "zlib" = 'Ev "zlib" ('V 1 3 0) '[]

-- In module Openssl:
type instance Resolve "openssl" = 'Ev "openssl" ('V 3 1 0) '[Resolve "zlib"]

-- In a user's overlay module:
type instance Resolve "mylib" = 'Ev "mylib" ('V 0 1 0) '[Resolve "zlib"]
```

This models **extensible package sets** — anyone can add packages, like
nixpkgs with overlays. The set is open-ended; new packages can appear in
any module.

**Closed type families** — all equations in one declaration, ordered:

```haskell
type family Resolve (name :: Symbol) :: Evidence where
  Resolve "openssl" = 'Ev "openssl" ('V 3 2 0) '[Resolve "zlib"]   -- priority 1
  Resolve "openssl" = 'Ev "openssl" ('V 3 1 0) '[Resolve "zlib"]   -- never reached
  Resolve "zlib"    = 'Ev "zlib" ('V 1 3 0) '[]
  Resolve "curl"    = 'Ev "curl" ('V 8 5 0) '[Resolve "openssl", Resolve "zlib"]
```

Wait — closed type families don't allow duplicate LHS patterns in the same
way. But the **ordering** of equations gives priority, which models
**selection policy** (calculus §7):

```haskell
-- Selection: try specific version first, fall back to general
type family SelectImpl (name :: Symbol) (variant :: Symbol) :: Evidence where
  SelectImpl "tls" "libressl" = ...      -- if variant matches, use this
  SelectImpl "tls" _          = ...      -- default: openssl
```

**The correspondence:**

| Type family flavor | Package model | Analogue |
|---|---|---|
| Open | Extensible, anyone can add | nixpkgs + overlays |
| Closed | Fixed set, ordered priority | Lock file, pinned versions |
| Associated | Per-class, varies by instance | Per-package-set configuration |

### 11.2 Type Family Reduction as Resolution

GHC's type family reduction is a rewriting system: it matches equations
top-down and substitutes. This is exactly proof search.

Define the evidence type and the resolver as a type family:

```haskell
data Evidence = Ev Symbol Version [Evidence]

type family Resolve (name :: Symbol) :: Evidence where
  Resolve "zlib"    = 'Ev "zlib"    ('V 1 3 0) '[]
  Resolve "openssl" = 'Ev "openssl" ('V 3 1 0) '[Resolve "zlib"]
  Resolve "curl"    = 'Ev "curl"    ('V 8 5 0) '[Resolve "openssl", Resolve "zlib"]
  Resolve "python"  = 'Ev "python"  ('V 3 12 0) '[Resolve "openssl", Resolve "zlib"]
```

When GHC encounters `Resolve "python"`, it:

1. Matches the equation for `"python"`
2. Finds `'Ev "python" ('V 3 12 0) '[Resolve "openssl", Resolve "zlib"]`
3. Recursively reduces `Resolve "openssl"` → needs `Resolve "zlib"` → base case
4. Recursively reduces `Resolve "zlib"` → base case (or **sharing** if already reduced)
5. Produces the full evidence tree

This IS the resolution algorithm from calculus §3-4, executed by GHC's type checker.
The parallel:

| Calculus concept | Type family mechanism |
|---|---|
| Constraint | Type-level `Symbol` (package name) |
| Evidence | Promoted `Evidence` type |
| Resolution rule [INST] | Type family equation |
| Proof search | Top-down equation matching |
| Sharing | GHC's type family reduction cache |
| Cycle detection | GHC's loop detection (`-freduction-depth`) |
| Resolution failure | Stuck type family (no matching equation) |

### 11.3 Stuck Families as Resolution Failure

When no type family equation matches, the family is **stuck** — it does not
reduce. In the package world, a stuck family is an **unsatisfied dependency**:

```haskell
type family Resolve (name :: Symbol) :: Evidence where
  Resolve "zlib"    = ...
  Resolve "openssl" = ...
  -- no equation for "missing-pkg"

-- Usage:
type Test = Resolve "missing-pkg"
-- GHC: cannot reduce Resolve "missing-pkg"
-- = dependency resolution failure
```

With `TypeError`, the failure message is readable:

```haskell
type family Resolve (name :: Symbol) :: Evidence where
  Resolve "zlib"    = 'Ev "zlib" ('V 1 3 0) '[]
  Resolve "openssl" = 'Ev "openssl" ('V 3 1 0) '[Resolve "zlib"]
  Resolve name      = TypeError ('Text "Package " ':<>: 'ShowType name
                                 ':<>: 'Text " not found in package set")
```

GHC reports: `Package "missing-pkg" not found in package set`. Compare to
Nix's `error: attribute 'missing-pkg' missing` — same error, caught at
compile time instead of evaluation time.

### 11.4 Data Families: Per-Package Configuration

**Data families** define indexed data types where each instance has its own
representation. For packages, this means each package has its own
configuration type:

```haskell
data family Config (name :: Symbol)

data instance Config "openssl" = OpensslConfig
  { opensslFips      :: Bool
  , opensslNoSSL3    :: Bool
  , opensslNoWeakCiphers :: Bool
  }

data instance Config "python" = PythonConfig
  { pythonWithTk     :: Bool
  , pythonWithSSL    :: Bool
  , pythonOptLevel   :: Int
  }

data instance Config "gcc" = GccConfig
  { gccLanguages     :: [String]    -- ["c", "c++", "fortran"]
  , gccMultilib      :: Bool
  }
```

Unlike a regular `data Config = Config ...` that must accommodate all
packages in one type, data families give each package a **structurally
distinct** configuration. A function that configures openssl only sees
openssl's options:

```haskell
configureOpenssl :: Config "openssl" -> Derivation
configureOpenssl cfg = mkDrv "openssl" (flags cfg)
  where
    flags cfg = [if opensslFips cfg then "--fips" else ""]
             ++ [if opensslNoSSL3 cfg then "--no-ssl3" else ""]
```

**Default configurations** via another type family:

```haskell
type family DefaultConfig (name :: Symbol) :: Config name

type instance DefaultConfig "openssl" =
  'OpensslConfig 'True 'True 'True     -- secure defaults

type instance DefaultConfig "python" =
  'PythonConfig 'False 'True 2          -- no Tk, with SSL, -O2
```

This models Nix's `override` pattern: each package has its own set of
knobs, and there are sensible defaults that can be overridden.

### 11.5 Injective Type Families: Package Identity

`TypeFamilyDependencies` enables **injective type families** — the output
uniquely determines the input:

```haskell
{-# LANGUAGE TypeFamilyDependencies #-}

type family StoreHash (name :: Symbol) (ver :: Version) = (h :: Symbol) | h -> name ver
```

The `| h -> name ver` annotation says: given a hash, you can recover the
package name and version. This is **content addressing**: the hash uniquely
identifies the package.

```haskell
type instance StoreHash "zlib" ('V 1 3 0) = "sha256-abc123"
type instance StoreHash "openssl" ('V 3 1 0) = "sha256-def456"
```

Injectivity enables **reverse lookup**: given a store hash, GHC can infer
which package produced it:

```haskell
-- GHC can infer name and ver from the hash:
fromStore :: StoreHash name ver ~ hash => Proxy hash -> (Proxy name, Proxy ver)
fromStore _ = (Proxy, Proxy)
```

This captures a fundamental property of Nix's content-addressed store:
the store path determines the package. Here it's a type-level invariant.

### 11.6 Type-Level Package Set Operations

Type families enable bulk operations over package sets represented as
type-level lists:

**Map** — apply a transformation to every package:

```haskell
type family MapVersion (f :: Version -> Version) (pkgs :: [PkgEntry])
    :: [PkgEntry] where
  MapVersion f '[] = '[]
  MapVersion f ('PkgE name ver ': rest) = 'PkgE name (f ver) ': MapVersion f rest
```

**Filter** — select packages matching a predicate:

```haskell
type family FilterByPred (pred :: Symbol -> Bool) (pkgs :: [PkgEntry])
    :: [PkgEntry] where
  FilterByPred pred '[] = '[]
  FilterByPred pred ('PkgE name ver ': rest) =
    If (pred name)
      ('PkgE name ver ': FilterByPred pred rest)
      (FilterByPred pred rest)
```

**Fold** — aggregate over the package set:

```haskell
-- Count packages:
type family Length (pkgs :: [PkgEntry]) :: Nat where
  Length '[] = 0
  Length (_ ': rest) = 1 + Length rest

-- Collect all dependency edges:
type family AllEdges (pkgs :: [PkgEntry]) :: [DepEdge] where
  AllEdges '[] = '[]
  AllEdges ('PkgE name _ ': rest) =
    Append (EdgesOf name) (AllEdges rest)
```

**Topological sort** — order packages by dependency depth:

```haskell
type family TopoSort (pkgs :: [PkgEntry]) (graph :: [DepEdge]) :: [Symbol] where
  -- packages with no dependencies first, then their dependents, etc.
  ...
```

These operations enable **type-level package set queries**: "how many
packages depend on openssl?", "what is the dependency depth of python?",
"is the package set topologically consistent?" — all answered at compile time.

### 11.7 Closed Families for Selection Policy

The closed type family's ordered matching models **version selection with
priority** — the calculus §7 selection policies as type-level computation:

```haskell
-- Most-specific policy: try exact match, then range, then any:
type family SelectOpenssl (pred :: VerPred) :: Version where
  SelectOpenssl ('Exact ('V 3 1 0))  = 'V 3 1 0
  SelectOpenssl ('Exact ('V 3 2 0))  = 'V 3 2 0
  SelectOpenssl ('AtLeast ('V 3 0 0)) = 'V 3 2 0     -- highest available
  SelectOpenssl ('AtLeast ('V 1 0 0)) = 'V 3 2 0     -- still highest
  SelectOpenssl 'Any                  = 'V 3 2 0     -- default to highest
```

**Generalized selection** across all packages:

```haskell
type family Select (name :: Symbol) (pred :: VerPred) :: Version where
  -- openssl versions: 3.2 > 3.1 > 1.1
  Select "openssl" ('AtLeast ('V 3 0 0)) = 'V 3 2 0
  Select "openssl" ('AtLeast ('V 1 0 0)) = 'V 3 2 0    -- highest satisfying
  Select "openssl" ('Range ('V 1 0 0) ('V 2 0 0)) = 'V 1 1 1  -- range match
  -- zlib versions: 1.3 > 1.2
  Select "zlib" ('AtLeast ('V 1 0 0)) = 'V 1 3 0
  Select "zlib" 'Any = 'V 1 3 0
  -- fallthrough:
  Select name pred = TypeError ('Text "No version of " ':<>: 'ShowType name
                                ':<>: 'Text " satisfies " ':<>: 'ShowType pred)
```

The equation ordering IS the selection priority. GHC tries top-down and
commits to the first match — exactly the "no backtracking" policy of calculus §7.
The closed world assumption means no external module can add versions
after the fact — this is a **locked** package set.

### 11.8 Open Families for Extensible Package Sets

Open type families model the opposite: anyone can add packages:

```haskell
-- Core packages (in base module):
type family Resolve (name :: Symbol) :: Evidence
type instance Resolve "zlib" = 'Ev "zlib" ('V 1 3 0) '[]
type instance Resolve "openssl" = 'Ev "openssl" ('V 3 1 0) '[Resolve "zlib"]

-- User extension (in overlay module):
type instance Resolve "mylib" = 'Ev "mylib" ('V 0 1 0) '[Resolve "zlib"]
```

This is the **overlay model**: new packages can be added from any module.
GHC ensures consistency — two modules cannot define `type instance Resolve
"zlib"` differently (that would be a conflicting instance error). This IS
coherence (calculus §7): one version per package, enforced by the compiler.

**The open/closed decision is the coherence/extensibility trade-off:**

| | Open families | Closed families |
|---|---|---|
| Add packages | Any module | Only in the defining module |
| Override versions | Not allowed (conflicts) | Ordered priority |
| Coherence | By construction | By equation ordering |
| Model | nixpkgs + overlays | Lock file / pinned set |
| Separate compilation | Yes | No (all equations in one place) |

### 11.9 Associated vs Top-Level: Scoping

The choice between associated type families (in a class) and top-level type
families (standalone) determines **scoping**:

```haskell
-- Associated: scoped per package set (varies by pkgs)
class Has (name :: Symbol) pkgs where
  type Ver name pkgs :: Version

-- Top-level: global (one answer regardless of pkgs)
type family GlobalVer (name :: Symbol) :: Version
```

For package management, this means:
- **Associated**: version of openssl depends on which package set you're in.
  `Ver "openssl" Nixpkgs` might be `'V 3 1 0` while `Ver "openssl" Unstable`
  is `'V 3 2 0`.
- **Top-level**: there is one global answer. Suitable for lock files or
  single-set configurations.

The associated form enables the `pkgs`-parameterized encodings (§1-§3).
The top-level form is simpler and enables the closed-world optimizations
of §11.7.

### 11.10 Summary

| Type family feature | Package management use |
|---|---|
| Open families (§11.1, §11.8) | Extensible package sets, overlays |
| Closed families (§11.1, §11.7) | Locked sets, selection policies with priority |
| Reduction as resolution (§11.2) | GHC type checker IS the resolver |
| Stuck families (§11.3) | Unsatisfied dependencies as type errors |
| Data families (§11.4) | Per-package configuration types |
| Injective families (§11.5) | Content-addressed store identity |
| Bulk operations (§11.6) | Map/filter/fold over package sets |
| Associated families (§11.9) | Per-package-set versioning |

Type families are the computational engine behind everything in §4-§10.
`DataKinds` provides the data (promoted types). `ConstraintKinds` provides
the constraints (dependencies as types). Type families provide the
**computation** — the type-level functions that resolve, select, transform,
and verify. Together, they turn GHC's type system into a complete package
resolution engine.


## §12 GADTs: Type-Indexed Evidence

The calculus's evidence terms (`inst(p, v, e₁...eₙ)`, `(e₁, e₂)`, `★`) are
untyped — nothing in their runtime representation says *which* constraint they
prove. GADTs fix this: evidence becomes **type-indexed**, and construction is
the only way to produce it.

### 12.1 Evidence as a GADT

The core calculus evidence terms, encoded as a GADT:

```haskell
data Evidence (c :: Constraint) where
  Star :: Evidence ()
  Pair :: Evidence c1 -> Evidence c2 -> Evidence (c1, c2)
  Inst :: (KnownSymbol name, KnownVersion ver)
       => Proxy name -> Proxy ver
       -> Evidence deps          -- sub-evidence for dependencies
       -> Evidence (Has name ver)
```

The type index `c` records which constraint this evidence proves. You cannot
construct `Evidence (Has "zlib" '(1,3,0))` without actually resolving zlib's
dependencies — the type system enforces it.

Pattern matching on `Evidence c` refines `c`, giving the caller access to
the resolved name, version, and sub-evidence:

```haskell
inspect :: Evidence (Has "openssl" ver) -> (Evidence (Has "zlib" zver), ...)
inspect (Inst _ _ deps) = ... -- 'ver' and dep evidence available
```

### 12.2 Existential Version Hiding

The resolver picks a version, but consumers often don't care which:

```haskell
-- Exact: I know which version
type Resolved name ver = Evidence (Has name ver)

-- Existential: some version was resolved, I don't know which
data SomeResolved (name :: Symbol) where
  MkResolved :: KnownVersion ver
             => Evidence (Has name ver)
             -> SomeResolved name
```

`SomeResolved "zlib"` says "zlib was resolved" without committing to a
version. This is the `∃v` quantifier — the resolver picks `v` existentially,
and the consumer can only use operations that work for *any* version.

The two levels correspond to different dependency styles:
- **Exact** (`Resolved "zlib" '(1,3,0)`): lockfile entry, reproducible build
- **Existential** (`SomeResolved "zlib"`): version-range dependency, flexible

### 12.3 Version Predicate Witnesses

A GADT that witnesses constraint satisfaction:

```haskell
data Satisfies (pred :: VerPred) (ver :: Version) where
  SatExact   :: Satisfies ('Exact v) v
  SatAtLeast :: (CmpVersion v lo ~ 'GT)
             => Satisfies ('AtLeast lo) v
  SatRange   :: (CmpVersion v lo ~ 'GT, CmpVersion v hi ~ 'LT)
             => Satisfies ('Range lo hi) v
  SatAny     :: Satisfies 'AnyVersion v
```

Pattern matching on `Satisfies pred ver` gives you the proof that `ver |= pred`.
The resolver produces these witnesses; downstream code consumes them:

```haskell
resolve :: Env -> Constraint pred -> (SomeVersion ver, Satisfies pred ver)
```

The return type guarantees the resolved version actually satisfies the
requested predicate — not by convention, but by construction.

### 12.4 Heterogeneous Package Sets

A package set as a type-indexed list:

```haskell
data PkgSet (manifest :: [(Symbol, Version)]) where
  Empty :: PkgSet '[]
  Add   :: Evidence (Has name ver)
        -> PkgSet rest
        -> PkgSet ('(name, ver) ': rest)
```

The type-level manifest tracks exactly which packages at which versions are
present. You cannot add zlib@1.3 to a set whose manifest says zlib@1.2 —
the types don't unify. This is §4's `Manifest` (DataKinds) made
*constructive* — the GADT enforces the manifest by construction, not just
declaration.

Lookup is type-safe:

```haskell
type family HasEntry (name :: Symbol) (ver :: Version)
              (manifest :: [(Symbol, Version)]) :: Constraint where
  HasEntry n v ('(n, v) ': _)    = ()
  HasEntry n v ('(_, _) ': rest) = HasEntry n v rest

lookupPkg :: HasEntry name ver manifest
          => PkgSet manifest -> Evidence (Has name ver)
```

### 12.5 Phase-Indexed Builds

Build pipelines as a type-safe state machine:

```haskell
data Phase = Fetch | Configure | Build | Install | Done

data BuildStep (before :: Phase) (after :: Phase) where
  DoFetch     :: Sources    -> BuildStep 'Fetch     'Configure
  DoConfigure :: ConfigOpts -> BuildStep 'Configure 'Build
  DoBuild     :: BuildFlags -> BuildStep 'Build     'Install
  DoInstall   :: InstallDir -> BuildStep 'Install   'Done
```

Steps compose only when phases match:

```haskell
andThen :: BuildStep a b -> BuildStep b c -> BuildPlan a c
```

You cannot install before building — `BuildStep 'Fetch 'Install` is
uninhabited. The Nix build phases (`unpackPhase`, `configurePhase`,
`buildPhase`, `installPhase`) become type-safe.

### 12.6 Resolution Proof Trees

The Curry-Howard payoff. Each constructor is an inference rule:

```haskell
data Proof (env :: [Decl]) (c :: Constraint) where
  -- ⊤-intro
  PTriv :: Proof env ()

  -- ∧-intro
  PPair :: Proof env c1 -> Proof env c2 -> Proof env (c1, c2)

  -- Instance rule: if declaration d is in env,
  -- and we can prove all of d's requirements,
  -- then we can prove d's head
  PInst :: Member d env              -- d ∈ Γ
        -> Proofs env (DeclReqs d)   -- proofs for all dependencies
        -> Proof env (Has (DeclPkg d) (DeclVer d))
```

This IS the derivation tree from calculus §3's inference rules, but now the type
system verifies that every step is valid. An ill-formed proof tree (wrong
dependency, missing sub-proof, circular reasoning) is a *type error*.

The triple pun completes: `Proof env c` is simultaneously a Nix derivation
(build plan), a logical derivation (proof tree), and a Haskell deriving
artifact (evidence) — and the GADT makes all three type-safe.

### 12.7 Singleton Bridge

GADTs connect type-level versions (DataKinds, §4) to runtime values:

```haskell
data SVersion (v :: Version) where
  SVersion :: (KnownNat major, KnownNat minor, KnownNat patch)
           => SVersion '(major, minor, patch)

-- Runtime comparison using type-level knowledge
compareSVersions :: SVersion v1 -> SVersion v2 -> Ordering
compareSVersions (SVersion @m1 @n1 @p1) (SVersion @m2 @n2 @p2) =
  compare (natVal @m1 Proxy, natVal @n1 Proxy, natVal @p1 Proxy)
          (natVal @m2 Proxy, natVal @n2 Proxy, natVal @p2 Proxy)
```

The resolver operates at the type level (via type families, §11) but produces
`SVersion` singletons when it needs to emit runtime build plans. The GADT
guarantees the runtime value matches the type-level decision.

### 12.8 Safe Version Coercions

When two versions are compatible (same major version under semver), a GADT
witnesses the safe substitution:

```haskell
data Compatible (v1 :: Version) (v2 :: Version) where
  MkCompat :: (Major v1 ~ Major v2)
           => Compatible v1 v2

-- Safely substitute one version for another
upgrade :: Compatible v1 v2
        -> Evidence (Has name v1) -> Evidence (Has name v2)
```

Without the `Compatible` witness, version substitution is a type error.
This makes the semver contract enforceable: a minor bump produces a
`Compatible` witness, a major bump doesn't, and the type system ensures
you don't mix them up.

### 12.9 Scoped Environments

GADTs can enforce that evidence is only used in the scope where it was
resolved:

```haskell
data Scoped (env :: [Decl]) a where
  InScope :: (AllResolved env => a) -> Scoped env a

runScoped :: Proofs env (AllConstraints env) -> Scoped env a -> a
```

This prevents using evidence from one package set in another — the `env`
index ties evidence to its resolution context. Combined with overlays (calculus §6),
this ensures that overridden packages use the *overlaid* evidence, not stale
evidence from the base layer.

### 12.10 Summary

| GADT feature | Package management use |
|---|---|
| Type-indexed evidence (§12.1) | Proof that a specific package was resolved |
| Existential hiding (§12.2) | Version-flexible dependencies (`∃v. Has p v`) |
| Predicate witnesses (§12.3) | Proof that version satisfies constraint |
| Heterogeneous sets (§12.4) | Manifest-correct package sets by construction |
| Phase indexing (§12.5) | Type-safe build pipelines |
| Proof trees (§12.6) | Type-checked derivation trees (Curry-Howard) |
| Singletons (§12.7) | Type-level ↔ runtime version bridge |
| Safe coercions (§12.8) | Semver-enforced version substitution |
| Scoped environments (§12.9) | Evidence tied to its resolution context |

GADTs are the **verification layer**. Everything in §4-§11 constructs
type-level representations and computations. GADTs make those representations
*trustworthy* — if you have a value of the right GADT type, the property
holds by construction, not by convention.


## §13 Open Questions and Extensions

### 13.1 Backtracking

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

### 13.2 Quantified Constraints

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

### 13.3 Semantic Versioning as Type Change

A breaking change (major version bump) corresponds to a change in the
"evidence type" — the package now provides a different interface. A minor
bump adds new evidence while preserving the old. This could be formalized
with structural subtyping on evidence:

```haskell
-- minor bump:   e_old :: C_old   and   e_new :: (C_old, C_extra)
-- patch bump:   same evidence type, different implementation
-- major bump:   C_new is incompatible with C_old
```

### 13.4 Build-Time Configuration as Associated Types

A package parameterized by build configuration (e.g., Python with/without SSL)
corresponds to a typeclass with **associated types**:

```haskell
class Package p where
  type Config p :: *
  build :: Config p -> Derivation
```

In our calculus, this could be modeled by indexing evidence by a configuration
parameter, making constraints richer.

### 13.5 Proof Search Strategies

The current calculus uses depth-first resolution (like GHC). Alternatives:
- Breadth-first (complete but slower)
- Iterative deepening (complete, better space)
- Tabled resolution (like XSB Prolog — memoize failed branches)

The choice of strategy affects termination behavior on non-terminating
inputs but does not affect the set of solvable constraints when resolution
does terminate.
