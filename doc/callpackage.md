# Derix: The `callPackage` Encoding

Starting from scratch. If `pkgs` is a type, what is `callPackage`?

Building is delegated to the Nix daemon — `Derivation` is a concrete
Nix store derivation, not an abstract type.

## §0 `Derivation`: The Concrete Type

A `Derivation` is exactly what the Nix daemon builds — a `.drv` file in
the Nix store. The Haskell type mirrors the ATerm format:

```haskell
data Derivation = Derivation
  { drvOutputs   :: Map Text DerivationOutput
  , drvInputDrvs :: Map StorePath (Set Text)  -- drv path → outputs needed
  , drvInputSrcs :: Set StorePath             -- direct source paths
  , drvPlatform  :: Text                      -- e.g. "x86_64-linux"
  , drvBuilder   :: Text                      -- e.g. "/bin/bash"
  , drvArgs      :: [Text]                    -- builder arguments
  , drvEnv       :: Map Text Text             -- environment variables
  }

data DerivationOutput = DerivationOutput
  { outputPath     :: Maybe StorePath         -- Nothing = content-addressed
  , outputHashAlgo :: Maybe Text              -- e.g. "sha256"
  , outputHash     :: Maybe Text              -- fixed-output hash
  }

type StorePath = Text  -- e.g. "/nix/store/abc123...-zlib-1.3"
```

This is the same structure as `nix derivation show` outputs. To build:

```haskell
build :: Derivation -> IO StorePath
build drv = do
  drvPath <- addToStore drv    -- nix-store --add / daemon protocol
  nixBuild drvPath             -- nix-store --realise
```

The key consequence: `callPackage` doesn't return a hypothetical
build plan — it returns something the Nix daemon can **actually build**.
Dependencies in `drvInputDrvs` are real store paths to other `.drv`
files, produced by other `callPackage` invocations.

## §1 The Core Observation

In Nix, a package recipe is a function:

```nix
# openssl.nix
{ zlib, perl }: mkDerivation { ... buildInputs = [ zlib perl ]; ... }
```

`callPackage` fills the arguments from the package set:

```nix
pkgs.openssl = callPackage ./openssl.nix {}
```

In Haskell, if `pkgs` is a type representing the package set, the recipe
has signature:

```haskell
openssl :: forall pkgs -> Derivation
```

But bare `forall pkgs ->` gives no access to the package set's contents.
To reference other packages, you need **constraints** — the dependency
declaration:

```haskell
openssl :: forall pkgs -> (Has "zlib" pkgs, Has "perl" pkgs) => Derivation
```

The Nix formal parameters `{ zlib, perl }` become Haskell constraints
`(Has "zlib" pkgs, Has "perl" pkgs)`. Filling those parameters from the
package set is **instance resolution**.

So `callPackage` IS instance resolution. And the recipe IS a constrained
polymorphic function over `pkgs`.

## §2 The `Package` Class

Since constraints vary per package, the recipe must be a class method.
The class is indexed by the package name:

```haskell
class Package (name :: Symbol) where
  type Deps name (pkgs :: Type) :: Constraint
  type Output name :: Type
  type Output name = AnyDerivation            -- default for normal packages
  callPackage :: forall pkgs -> Deps name pkgs => Output name
```

- `name` identifies the package (type-level string)
- `Deps` is an associated constraint family: given a package name and a
  package set type, it returns the constraint that must be satisfied
- `Output` is the type of what the package produces. Defaults to
  `AnyDerivation` (see below), but can be anything — a function, a
  record, a path. This makes the package set **heterogeneous**: fetchers
  return functions, packages return derivations, utilities return
  whatever they are
- `callPackage` takes a package set type (visible `forall`) and, given
  the dependencies are met, produces `Output name`

### 2.1 `HasDerivation` and `AnyDerivation`

Not every package output is a bare `Derivation`, but most can produce
one. The `HasDerivation` class captures this:

```haskell
class HasDerivation a where
  toDerivation :: a -> Derivation

instance HasDerivation Derivation where
  toDerivation = id
```

The default `Output` is an existentially quantified wrapper — any type
that knows how to produce a `Derivation`:

```haskell
data AnyDerivation = forall a. HasDerivation a => MkAnyDerivation a

instance HasDerivation AnyDerivation where
  toDerivation (MkAnyDerivation x) = toDerivation x
```

A package returning `AnyDerivation` can internally carry any rich type
(with version info, multiple outputs, metadata) while presenting a
uniform interface. Consumers that need a `Derivation` call
`toDerivation`. Consumers that know the concrete type (via `Pkg`, see
§11.5) can access the richer structure.

Infrastructure like stdenv and fetchers override `Output` with specific
types (functions). Normal packages use the default `AnyDerivation`.

## §3 Package Definitions as Instances

### 3.1 Fetchers and Utilities

Not everything in the package set is a derivation. Infrastructure —
fetchers, `stdenv`, build utilities — are **functions or records** that
live in `pkgs` and depend on other packages:

```haskell
instance Package "fetchurl" where
  type Deps "fetchurl" pkgs = Has "curl" pkgs
  type Output "fetchurl" = Text -> Text -> Derivation
  callPackage @pkgs = \url hash -> mkFixedOutputDrv
    (callPackage @"curl" @pkgs) url hash

instance Package "fetchFromGitHub" where
  type Deps "fetchFromGitHub" pkgs = (Has "fetchurl" pkgs, Has "git" pkgs)
  type Output "fetchFromGitHub" = FetchGitHubArgs -> Derivation
  callPackage @pkgs = \args ->
    let fetch = callPackage @"fetchurl" @pkgs
        git   = callPackage @"git" @pkgs
    in fetchGitHubWith fetch git args
```

`stdenv` provides `mkDerivation`. In nixpkgs, `stdenv.mkDerivation` is
how most packages are built — it depends on a C compiler, coreutils,
and other bootstrap tools. Same here:

```haskell
instance Package "stdenv" where
  type Deps "stdenv" pkgs = (Has "gcc" pkgs, Has "coreutils" pkgs, Has "bash" pkgs)
  type Output "stdenv" = MkDerivationArgs -> AnyDerivation
  callPackage @pkgs = \args ->
    let gcc       = callPackage @"gcc" @pkgs
        coreutils = callPackage @"coreutils" @pkgs
        bash      = callPackage @"bash" @pkgs
    in MkAnyDerivation $ buildWithStdenv gcc coreutils bash args
```

`Output "stdenv"` is `MkDerivationArgs -> AnyDerivation` — a builder
function, not a derivation. The `MkAnyDerivation` wrapping happens
once here; every package that uses `mkDerivation` gets `AnyDerivation`
for free. `mkDerivation` is no longer a magic top-level function; it's
resolved from `pkgs` like everything else.

The package set is heterogeneous — each entry has its own type, determined
by `Output`.

In nixpkgs, `pkgs.fetchurl` depends on `pkgs.curl`. Here, `Deps "fetchurl"`
requires `Has "curl" pkgs`. Same dependency, statically checked.

### 3.2 Normal Packages

Normal packages use the default `Output` (= `AnyDerivation`). Since
`mkDerivation` from stdenv already returns `AnyDerivation`, the types
line up directly. Dependencies in `buildInputs` go through
`toDerivation`:

```haskell
instance Package "zlib" where
  type Deps "zlib" pkgs = (Has "stdenv" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let mkDerivation = callPackage @"stdenv" @pkgs
    in mkDerivation MkDerivationArgs
      { name = "zlib", version = "1.3"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://zlib.net/zlib-1.3.tar.gz"
          "sha256-abc..."
      , buildInputs = []
      }

instance Package "openssl" where
  type Deps "openssl" pkgs = (Has "stdenv" pkgs, Has "zlib" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let mkDerivation = callPackage @"stdenv" @pkgs
    in mkDerivation MkDerivationArgs
      { name = "openssl", version = "3.1"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://openssl.org/source/openssl-3.1.tar.gz"
          "sha256-def..."
      , buildInputs = [toDerivation $ callPackage @"zlib" @pkgs]
      }

instance Package "curl" where
  type Deps "curl" pkgs =
    (Has "stdenv" pkgs, Has "openssl" pkgs, Has "zlib" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let mkDerivation = callPackage @"stdenv" @pkgs
    in mkDerivation MkDerivationArgs
      { name = "curl", version = "8.5"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://curl.se/download/curl-8.5.tar.gz"
          "sha256-ghi..."
      , buildInputs =
          [ toDerivation $ callPackage @"openssl" @pkgs
          , toDerivation $ callPackage @"zlib" @pkgs
          ]
      }
```

`callPackage @"fetchurl" @pkgs` returns a function (not a derivation).
Applying it to url + hash produces a fixed-output derivation. The
fetcher's dependency on curl is resolved through the same `pkgs`.

### 3.3 The Dependency Chain

Everything flows through `pkgs`:

```
openssl
  ├── src: (callPackage @"fetchurl" @pkgs) url hash
  │         └── fetchurl depends on: callPackage @"curl" @pkgs
  │             └── curl depends on: ... (bootstrap)
  └── buildInputs: [callPackage @"zlib" @pkgs]
                     └── zlib depends on: callPackage @"fetchurl" @pkgs
                         └── fetchurl depends on: ... (same curl)
```

In practice, the bootstrap breaks the circularity: the fetcher used
during bootstrap is a pre-built binary, not the curl from `pkgs`.
See encoding §5 for how staged bootstrap resolves this.

## §4 The `Has` Class: Package Set Membership

`Package` defines recipes. A separate class records which packages are
available in a given package set:

```haskell
class Package name => Has (name :: Symbol) (pkgs :: Type) where
  -- possibly empty: mere membership witness
```

The superclass `Package name` ensures you can only claim membership for
packages that have a recipe. An instance `Has "zlib" Nixpkgs` says
"zlib is available in Nixpkgs."

## §5 Package Set Construction

A concrete package set declares which packages it contains — both
packages and fetchers:

```haskell
data Nixpkgs

-- Infrastructure (stdenv, fetchers, utilities)
instance Has "stdenv"          Nixpkgs
instance Has "gcc"             Nixpkgs
instance Has "coreutils"       Nixpkgs
instance Has "bash"            Nixpkgs
instance Has "curl"            Nixpkgs
instance Has "git"             Nixpkgs
instance Has "fetchurl"        Nixpkgs
instance Has "fetchFromGitHub" Nixpkgs

-- Packages
instance Has "zlib"    Nixpkgs
instance Has "openssl" Nixpkgs
instance Has "python"  Nixpkgs
```

Usage:

```haskell
main = print (callPackage @"python" @Nixpkgs)
```

GHC resolves `Deps "python" Nixpkgs`, which requires `Has "openssl" Nixpkgs`,
`Has "zlib" Nixpkgs`, and `Has "fetchurl" Nixpkgs`. All declared, so
resolution proceeds. The fetcher dependency is tracked just like any
package dependency.

If you declare `Has "curl" Nixpkgs` but not `Has "openssl" Nixpkgs`,
GHC reports: `No instance for Has "openssl" Nixpkgs` — unsatisfied
dependency, caught at compile time.

## §6 Separation of Concerns

The encoding separates three things:

| Concept | Mechanism | Nix analogue |
|---|---|---|
| Recipe | `Package` instance | `.nix` file |
| Dependencies | `Deps` associated constraint | Function formal params |
| Output type | `Output` associated type | Derivation, function, attrset, ... |
| Package set | `Has` instances for a type | `callPackage` wiring in `all-packages.nix` |

A recipe is defined once (one `Package` instance per package name) and
is package-set-independent. A package set chooses which recipes to
include by declaring `Has` instances. This mirrors Nix's separation
between `pkgs/by-name/` (recipes) and `all-packages.nix` (wiring).

The package set is heterogeneous: `callPackage @"zlib" @Nixpkgs` returns
a `Derivation`, while `callPackage @"fetchurl" @Nixpkgs` returns a
`Text -> Text -> Derivation`. The type of each entry is determined by
`Output name`, known at compile time.

## §7 Resolution Flow

When GHC evaluates `callPackage @"python" @Nixpkgs`:

```
callPackage @"python" @Nixpkgs
  requires: Deps "python" Nixpkgs
          = (Has "openssl" Nixpkgs, Has "zlib" Nixpkgs)
  ✓ Has "openssl" Nixpkgs — declared
  ✓ Has "zlib" Nixpkgs — declared
  body calls:
    callPackage @"openssl" @Nixpkgs
      requires: Deps "openssl" Nixpkgs = Has "zlib" Nixpkgs
      ✓ Has "zlib" Nixpkgs — declared
      body calls:
        callPackage @"zlib" @Nixpkgs
          requires: Deps "zlib" Nixpkgs = ()
          ✓ trivially satisfied
          → mkDrv "zlib" "1.3" []
      → mkDrv "openssl" "3.1" [<zlib>]
    callPackage @"zlib" @Nixpkgs
      → (shared: same dictionary as above)
  → mkDrv "python" "3.12" [<openssl>, <zlib>]
```

Sharing: `callPackage @"zlib" @Nixpkgs` produces one dictionary, reused
by both openssl and python.

## §8 What This Buys

**`callPackage` is a first-class concept.** In the multi-class encoding
(encoding §1), `callPackage` is implicit — it's the act of writing an
instance. Here, it's an actual method you can call, with a uniform
signature across all packages.

**Recipes are package-set-independent.** The `Package` instance for
openssl doesn't mention `Nixpkgs`. It works for any `pkgs` that
satisfies `Has "zlib" pkgs`. This is exactly how Nix recipes are
written — they don't know which package set will call them.

**The package set is just a list of `Has` assertions.** No method
bodies, no build logic — just declarations of what's available. The
recipes are elsewhere. This mirrors `all-packages.nix` being mostly
`callPackage` invocations with no build logic.

**Dependencies are inspectable at the type level.** `Deps "openssl" pkgs`
is a type family you can reduce, query, and compose. "What does openssl
need?" is a type-level question with a type-level answer.

## §9 What This Costs

**No per-package superclass.** There is one `Package` class, so there
is one superclass context (namely `Package name => Has name pkgs`). You
cannot express "anything that has openssl automatically has zlib" — that
requires a per-package class declaration with per-package superclass.
Propagated dependencies (encoding §2) are lost.

**No per-package methods.** The class has one method (`callPackage`).
Multiple outputs and typed interfaces need a richer `Output` type
(e.g., a record with `lib`, `dev`, `doc` fields).

**Overlays require separate design.** With one class and `Symbol`
indexing, you cannot have two `Package "openssl"` instances (different
versions). Overlays need a different mechanism — perhaps a `pkgs`-indexed
version of `Package`, or a separate overlay algebra.

These are acknowledged trade-offs. This document focuses on the
`callPackage` core; extensions can recover the lost features.

## §10 Comparison with Nix

```
Nix                              Haskell (this encoding)
──────────────────────────────── ────────────────────────────────
{ stdenv, zlib, perl }:          type Deps "openssl" pkgs =
  stdenv.mkDerivation { ... }      (Has "stdenv" pkgs, Has "zlib" pkgs, ...)
                                 callPackage @pkgs =
                                   let mk = callPackage @"stdenv" @pkgs
                                   in mk MkDerivationArgs { ... }

callPackage ./openssl.nix {}     callPackage @"openssl" @Nixpkgs

pkgs = self: { ... }             data Nixpkgs
                                 instance Has "stdenv" Nixpkgs
                                 instance Has "zlib" Nixpkgs
                                 instance Has "fetchurl" Nixpkgs
                                 ...

pkgs.stdenv.mkDerivation         callPackage @"stdenv" @Nixpkgs
  (a function, not a drv)          :: MkDerivationArgs -> Derivation

pkgs.fetchurl                    callPackage @"fetchurl" @Nixpkgs
  (a function, not a drv)          :: Text -> Text -> Derivation

builtins.functionArgs            Deps "openssl" pkgs
                                 (type family, inspectable)

missing argument error           No instance for Has "foo" Nixpkgs
(at eval time)                   (at compile time)
```

The structural correspondence: Nix's dynamic function argument
inspection becomes Haskell's static constraint resolution. What Nix
checks at evaluation time, GHC checks at compile time.

## §11 Extensions (Sketches)

### 11.1 Multiple Outputs

`Output` (now in the core class) handles this directly:

```haskell
data OpensslOutputs = OpensslOutputs
  { opensslLib :: Derivation
  , opensslDev :: Derivation
  , opensslDoc :: Derivation
  }

instance Package "openssl" where
  type Deps "openssl" pkgs = (Has "zlib" pkgs, Has "fetchurl" pkgs)
  type Output "openssl" = OpensslOutputs
  callPackage @pkgs = OpensslOutputs
    { opensslLib = ...
    , opensslDev = ...
    , opensslDoc = ...
    }
```

Consumers pick which output: `opensslDev (callPackage @"openssl" @pkgs)`.
Unused outputs are never forced (laziness).

### 11.2 Version Information

```haskell
class Package (name :: Symbol) where
  type Deps name pkgs :: Constraint
  type Ver name :: Version                  -- NEW
  callPackage :: forall pkgs -> Deps name pkgs => Derivation

instance Package "openssl" where
  type Ver "openssl" = 'V 3 1 0
  ...
```

Version predicates in `Deps`:

```haskell
instance Package "curl" where
  type Deps "curl" pkgs =
    ( Has "openssl" pkgs
    , Requires (Ver "openssl") ('AtLeast ('V 3 0 0))
    , Has "zlib" pkgs
    )
  ...
```

### 11.3 Propagated Dependencies

Without per-package superclass, propagation can be encoded via a
type family:

```haskell
type family Propagated (name :: Symbol) (pkgs :: Type) :: Constraint
type instance Propagated "openssl" pkgs = Has "zlib" pkgs
type instance Propagated "curl" pkgs =
  (Has "openssl" pkgs, Propagated "openssl" pkgs)
```

A consumer of openssl gets zlib by including `Propagated "openssl" pkgs`
in their constraint. This is explicit (not automatic like superclass)
but composes.

### 11.4 Minimal Package Sets

```haskell
-- Only include what's needed. GHC tells you what's missing.
data Minimal

instance Has "zlib" Minimal
-- instance Has "openssl" Minimal  -- omitted

-- callPackage @"openssl" @Minimal
-- ERROR: No instance for Has "zlib" Minimal
-- Wait — zlib IS there. Let's try:

-- callPackage @"curl" @Minimal
-- ERROR: No instance for Has "openssl" Minimal
-- Tells you exactly what's missing.
```

### 11.5 Per-Package Data Types (`Pkg` Data Family)

The core spec is `Package`, `Has`, and `HasDerivation`/`AnyDerivation`.
Everything below is a suggested ergonomic pattern, not part of the spec.

In Nix, `{ openssl, zlib, perl }:` binds named variables. In this
encoding, you write `callPackage @"openssl" @pkgs` everywhere — verbose,
and no structured access to package-specific metadata (headers path,
version string, sub-libraries).

An open data family gives each package an optional record type:

```haskell
data family Pkg (name :: Symbol)
```

Packages that benefit from structured access define an instance:

```haskell
data instance Pkg "openssl" = Openssl
  { opensslDrv     :: Derivation
  , opensslHeaders :: StorePath
  , opensslVersion :: Text
  }
```

A suggested helper class constructs the record. `pkgMeta` takes the
`callPackage` result as input — so `Pkg` captures both the **output**
(the derivation itself) and **input-side** metadata (version, paths,
configuration):

```haskell
class Package name => HasPkg (name :: Symbol) where
  pkgMeta :: forall pkgs -> Has name pkgs => Output name -> Pkg name
```

The output of `callPackage` flows into `pkgMeta`, which enriches it:

```haskell
instance HasPkg "openssl" where
  pkgMeta @pkgs output =
    let drv = toDerivation output
    in Openssl
      { opensslDrv     = drv
      , opensslHeaders = drvOutPath drv <> "/include"
      , opensslVersion = "3.1"
      }
```

`pkgMeta` receives the `AnyDerivation` from `callPackage`, extracts
the `Derivation` via `toDerivation`, and bundles it with metadata.
The `Pkg` record has both: the build result (output info) and version,
paths, flags (input info).

`Pkg` instances are themselves `HasDerivation`, closing the loop:

```haskell
instance HasDerivation (Pkg "openssl") where
  toDerivation = opensslDrv
```

The biggest payoff: `stdenv` itself gets a `Pkg` instance. Since almost
every package needs `mkDerivation`, putting it in the `Pkg` record means
RecordWildCards brings it into scope automatically:

```haskell
data instance Pkg "stdenv" = Stdenv
  { mkDerivation :: MkDerivationArgs -> AnyDerivation
  , cc           :: StorePath
  }

instance HasPkg "stdenv" where
  pkgMeta @pkgs build = Stdenv
    { mkDerivation = build
    , cc = drvOutPath (toDerivation $ callPackage @"gcc" @pkgs) <> "/bin/gcc"
    }
```

Now recipe bodies become concise — destructure dependencies with
RecordWildCards, and `mkDerivation` appears as a local function:

```haskell
instance Package "curl" where
  type Deps "curl" pkgs =
    (Has "stdenv" pkgs, Has "openssl" pkgs, Has "zlib" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let Stdenv{..}  = pkgMeta @"stdenv"  @pkgs (callPackage @"stdenv" @pkgs)
        Openssl{..} = pkgMeta @"openssl" @pkgs (callPackage @"openssl" @pkgs)
    -- mkDerivation, cc, opensslDrv, opensslHeaders, opensslVersion in scope
    in mkDerivation MkDerivationArgs
      { name = "curl", version = "8.5"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://curl.se/download/curl-8.5.tar.gz"
          "sha256-ghi..."
      , buildInputs = [opensslDrv, toDerivation $ callPackage @"zlib" @pkgs]
      , configureFlags =
          ["--with-ssl-headers=" <> opensslHeaders]
      }
```

Compare with the non-`Pkg` version from §3.2, where every dependency
is `callPackage @"..." @pkgs` and `mkDerivation` must be explicitly
bound from stdenv. The `Pkg` pattern recovers the feel of Nix's
`{ stdenv, openssl, zlib, fetchurl }:` destructuring.

Key properties:

- **Optional.** Packages without a `Pkg` instance are used via
  `callPackage` directly. The data family is open — define instances
  only when the ergonomics matter.
- **Both input and output.** `pkgMeta` takes the `callPackage` result
  (output info) and enriches it with metadata (input info). The `Pkg`
  record captures both sides of the build process.
- **`HasDerivation` bridge.** `Pkg` instances implement `HasDerivation`,
  so they can flow into `buildInputs` via `toDerivation`. A `Pkg`
  value carries rich structure but can always be projected to a
  `Derivation`.
- **Proof-carrying.** A value of type `Pkg "openssl"` witnesses that
  openssl was resolved. The constructor name `Openssl` can be used
  in pattern matching to make the provenance of each field explicit.
- **Namespaced fields.** By convention, field names are prefixed with
  the package name (`opensslHeaders`, not `headers`). With
  `RecordWildCards`, these come into scope without clashing.

### 11.6 Phantom-Tagged Derivations

A bare `Derivation` is untyped — nothing stops you from passing a gcc
derivation where zlib is expected. A phantom type tags derivations with
their package name:

```haskell
newtype Drv (name :: Symbol) = Drv { unDrv :: Derivation }

instance HasDerivation (Drv name) where
  toDerivation = unDrv
```

If the default `Output` is `Drv name` instead of `AnyDerivation`:

```haskell
class Package (name :: Symbol) where
  type Deps name (pkgs :: Type) :: Constraint
  type Output name :: Type
  type Output name = Drv name               -- phantom-tagged by default
  callPackage :: forall pkgs -> Deps name pkgs => Output name
```

then `callPackage @"zlib" @pkgs :: Drv "zlib"` and
`callPackage @"openssl" @pkgs :: Drv "openssl"` are distinct types.
Swapping them is a compile error:

```haskell
-- Type error: expected Drv "openssl", got Drv "zlib"
configure :: Drv "openssl" -> Drv "zlib" -> IO ()
configure openssl zlib = ...

bad = configure (callPackage @"zlib" @pkgs) (callPackage @"openssl" @pkgs)
--               ^^^^^^^^^^^^^^^^^^^^^^^^^ Drv "zlib" ≠ Drv "openssl"
```

The tag is erased when entering `buildInputs` via `toDerivation`:

```haskell
buildInputs =
  [ toDerivation $ callPackage @"openssl" @pkgs  -- Drv "openssl" → Derivation
  , toDerivation $ callPackage @"zlib" @pkgs     -- Drv "zlib"    → Derivation
  ]
```

The type hierarchy from most to least specific:

```
Drv "zlib"      -- tagged: provenance known at the type level
AnyDerivation   -- existential: some HasDerivation, tag erased
Derivation      -- raw: the .drv the Nix daemon builds
```

`Drv name` and `AnyDerivation` serve different roles. `Drv name` is the
default for normal packages — it prevents accidental swaps in typed
interfaces. `AnyDerivation` is for heterogeneous collections where the
concrete package is unknown (e.g., `buildInputs` could be
`[AnyDerivation]` instead of `[Derivation]`).

Infrastructure overrides `Output` as before:

```haskell
instance Package "stdenv" where
  type Output "stdenv" = MkDerivationArgs -> AnyDerivation  -- function, not Drv
instance Package "fetchurl" where
  type Output "fetchurl" = Text -> Text -> Derivation       -- function, not Drv
```

`Pkg` instances also carry the tag naturally — `Pkg "openssl"` already
witnesses provenance (§11.5), and its `HasDerivation` instance bridges
to `Derivation` when needed.
