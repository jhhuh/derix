# Derix: The `callPackage` Encoding

Starting from scratch. If `pkgs` is a type, what is `callPackage`?

Building is delegated to the Nix daemon — `Drv` is the concrete `.drv`
file, `Derivation name` is the richer Haskell-side wrapper.

## §0 `Drv`: The Store Derivation

A `Drv` is exactly what the Nix daemon builds — a `.drv` file in the
Nix store. The Haskell type mirrors the ATerm format:

```haskell
data Drv = Drv
  { drvOutputs   :: Map Text DrvOutput
  , drvInputDrvs :: Map StorePath (Set Text)  -- drv path → outputs needed
  , drvInputSrcs :: Set StorePath             -- direct source paths
  , drvPlatform  :: Text                      -- e.g. "x86_64-linux"
  , drvBuilder   :: Text                      -- e.g. "/bin/bash"
  , drvArgs      :: [Text]                    -- builder arguments
  , drvEnv       :: Map Text Text             -- environment variables
  }

data DrvOutput = DrvOutput
  { outputPath     :: Maybe StorePath         -- Nothing = content-addressed
  , outputHashAlgo :: Maybe Text              -- e.g. "sha256"
  , outputHash     :: Maybe Text              -- fixed-output hash
  }

type StorePath = Text  -- e.g. "/nix/store/abc123...-zlib-1.3"
```

This is the same structure as `nix derivation show` outputs. To build:

```haskell
build :: Drv -> IO StorePath
build drv = do
  drvPath <- addToStore drv    -- nix-store --add / daemon protocol
  nixBuild drvPath             -- nix-store --realise
```

The naming follows Nix: a `.drv` file contains only the essential
build specification (outputs, inputs, builder, environment). The richer
`Derivation name` type (§2.1) wraps a `Drv` with additional
Haskell-side information. Dependencies in `drvInputDrvs` are real
store paths to other `.drv` files, produced by other `callPackage`
invocations.

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
openssl :: forall pkgs -> Derivation "openssl"
```

But bare `forall pkgs ->` gives no access to the package set's contents.
To reference other packages, you need **constraints** — the dependency
declaration:

```haskell
openssl :: forall pkgs -> (Has "zlib" pkgs, Has "perl" pkgs) => Derivation "openssl"
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
  type Output name = Derivation name          -- default for normal packages
  callPackage :: forall pkgs -> Deps name pkgs => Output name
```

- `name` identifies the package (type-level string)
- `Deps` is an associated constraint family: given a package name and a
  package set type, it returns the constraint that must be satisfied
- `Output` is the type of what the package produces. Defaults to
  `Derivation name` (see below), but can be anything — a function, a
  record, a path. This makes the package set **heterogeneous**: fetchers
  return functions, packages return derivations, utilities return
  whatever they are
- `callPackage` takes a package set type (visible `forall`) and, given
  the dependencies are met, produces `Output name`

### 2.1 `HasDrv`, `Derivation`

Not every package output is a bare `Drv`, but most can produce one.
The `HasDrv` class captures this:

```haskell
class HasDrv a where
  toDrv :: a -> Drv

instance HasDrv Drv where
  toDrv = id
```

The default `Output` is `Derivation name` — existentially quantified
and phantom-tagged with the package name:

```haskell
data Derivation (name :: Symbol) = forall a. HasDrv a => Derivation a

instance HasDrv (Derivation name) where
  toDrv (Derivation x) = toDrv x
```

`Derivation name` combines two features:

- **Phantom tag.** `Derivation "zlib"` and `Derivation "openssl"` are
  distinct types. Accidentally swapping them is a compile error.
- **Existential.** The wrapped `a` can carry any rich type (version
  info, multiple outputs, metadata) as long as it has a `HasDrv`
  instance. The extra information survives inside the existential.

Consumers that need a raw `Drv` (e.g., for `buildInputs`) call `toDrv`.
Infrastructure like stdenv and fetchers override `Output` with specific
types (functions). Normal packages use the default `Derivation name`.

## §3 Package Definitions as Instances

### 3.1 Fetchers and Utilities

Not everything in the package set is a derivation. Infrastructure —
fetchers, `stdenv`, build utilities — are **functions or records** that
live in `pkgs` and depend on other packages:

```haskell
instance Package "fetchurl" where
  type Deps "fetchurl" pkgs = Has "curl" pkgs
  type Output "fetchurl" = Text -> Text -> Drv
  callPackage @pkgs = \url hash -> mkFixedOutputDrv
    (toDrv $ callPackage @"curl" @pkgs) url hash

instance Package "fetchFromGitHub" where
  type Deps "fetchFromGitHub" pkgs = (Has "fetchurl" pkgs, Has "git" pkgs)
  type Output "fetchFromGitHub" = FetchGitHubArgs -> Drv
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
  type Output "stdenv" = MkDerivationArgs -> Drv
  callPackage @pkgs = \args ->
    let gcc       = callPackage @"gcc" @pkgs
        coreutils = callPackage @"coreutils" @pkgs
        bash      = callPackage @"bash" @pkgs
    in buildWithStdenv gcc coreutils bash args
```

`Output "stdenv"` is `MkDerivationArgs -> Drv` — a builder function
that produces a raw `Drv`. Normal packages wrap the result in
`Derivation` (§3.2), tagging it with their name. `mkDerivation` is no
longer a magic top-level function; it's resolved from `pkgs` like
everything else.

The package set is heterogeneous — each entry has its own type, determined
by `Output`.

In nixpkgs, `pkgs.fetchurl` depends on `pkgs.curl`. Here, `Deps "fetchurl"`
requires `Has "curl" pkgs`. Same dependency, statically checked.

### 3.2 Normal Packages

Normal packages use the default `Output` (= `Derivation name`). The
`mkDerivation` from stdenv produces a raw `Drv`; the package wraps it
in `Derivation`, tagging it with the package name. Dependencies in
`buildInputs` are `[Drv]`, extracted via `toDrv`:

```haskell
instance Package "zlib" where
  type Deps "zlib" pkgs = (Has "stdenv" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let mkDrv = callPackage @"stdenv" @pkgs
    in Derivation $ mkDrv MkDerivationArgs
      { name = "zlib", version = "1.3"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://zlib.net/zlib-1.3.tar.gz"
          "sha256-abc..."
      , buildInputs = []
      }

instance Package "openssl" where
  type Deps "openssl" pkgs = (Has "stdenv" pkgs, Has "zlib" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let mkDrv = callPackage @"stdenv" @pkgs
    in Derivation $ mkDrv MkDerivationArgs
      { name = "openssl", version = "3.1"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://openssl.org/source/openssl-3.1.tar.gz"
          "sha256-def..."
      , buildInputs = [toDrv $ callPackage @"zlib" @pkgs]
      }

instance Package "curl" where
  type Deps "curl" pkgs =
    (Has "stdenv" pkgs, Has "openssl" pkgs, Has "zlib" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let mkDrv = callPackage @"stdenv" @pkgs
    in Derivation $ mkDrv MkDerivationArgs
      { name = "curl", version = "8.5"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://curl.se/download/curl-8.5.tar.gz"
          "sha256-ghi..."
      , buildInputs =
          [ toDrv $ callPackage @"openssl" @pkgs
          , toDrv $ callPackage @"zlib" @pkgs
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
a `Derivation "zlib"`, while `callPackage @"fetchurl" @Nixpkgs` returns
a `Text -> Text -> Drv`. The type of each entry is determined by
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
                                   let mkDrv = callPackage @"stdenv" @pkgs
                                   in Derivation $ mkDrv MkDerivationArgs { ... }

callPackage ./openssl.nix {}     callPackage @"openssl" @Nixpkgs
                                   :: Derivation "openssl"

pkgs = self: { ... }             data Nixpkgs
                                 instance Has "stdenv" Nixpkgs
                                 instance Has "zlib" Nixpkgs
                                 instance Has "fetchurl" Nixpkgs
                                 ...

pkgs.stdenv.mkDerivation         callPackage @"stdenv" @Nixpkgs
  (a function, not a drv)          :: MkDerivationArgs -> Drv

pkgs.fetchurl                    callPackage @"fetchurl" @Nixpkgs
  (a function, not a drv)          :: Text -> Text -> Drv

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
  { lib :: Drv
  , dev :: Drv
  , doc :: Drv
  }

instance Package "openssl" where
  type Deps "openssl" pkgs = (Has "zlib" pkgs, Has "fetchurl" pkgs)
  type Output "openssl" = OpensslOutputs
  callPackage @pkgs = OpensslOutputs
    { lib = ...
    , dev = ...
    , doc = ...
    }
```

Consumers pick which output: `dev (callPackage @"openssl" @pkgs)`.
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

The core spec is `Package`, `Has`, `HasDrv`, and `Derivation name`.
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
  { drv     :: Drv
  , headers :: StorePath
  , version :: Text
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
    let d = toDrv output
    in Openssl
      { drv     = d
      , headers = drvOutPath d <> "/include"
      , version = "3.1"
      }
```

`pkgMeta` receives the `Derivation "openssl"` from `callPackage`,
extracts the `Drv` via `toDrv`, and bundles it with metadata.
The `Pkg` record has both: the build result (output info) and version,
paths, flags (input info).

`Pkg` instances are themselves `HasDrv`, closing the loop:

```haskell
instance HasDrv (Pkg "openssl") where
  toDrv = drv
```

The biggest payoff: `stdenv` itself gets a `Pkg` instance. Since almost
every package needs `mkDerivation`, putting it in the `Pkg` record means
RecordWildCards brings it into scope automatically:

```haskell
data instance Pkg "stdenv" = Stdenv
  { mkDerivation :: MkDerivationArgs -> Drv
  , cc           :: StorePath
  }

instance HasPkg "stdenv" where
  pkgMeta @pkgs build = Stdenv
    { mkDerivation = build
    , cc = drvOutPath (toDrv $ callPackage @"gcc" @pkgs) <> "/bin/gcc"
    }
```

Now recipe bodies become concise — destructure dependencies with
RecordWildCards, and `mkDerivation` appears as a local function.
Since Haskell's `let` bindings are recursive, a package can also
bind its own `pkgMeta` — useful when the metadata depends on the
build result:

```haskell
instance Package "curl" where
  type Deps "curl" pkgs =
    (Has "stdenv" pkgs, Has "openssl" pkgs, Has "zlib" pkgs, Has "fetchurl" pkgs)
  callPackage @pkgs =
    let Stdenv{..}  = pkgMeta @"stdenv"  @pkgs (callPackage @"stdenv" @pkgs)
        Openssl{..} = pkgMeta @"openssl" @pkgs (callPackage @"openssl" @pkgs)
        self        = pkgMeta @"curl"    @pkgs (callPackage @"curl" @pkgs)
    -- mkDerivation, cc, drv, headers, version, self all in scope
    in Derivation $ mkDerivation MkDerivationArgs
      { name = "curl", version = "8.5"
      , src = (callPackage @"fetchurl" @pkgs)
          "https://curl.se/download/curl-8.5.tar.gz"
          "sha256-ghi..."
      , buildInputs = [drv, toDrv $ callPackage @"zlib" @pkgs]
      , configureFlags =
          ["--with-ssl-headers=" <> headers]
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
- **Self-referential.** Recursive `let` allows a package to bind its
  own `pkgMeta self`, which Haskell's laziness evaluates on demand.
- **`HasDrv` bridge.** `Pkg` instances implement `HasDrv`, so they
  can flow into `buildInputs` via `toDrv`. A `Pkg` value carries
  rich structure but can always be projected to a `Drv`.
- **Proof-carrying.** A value of type `Pkg "openssl"` witnesses that
  openssl was resolved. The constructor name `Openssl` can be used
  in pattern matching to make the provenance of each field explicit.

### 11.6 `Derivation` as Both Tag and Carrier

(Now part of the core design — see §2.1.)

`Derivation name` unifies two features that were previously separate:

- **Phantom tag** (prevents swapping `Derivation "gcc"` for
  `Derivation "zlib"` at compile time — zero runtime cost)
- **Existential carrier** (wraps any `HasDrv a`, carrying extra
  information beyond the raw `Drv`)

```haskell
-- Compile error: Derivation "zlib" ≠ Derivation "openssl"
bad = configure (callPackage @"zlib" @pkgs) (callPackage @"openssl" @pkgs)
```

The tag is erased when entering `buildInputs` via `toDrv`:

```haskell
buildInputs =
  [ toDrv $ callPackage @"openssl" @pkgs  -- Derivation "openssl" → Drv
  , toDrv $ callPackage @"zlib" @pkgs     -- Derivation "zlib"    → Drv
  ]
```

Two-level type hierarchy:

```
Derivation "zlib"   -- tagged + existential: provenance known, extra info inside
Drv                 -- raw: the .drv file the Nix daemon builds
```
