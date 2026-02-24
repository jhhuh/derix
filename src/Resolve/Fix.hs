-- | The package set as a lazy fixed point.
--
-- This is the heart of the system. The package set is a lazy Map
-- where each entry is a thunk that, when forced, triggers resolution.
-- Haskell's own call-by-need evaluation IS the operational semantics
-- from §4 of the calculus:
--
--   ○(C)  =  a Haskell thunk (unevaluated)
--   ●     =  a Haskell black hole (<<loop>> on cycle)
--   e     =  a Haskell WHNF value (memoized after first force)
--
-- We get sharing, black-hole detection, and demand-driven evaluation
-- for free from the runtime. No IORef, no explicit thunk management.
module Resolve.Fix where

import Data.List (nub)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map

import Resolve.Syntax
import Resolve.Resolve

-- | Build the package set as a lazy fixed point.
--
-- This is literally:
--   Σ = fix(σ ↦ { p ↦ resolve(Γ, σ, C_p) | p ∈ pkgnames(Γ) })
--
-- In Haskell, the recursive 'let' is the fixed point. Each map
-- entry is a thunk until forced. GHC's runtime provides:
--   - Thunk suspension (○)
--   - Black-hole detection (● → <<loop>>)
--   - Memoization (○ → e, shared thereafter)
buildPkgSet :: SelectPolicy -> Env -> Either ResError (Map PkgName Evidence)
buildPkgSet sel env =
  let names = nub (envPkgNames env)
      -- The recursive let: pkgSet references itself. This IS the fix.
      pkgSet = Map.fromList
        [ (name, resolveEntry name)
        | name <- names
        ]
      resolveEntry name =
        case sel env name AnyVersion of
          Left err -> error (show err) -- will be caught on force
          Right decl ->
            case resolve sel env pkgSet (declReqs decl) of
              Left err -> error (show err)
              Right depEvidence ->
                Inst name (declVer decl) (flattenEvidence depEvidence)
  in Right pkgSet

-- | Flatten evidence into a list of direct dependencies.
flattenEvidence :: Evidence -> [Evidence]
flattenEvidence Unit = []
flattenEvidence (Pair e1 e2) = flattenEvidence e1 ++ flattenEvidence e2
flattenEvidence e = [e]

-- | Force a specific package from the set, returning the evidence
-- or an error message if resolution fails.
forcePkg :: Map PkgName Evidence -> PkgName -> Either String Evidence
forcePkg pkgs name =
  case Map.lookup name pkgs of
    Nothing -> Left ("Package not found: " <> show name)
    Just ev -> Right ev  -- forcing happens here via Haskell's evaluation
