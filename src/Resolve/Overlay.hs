-- | Overlays: environment extension with lazy re-computation.
--
-- An overlay takes (final, prev) and produces replacement entries.
-- The fixed point is re-tied so that dependents automatically
-- pick up the new versions.
module Resolve.Overlay where

import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map

import Resolve.Syntax
import Resolve.Resolve

-- | An overlay is a function from (final, prev) to replacement entries.
-- 'final' is the post-overlay package set (self-reference through the fix).
-- 'prev' is the pre-overlay package set.
type Overlay = Map PkgName Evidence -> Map PkgName Evidence -> Map PkgName Evidence

-- | Apply an overlay to a base package set.
--
-- Σ' = fix(σ ↦ Σ_base ⊕ O(σ, Σ_base))
--
-- The recursive let IS the fixed point. 'final' references itself.
-- Entries from the overlay shadow the base (Map.union is left-biased).
applyOverlay :: Overlay -> Map PkgName Evidence -> Map PkgName Evidence
applyOverlay overlay base =
  let final = Map.union (overlay final base) base
  in final

-- | Apply multiple overlays in sequence.
-- Each overlay sees the result of applying all previous overlays as 'prev'.
applyOverlays :: [Overlay] -> Map PkgName Evidence -> Map PkgName Evidence
applyOverlays [] base = base
applyOverlays (o:os) base = applyOverlay o (applyOverlays os base)

-- | Build a simple overlay that replaces specific packages.
-- The replacement function receives the final package set for
-- forward references (like Nix's 'final.zlib').
mkOverlay :: [(PkgName, Map PkgName Evidence -> Evidence)] -> Overlay
mkOverlay replacements final _prev =
  Map.fromList [(name, f final) | (name, f) <- replacements]

-- | Build an overlay from an extended environment.
-- This re-resolves the specified packages using new declarations.
overlayFromEnv :: SelectPolicy -> Env -> [PkgName] -> Overlay
overlayFromEnv sel newEnv names final _prev =
  Map.fromList
    [ (name, resolveWithFinal name)
    | name <- names
    ]
  where
    resolveWithFinal name =
      case sel newEnv name AnyVersion of
        Left err -> error (show err)
        Right decl ->
          case resolve sel newEnv final (declReqs decl) of
            Left err -> error (show err)
            Right depEvidence ->
              Inst name (declVer decl) (flattenEvidence depEvidence)

    flattenEvidence Unit = []
    flattenEvidence (Pair e1 e2) = flattenEvidence e1 ++ flattenEvidence e2
    flattenEvidence e = [e]
