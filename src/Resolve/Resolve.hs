-- | Constraint resolution (proof search).
--
-- Implements the [INST], [CONJ], and [TRIV] rules from the calculus.
-- Resolution is demand-driven: it only forces package set entries
-- that are actually needed (laziness comes from Haskell itself).
module Resolve.Resolve where

import Data.List (sortBy)
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Ord (Down(..))
import Data.Text (Text)

import Resolve.Syntax

-- | Resolution errors.
data ResError
  = Unsatisfiable PkgName VerPred
  | Ambiguous PkgName VerPred [Decl]
  | VersionMismatch PkgName Version VerPred
  deriving (Show)

-- §7.2 Selection Policies

-- | A selection policy picks one declaration from the candidates.
type SelectPolicy = Env -> PkgName -> VerPred -> Either ResError Decl

-- | Policy 2: most-specific (highest version satisfying the predicate).
selectHighest :: SelectPolicy
selectHighest env name pred =
  case findMatching env name pred of
    []  -> Left (Unsatisfiable name pred)
    [d] -> Right d
    ds  -> Right (head (sortBy (\a b -> compare (Down (declVer a)) (Down (declVer b))) ds))

-- §3 Resolution

-- | Resolve a constraint against the environment, looking up
-- dependencies through the package set.
--
-- The package set is a lazy Map — looking up an entry forces
-- its thunk, which triggers recursive resolution. This IS the
-- call-by-need operational semantics from §4.
resolve :: SelectPolicy -> Env -> Map PkgName Evidence -> Constraint -> Either ResError Evidence
resolve _ _ _ Trivial = Right Unit
resolve sel env pkgs (And c1 c2) = do
  e1 <- resolve sel env pkgs c1
  e2 <- resolve sel env pkgs c2
  Right (Pair e1 e2)
resolve sel env pkgs (Pkg name pred) =
  -- Look up in the package set (lazy! forces the thunk if needed)
  case Map.lookup name pkgs of
    Just ev
      | Just v <- evidenceVersion ev
      , satisfies v pred -> Right ev
      | otherwise -> Left (VersionMismatch name (maybe (Version 0 0 0) id (evidenceVersion ev)) pred)
    Nothing -> Left (Unsatisfiable name pred)
