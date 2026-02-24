-- | Coherence checking: verify that the environment has at most
-- one resolution for each constraint.
module Resolve.Coherence where

import Data.List (groupBy, sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord (comparing)
import Data.Text (Text)

import Resolve.Syntax

-- | Coherence violations.
data CoherenceError
  = OverlappingDecls PkgName [Decl]
    -- ^ Multiple declarations for the same package with overlapping version ranges
  deriving (Show)

-- | Check that the environment is non-overlapping: for each package name,
-- at most one declaration exists.
--
-- This is the strictest coherence condition (§7.3).
checkNonOverlapping :: Env -> [CoherenceError]
checkNonOverlapping env =
  [ OverlappingDecls name ds
  | ds@(_:_:_) <- groupByPkg env
  , let name = declPkg (head ds)
  ]

-- | Group declarations by package name.
groupByPkg :: Env -> [[Decl]]
groupByPkg env =
  map (map snd)
    . groupBy (\a b -> fst a == fst b)
    . sortBy (comparing fst)
    $ [(declPkg d, d) | d <- env]

-- | Check the weaker condition: for each package name, at most one
-- declaration is the "best" under the most-specific policy.
-- Multiple declarations are allowed if they have distinct versions
-- (the highest wins unambiguously).
checkMostSpecific :: Env -> [CoherenceError]
checkMostSpecific env =
  [ OverlappingDecls name ds
  | ds <- groupByPkg env
  , let name = declPkg (head ds)
  , hasTiedVersions ds
  ]
  where
    hasTiedVersions ds =
      let maxVer = maximum (map declVer ds)
      in length (filter (\d -> declVer d == maxVer) ds) > 1

-- | Check that the dependency graph is acyclic (§8.3 Termination).
-- Returns Nothing if acyclic, or Just the cycle if one is found.
checkAcyclic :: Env -> Maybe [PkgName]
checkAcyclic env = findCycle graph
  where
    graph = Map.fromListWith (<>)
      [ (declPkg d, constraintPkgs (declReqs d))
      | d <- env
      ]

    constraintPkgs :: Constraint -> [PkgName]
    constraintPkgs Trivial = []
    constraintPkgs (Pkg name _) = [name]
    constraintPkgs (And c1 c2) = constraintPkgs c1 ++ constraintPkgs c2

    -- Simple DFS cycle detection
    findCycle :: Map.Map PkgName [PkgName] -> Maybe [PkgName]
    findCycle g = go [] [] (Map.keys g)
      where
        go _path _visited [] = Nothing
        go path visited (n:ns)
          | n `elem` path = Just (n : takeWhile (/= n) path ++ [n])
          | n `elem` visited = go path visited ns
          | otherwise =
              case go (n:path) visited (Map.findWithDefault [] n g) of
                Just cycle -> Just cycle
                Nothing -> go path (n:visited) ns
