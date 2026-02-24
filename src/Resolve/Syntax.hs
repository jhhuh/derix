-- | Core syntax of the resolution calculus.
--
-- Constraints are the "types", evidence terms are the "terms".
-- Instance declarations are inference rules.
module Resolve.Syntax where

import Data.Map.Strict qualified as Map
import Data.Text (Text)

-- §2.1 Names and Versions

type PkgName = Text

data Version = Version !Int !Int !Int
  deriving (Eq, Ord)

instance Show Version where
  show (Version a b c) = show a <> "." <> show b <> "." <> show c

-- §2.2 Version Predicates

data VerPred
  = Exact Version       -- = v
  | AtLeast Version     -- >= v
  | Range Version Version  -- [v1, v2)
  | AnyVersion          -- ⊤
  deriving (Show)

-- | v ⊨ π
satisfies :: Version -> VerPred -> Bool
satisfies _ AnyVersion = True
satisfies v (Exact v') = v == v'
satisfies v (AtLeast v') = v >= v'
satisfies v (Range lo hi) = v >= lo && v < hi

-- §2.3 Constraints

data Constraint
  = Pkg PkgName VerPred     -- Pkg(p, π)
  | And Constraint Constraint  -- C₁ ∧ C₂
  | Trivial                 -- ⊤
  deriving (Show)

-- §2.4 Evidence

data Evidence
  = Inst PkgName Version [Evidence]  -- inst(p, v, ē)
  | Pair Evidence Evidence           -- (e₁, e₂)
  | Unit                             -- ★
  deriving (Show, Eq)

-- | Extract the version from an evidence term.
evidenceVersion :: Evidence -> Maybe Version
evidenceVersion (Inst _ v _) = Just v
evidenceVersion _ = Nothing

-- §2.5 Instance Declarations

data Decl = Decl
  { declPkg  :: PkgName
  , declVer  :: Version
  , declReqs :: Constraint
  } deriving (Show)

-- §2.6 Instance Environment

type Env = [Decl]

-- | All package names mentioned in an environment.
envPkgNames :: Env -> [PkgName]
envPkgNames = map declPkg

-- | Find all declarations for a given package name.
findDecls :: Env -> PkgName -> [Decl]
findDecls env name = filter (\d -> declPkg d == name) env

-- | Find declarations matching a name and predicate.
findMatching :: Env -> PkgName -> VerPred -> [Decl]
findMatching env name pred =
  filter (\d -> declPkg d == name && satisfies (declVer d) pred) env
