-- | Pretty-printing for evidence terms and resolution traces.
module Resolve.Pretty where

import Data.Text (Text)
import Data.Text qualified as T
import Resolve.Syntax

-- | Pretty-print an evidence term as a derivation tree.
ppEvidence :: Evidence -> String
ppEvidence = go 0
  where
    go indent Unit = replicate indent ' ' ++ "★"
    go indent (Pair e1 e2) =
      go indent e1 ++ "\n" ++ go indent e2
    go indent (Inst name ver deps) =
      replicate indent ' '
        ++ T.unpack name ++ "@" ++ show ver
        ++ case deps of
             [] -> ""
             _  -> "\n" ++ unlines' (map (go (indent + 2)) deps)

    unlines' = init . unlines  -- drop trailing newline

-- | Pretty-print a constraint.
ppConstraint :: Constraint -> String
ppConstraint Trivial = "⊤"
ppConstraint (Pkg name pred) = T.unpack name ++ ppVerPred pred
ppConstraint (And c1 c2) = ppConstraint c1 ++ " ∧ " ++ ppConstraint c2

ppVerPred :: VerPred -> String
ppVerPred AnyVersion = "@*"
ppVerPred (Exact v) = "@=" ++ show v
ppVerPred (AtLeast v) = "@≥" ++ show v
ppVerPred (Range lo hi) = "@[" ++ show lo ++ "," ++ show hi ++ ")"

-- | Pretty-print a declaration.
ppDecl :: Decl -> String
ppDecl d =
  "instance " ++ T.unpack (declPkg d) ++ "@" ++ show (declVer d)
    ++ " given " ++ ppConstraint (declReqs d)

-- | Pretty-print the environment.
ppEnv :: Env -> String
ppEnv = unlines . map ppDecl
