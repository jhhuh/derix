-- | Derix — demonstration of the resolution calculus.
--
-- Runs the worked examples from doc/calculus.md §9, showing that
-- Haskell's laziness IS the calculus's operational semantics.
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Map.Lazy qualified as Map
import Data.Text (Text)

import Resolve.Syntax
import Resolve.Resolve
import Resolve.Fix
import Resolve.Overlay
import Resolve.Coherence
import Resolve.Pretty

-- Convenience
v :: Int -> Int -> Int -> Version
v = Version

-- §9.1 Basic Resolution with Sharing
exampleEnv :: Env
exampleEnv =
  [ Decl "zlib"    (v 1 3 0) Trivial
  , Decl "openssl" (v 3 1 0) (Pkg "zlib" (AtLeast (v 1 0 0)))
  , Decl "curl"    (v 8 5 0) (And (Pkg "openssl" (AtLeast (v 3 0 0)))
                                   (Pkg "zlib"    (AtLeast (v 1 0 0))))
  , Decl "python"  (v 3 12 0) (And (Pkg "openssl" (AtLeast (v 3 0 0)))
                                    (Pkg "zlib"    (AtLeast (v 1 2 0))))
  ]

-- §9.4 Diamond Dependencies
diamondEnv :: Env
diamondEnv =
  [ Decl "base"   (v 4 18 0) Trivial
  , Decl "text"   (v 2 0 0)  (Pkg "base" (AtLeast (v 4 0 0)))
  , Decl "parsec" (v 3 1 0)  (And (Pkg "base" (AtLeast (v 4 0 0)))
                                   (Pkg "text" (AtLeast (v 1 0 0))))
  , Decl "aeson"  (v 2 2 0)  (And (Pkg "base" (AtLeast (v 4 0 0)))
                                   (Pkg "text" (AtLeast (v 2 0 0))))
  , Decl "myapp"  (v 1 0 0)  (And (Pkg "parsec" (AtLeast (v 3 0 0)))
                                   (Pkg "aeson"  (AtLeast (v 2 0 0))))
  ]

-- §9.2 Cycle Detection
cycleEnv :: Env
cycleEnv =
  [ Decl "a" (v 1 0 0) (Pkg "b" (AtLeast (v 1 0 0)))
  , Decl "b" (v 1 0 0) (Pkg "a" (AtLeast (v 1 0 0)))
  ]

main :: IO ()
main = do
  putStrLn "═══════════════════════════════════════════════════"
  putStrLn " Derix — Resolution Calculus Prototype"
  putStrLn "═══════════════════════════════════════════════════"
  putStrLn ""

  -- §9.1: Basic resolution with sharing
  putStrLn "── §9.1 Basic Resolution with Sharing ──"
  putStrLn ""
  putStrLn "Environment:"
  putStr (ppEnv exampleEnv)
  putStrLn ""

  case buildPkgSet selectHighest exampleEnv of
    Left err -> putStrLn ("Resolution failed: " ++ show err)
    Right pkgs -> do
      -- Force only python — demonstrates laziness (curl not forced)
      putStrLn "Forcing python:"
      case forcePkg pkgs "python" of
        Left err -> putStrLn ("  Error: " ++ err)
        Right ev -> putStrLn (ppEvidence ev)
      putStrLn ""

      -- Now force curl — demonstrates sharing (zlib/openssl already resolved)
      putStrLn "Forcing curl (zlib and openssl are shared):"
      case forcePkg pkgs "curl" of
        Left err -> putStrLn ("  Error: " ++ err)
        Right ev -> putStrLn (ppEvidence ev)
      putStrLn ""

  -- §9.4: Diamond dependencies
  putStrLn "── §9.4 Diamond Dependencies ──"
  putStrLn ""
  putStrLn "Environment:"
  putStr (ppEnv diamondEnv)
  putStrLn ""

  case buildPkgSet selectHighest diamondEnv of
    Left err -> putStrLn ("Resolution failed: " ++ show err)
    Right pkgs -> do
      putStrLn "Forcing myapp (diamond: parsec→text, aeson→text):"
      case forcePkg pkgs "myapp" of
        Left err -> putStrLn ("  Error: " ++ err)
        Right ev -> putStrLn (ppEvidence ev)

      -- Verify sharing: text evidence is the same object
      let textFromParsec = case Map.lookup "parsec" pkgs of
            Just (Inst _ _ deps) -> head [d | d@(Inst "text" _ _) <- deps]
            _ -> Unit
          textFromAeson = case Map.lookup "aeson" pkgs of
            Just (Inst _ _ deps) -> head [d | d@(Inst "text" _ _) <- deps]
            _ -> Unit
      putStrLn ""
      putStrLn $ "text from parsec == text from aeson: "
        ++ show (textFromParsec == textFromAeson)
      putStrLn ""

  -- §9.3: Overlay
  putStrLn "── §9.3 Overlay ──"
  putStrLn ""

  case buildPkgSet selectHighest exampleEnv of
    Left err -> putStrLn ("Resolution failed: " ++ show err)
    Right basePkgs -> do
      putStrLn "Base: openssl is 3.1.0"
      case forcePkg basePkgs "openssl" of
        Left err -> putStrLn ("  Error: " ++ err)
        Right ev -> putStrLn ("  " ++ ppEvidence ev)
      putStrLn ""

      -- Apply overlay: bump openssl to 3.2.0
      let overlay = mkOverlay
            [ ("openssl", \final ->
                let zlibEv = final Map.! "zlib"
                in Inst "openssl" (v 3 2 0) [zlibEv])
            ]
      let overlaid = applyOverlay overlay basePkgs

      putStrLn "After overlay: openssl bumped to 3.2.0"
      case forcePkg overlaid "openssl" of
        Left err -> putStrLn ("  Error: " ++ err)
        Right ev -> putStrLn ("  " ++ ppEvidence ev)
      putStrLn ""

      -- python in the overlaid set picks up the new openssl automatically.
      -- We re-resolve all dependents through the overlay so they see final.openssl.
      putStrLn "python in overlaid set (picks up openssl@3.2.0 automatically):"
      let patchedEnv =
            [ Decl "zlib"    (v 1 3 0) Trivial
            , Decl "openssl" (v 3 2 0) (Pkg "zlib" (AtLeast (v 1 0 0)))  -- bumped!
            , Decl "curl"    (v 8 5 0) (And (Pkg "openssl" (AtLeast (v 3 0 0)))
                                             (Pkg "zlib"    (AtLeast (v 1 0 0))))
            , Decl "python"  (v 3 12 0) (And (Pkg "openssl" (AtLeast (v 3 0 0)))
                                              (Pkg "zlib"    (AtLeast (v 1 2 0))))
            ]
      let fullOverlay = overlayFromEnv selectHighest patchedEnv
                          ["openssl", "curl", "python"]
      let fullyOverlaid = applyOverlay fullOverlay basePkgs
      case forcePkg fullyOverlaid "python" of
        Left err -> putStrLn ("  Error: " ++ err)
        Right ev -> putStrLn (ppEvidence ev)
      putStrLn ""

  -- Coherence checking
  putStrLn "── Coherence Checks ──"
  putStrLn ""

  putStrLn "exampleEnv (non-overlapping): "
  case checkNonOverlapping exampleEnv of
    [] -> putStrLn "  ✓ coherent"
    errs -> mapM_ (\e -> putStrLn ("  ✗ " ++ show e)) errs

  -- §9.2: Cycle detection
  putStrLn ""
  putStrLn "cycleEnv (acyclicity check):"
  case checkAcyclic cycleEnv of
    Nothing -> putStrLn "  ✓ acyclic"
    Just cycle -> putStrLn ("  ✗ cycle detected: " ++ show cycle)
  putStrLn ""

  putStrLn "── Done ──"
