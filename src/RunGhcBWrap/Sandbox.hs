{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Platform boundary for both compilation (including TH) and execution.
module RunGhcBWrap.Sandbox (sandboxProcess, bwrapBaseArgs) where

import System.Process (CreateProcess(..), proc)
import System.Which (staticWhich)
#ifdef darwin_HOST_OS
import Control.Monad (unless)
import System.Directory (canonicalizePath)
import System.Environment (lookupEnv)
#endif

bwrapBaseArgs :: FilePath -> FilePath -> String -> String -> [String]
bwrapBaseArgs projectDir tmpDir hostPath packageDb =
  [ "--die-with-parent", "--bind", projectDir, "/project"
  , "--bind", tmpDir, "/tmp", "--dev", "/dev", "--proc", "/proc"
  , "--ro-bind", "/nix/store", "/nix/store"
  , "--setenv", "PATH", hostPath, "--setenv", "TMPDIR", "/tmp"
  , "--setenv", "GHC_PACKAGE_PATH", packageDb, "--chdir", "/project"
  ]

-- | Arguments remain separate argv entries. There is no shell interpolation
-- and no unsandboxed fallback. Darwin uses real paths rather than bind mounts.
sandboxProcess :: FilePath -> FilePath -> String -> String
  -> FilePath -> [String] -> IO CreateProcess
#ifdef darwin_HOST_OS
sandboxProcess projectDir tmpDir hostPath packageDb executable arguments = do
  platform <- lookupEnv "JENGA_DEV_PLATFORM"
  unless (platform == Just "macos") $
    ioError (userError "macOS execution sandbox requires JENGA_DEV_PLATFORM=macos")
  project <- canonicalizePath projectDir
  temporary <- canonicalizePath tmpDir
  pure (proc $(staticWhich "sandbox-exec")
    (["-D", "PROJECT=" ++ project, "-D", "TEMP=" ++ temporary,
      "-p", darwinProfile, executable] ++ arguments))
    { cwd = Just project, close_fds = True
    , env = Just [("PATH", hostPath), ("TMPDIR", temporary),
        ("HOME", temporary), ("LANG", "en_US.UTF-8"),
        ("GHC_PACKAGE_PATH", packageDb)] }

-- Parameters are passed with -D, never embedded in the profile's syntax.
-- Child processes inherit the policy. Project/temp writes are allowed;
-- other user files, network, and Mach services remain denied by default.
darwinProfile :: String
darwinProfile = unlines
  [ "(version 1)"
  , "(deny default)"
  , "(import \"dyld-support.sb\")"
  , "(allow mach-bootstrap syscall*)"
  , "(allow process-fork process-exec sysctl-read)"
  , "(allow signal (target self))"
  , "(allow file-read-metadata)"
  , "(allow file-read* file-map-executable"
  , "  (subpath \"/nix/store\") (subpath \"/usr/lib\")"
  , "  (subpath \"/usr/bin\") (subpath \"/bin\")"
  , "  (subpath \"/System/Library\")"
  , "  (subpath (param \"PROJECT\")) (subpath (param \"TEMP\")))"
  , "(allow file-write* (subpath (param \"PROJECT\")) (subpath (param \"TEMP\")))"
  , "(allow file-read* file-write-data (literal \"/dev/null\"))"
  , "(allow file-read* (literal \"/dev/random\") (literal \"/dev/urandom\"))"
  ]
#elif defined(linux_HOST_OS)
sandboxProcess projectDir tmpDir hostPath packageDb executable arguments =
  pure $ proc $(staticWhich "bwrap")
    (bwrapBaseArgs projectDir tmpDir hostPath packageDb ++ executable : arguments)
#else
#error Unsupported sandbox platform
#endif
