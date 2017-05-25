{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE CPP        #-}

-- |
-- Module      :  $Header$
-- Copyright   :  (c) 2015 Adam C. Foltzer
-- License     :  BSD3
-- Maintainer  :  acfoltzer@galois.com
-- Stability   :  provisional
-- Portability :  portable
--
-- Some handy Template Haskell splices for including the current git
-- hash and branch in the code of your project. Useful for including
-- in panic messages, @--version@ output, or diagnostic info for more
-- informative bug reports.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > import Development.GitRev
-- >
-- > panic :: String -> a
-- > panic msg = error panicMsg
-- >   where panicMsg =
-- >           concat [ "[panic ", $(gitBranch), "@", $(gitHash)
-- >                  , " (", $(gitCommitDate), ")"
-- >                  , " (", $(gitCommitCount), " commits in HEAD)"
-- >                  , dirty, "] ", msg ]
-- >         dirty | $(gitDirty) = " (uncommitted files present)"
-- >               | otherwise   = ""
-- >
-- > main = panic "oh no!"
--
-- > % cabal exec runhaskell Example.hs
-- > Example.hs: [panic master@2ae047ba5e4a6f0f3e705a43615363ac006099c1 (Mon Jan 11 11:50:59 2016 -0800) (14 commits in HEAD) (uncommitted files present)] oh no!

module Development.GitRev
  ( gitBranch
  , gitCommitCount
  , gitCommitDate
  , gitDescribe
  , gitDirty
  , gitDirtyTracked
  , gitHash
  ) where

import Control.Exception
import Control.Monad
import Data.Maybe
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, trueName, falseName)
import System.Exit
import System.FilePath

import Prelude ()
import Prelude.Compat hiding (readFile)

#if MIN_VERSION_template_haskell(2,13,0)
import qualified Language.Haskell.TH.Syntax as TH
#else
import qualified Process as Proc
import qualified Directory as Dir
import qualified Prelude.Compat as P
#endif

readFile :: FilePath -> Q String
doesFileExist :: FilePath -> Q Bool
doesDirectoryExist :: FilePath -> Q Bool
getCurrentDirectory :: Q FilePath
findExecutables :: String -> Q [FilePath]
readProcessWithExitCode :: FilePath -> [String] -> String
                        -> Q (ExitCode, String, String)

#if MIN_VERSION_template_haskell(2,13,0)
readFile = TH.readFile
doesFileExist = TH.doesFileExist
doesDirectoryExist = TH.doesDirectoryExist
getCurrentDirectory = TH.getCurrentDirectory
findExecutables = TH.findExecutables
readProcessWithExitCode p args = TH.readProcessWithExitCode p args
#else
readFile = runIO . P.readFile
doesFileExist = runIO . Dir.doesFileExist
doesDirectoryExist = runIO . Dir.doesDirectoryExist
getCurrentDirectory = runIO Dir.getCurrentDirectory
findExecutables = runIO . Proc.findExecutables
readProcessWithExitCode p args = runIO . Proc.readProcessWithExitCode p args
#endif

-- | Run git with the given arguments and no stdin, returning the
-- stdout output. If git isn't available or something goes wrong,
-- return the second argument.
runGit :: [String] -> String -> IndexUsed -> Q String
runGit args def useIdx = do
--  let oops :: SomeException -> IO (ExitCode, String, String)
--      oops _e = return (ExitFailure 1, def, "")
  gitFound <- isJust . listToMaybe <$> findExecutables "git"
  if gitFound
    then do
      -- a lot of bookkeeping to record the right dependencies
      pwd <- getDotGit
      let hd         = pwd </> ".git" </> "HEAD"
          index      = pwd </> ".git" </> "index"
          packedRefs = pwd </> ".git" </> "packed-refs"
      hdExists  <- doesFileExist hd
      when hdExists $ do
        -- the HEAD file either contains the hash of a detached head
        -- or a pointer to the file that contains the hash of the head
        splitAt 5 `fmap` (readFile hd) >>= \case
          -- pointer to ref
          ("ref: ", relRef) -> do
            let ref = pwd </> ".git" </> relRef
            refExists <- doesFileExist ref
            when refExists $ addDependentFile ref
          -- detached head
          _hash -> addDependentFile hd
      -- add the index if it exists to set the dirty flag
      indexExists <- doesFileExist index
      when (indexExists && useIdx == IdxUsed) $ addDependentFile index
      -- if the refs have been packed, the info we're looking for
      -- might be in that file rather than the one-file-per-ref case
      -- handled above
      packedExists <- doesFileExist packedRefs
      when packedExists $ addDependentFile packedRefs
      do
        (code, out, _err) <- readProcessWithExitCode "git" args "" -- `catch` oops
        case code of
          ExitSuccess   -> return (takeWhile (/= '\n') out)
          ExitFailure _ -> return def
    else return def

-- | Determine where our @.git@ directory is, in case we're in a
-- submodule.
getDotGit :: Q FilePath
getDotGit = do
  pwd <- getGitRoot
  let dotGit = pwd </> ".git"
      oops = return dotGit -- it's gonna fail, that's fine
  isDir <- doesDirectoryExist dotGit
  isFile <- doesFileExist dotGit
  if | isDir -> return dotGit
     | not isFile -> oops
     | isFile ->
         splitAt 8 `fmap` readFile dotGit >>= \case
           ("gitdir: ", relDir) -> do
             isRelDir <- doesDirectoryExist relDir
             if isRelDir
               then return relDir
               else oops
           _ -> oops

-- | Get the root directory of the Git repo.
getGitRoot :: Q FilePath
getGitRoot = do
  pwd <- getCurrentDirectory
  (code, out, _) <-
    readProcessWithExitCode "git" ["rev-parse", "--show-toplevel"] ""
  case code of
    ExitSuccess   -> return $ takeWhile (/= '\n') out
    ExitFailure _ -> return pwd -- later steps will fail, that's fine

-- | Type to flag if the git index is used or not in a call to runGit
data IndexUsed = IdxUsed -- ^ The git index is used
               | IdxNotUsed -- ^ The git index is /not/ used
    deriving (Eq)

-- | Return the hash of the current git commit, or @UNKNOWN@ if not in
-- a git repository
gitHash :: ExpQ
gitHash =
  stringE =<< runGit ["rev-parse", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the branch (or tag) name of the current git commit, or @UNKNOWN@
-- if not in a git repository. For detached heads, this will just be
-- "HEAD"
gitBranch :: ExpQ
gitBranch =
  stringE =<< runGit ["rev-parse", "--abbrev-ref", "HEAD"] "UNKNOWN" IdxNotUsed

-- | Return the long git description for the current git commit, or
-- @UNKNOWN@ if not in a git repository.
gitDescribe :: ExpQ
gitDescribe =
  stringE =<< runGit ["describe", "--long", "--always"] "UNKNOWN" IdxNotUsed

-- | Return @True@ if there are non-committed files present in the
-- repository
gitDirty :: ExpQ
gitDirty = do
  output <- runGit ["status", "--porcelain"] "" IdxUsed
  case output of
    "" -> conE falseName
    _  -> conE trueName

-- | Return @True@ if there are non-commited changes to tracked files
-- present in the repository
gitDirtyTracked :: ExpQ
gitDirtyTracked = do
  output <- runGit ["status", "--porcelain","--untracked-files=no"] "" IdxUsed
  case output of
    "" -> conE falseName
    _  -> conE trueName

-- | Return the number of commits in the current head
gitCommitCount :: ExpQ
gitCommitCount =
  stringE =<< runGit ["rev-list", "HEAD", "--count"] "UNKNOWN" IdxNotUsed

-- | Return the commit date of the current head
gitCommitDate :: ExpQ
gitCommitDate =
  stringE =<< runGit ["log", "HEAD", "-1", "--format=%cd"] "UNKNOWN" IdxNotUsed
