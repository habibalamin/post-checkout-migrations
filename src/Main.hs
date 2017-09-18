{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Prelude hiding ((\\))
import System.Environment
import System.Exit
import System.IO.Temp
import System.Directory
import Git.Repository
import Git.Libgit2 hiding (Tree, CommitOid)
import Git.Tree
import Git.Tree.Builder
import Git.Blob
import Git.Types
import Data.Text as Text hiding (map, any)
import Control.Monad.Trans.Class
import qualified Data.Set as Set
import Data.Set ((\\), Set)
import Data.ByteString hiding (map, putStrLn, any)
import qualified Data.ByteString.Char8 as B8
import System.FilePath
import Control.Monad.IO.Class
import Control.Monad
import qualified Data.Text.IO as TextIO

type Ref = String

data CheckoutType
  = Branch
  | File
  deriving (Show, Eq)

data MigrationStatus
  = AddedMigration ByteString
  | RemovedMigration ByteString
  deriving (Show, Eq)

main :: IO ()
main = do
  argv <- getArgs

  case argv of
    [oldHEAD, newHEAD, isBranchCheckout] ->
      postCheckout oldHEAD newHEAD $ flagToCheckoutType isBranchCheckout
    _ -> do
      programName <- getProgName

      die $
        programName ++ ": " ++
        "did not receive expected arguments " ++
        "(old HEAD ref, new HEAD ref, checkout type flag)"

postCheckout
  :: String -- ^ Old HEAD ref
  -> String -- ^ New HEAD ref
  -> CheckoutType
  -> IO ()
postCheckout oldHEAD newHEAD Branch = do
  tmpDir <- getCanonicalTemporaryDirectory
            >>= flip createTempDirectory "postcheckout"

  repoPath <- (++ "/.git") <$> getCurrentDirectory

  migrations <- Git.Repository.withRepository lgFactory repoPath $ do
    oldCommit <- parseObjOid $ Text.pack oldHEAD
    newCommit <- parseObjOid $ Text.pack newHEAD

    oldMigrations <- migrationsFromRef oldCommit
    newMigrations <- migrationsFromRef newCommit

    let
      removedMigrations = Set.toList $ oldMigrations \\ newMigrations
      addedMigrations   = Set.toList $ newMigrations \\ oldMigrations

    treeFromRef oldCommit >>= flip withTree
      (mapM_ (saveMigrationFileTo tmpDir) removedMigrations)

    return
      $  map AddedMigration   addedMigrations
      ++ map RemovedMigration removedMigrations

  when (any isRemovedMigration migrations) . putStrLn
    $  "[INFO] "
    ++ "You are on a branch that lacks certain migrations found on "
    ++ "the branch you just left. If you'd like to roll back these "
    ++ "migrations, you can reach copies of them in this temporary "
    ++ "directory:\n" ++ tmpDir
postCheckout _ _ _ = exitSuccess

flagToCheckoutType :: String -> CheckoutType
flagToCheckoutType "1" = Branch
flagToCheckoutType _   = File

migrationsFromRef :: MonadGit r f => CommitOid r -> f (Set TreeFilePath)
migrationsFromRef ref = Set.fromList . map fst <$> migrationsFromRef'
  where
    migrationsFromRef' =
      treeFromRef ref
      >>= flip withTree (getEntry migrationsDir)
      >>= maybe (fail noMigrationsDir) (pure . treeEntryOid) . fst
      >>= lookupTree
      >>= listTreeEntries

treeFromRef :: MonadGit r m => CommitOid r -> m (Tree r)
treeFromRef ref = lookupCommit ref >>= lookupTree . commitTree

saveMigrationFileTo
  :: (MonadGit r m, MonadIO m)
  => FilePath
  -> ByteString
  -> TreeT r m ()
saveMigrationFileTo dir filename = do
  migrationEntry' <- getEntry $ migrationsDir `B8.append` filename

  let
    filename' = B8.unpack filename
    filepath = dir </> filename'

  case migrationEntry' of
    Nothing             -> fail $ missingMigration filename'
    Just migrationEntry ->
      readBlobEntry migrationEntry
      >>= writeBlobContents filepath

readBlobEntry :: MonadGit r m => TreeEntry r -> TreeT r m Text
readBlobEntry = lift . catBlobUtf8 . blobEntryOid

writeBlobContents :: MonadIO m => FilePath -> Text -> m ()
writeBlobContents = liftIO ... TextIO.writeFile

-- Composition with two arguments on the second function.
(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)

isRemovedMigration :: MigrationStatus -> Bool
isRemovedMigration (RemovedMigration _) = True
isRemovedMigration _ = False

migrationsDir :: ByteString
migrationsDir = "db/migrate/"

noMigrationsDir :: String
noMigrationsDir =
  "[WARNING] Could not find migrations folder from previous ref"
  ++ "; no migrations have been rolled back."

missingMigration :: String -> String
missingMigration name =
  "[WARNING] Could not find removed migration from previous ref"
  ++ ": " ++ name ++ "."
