{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Monad.IO.Class
import Control.Monad (void
                     ,liftM
                     )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Text (Text)

import qualified Data.Map.Strict as M
import Data.Map.Strict ((!), Map)

import Database.Persist.Sqlite
import Database.Esqueleto as E
import Text.Regex.PCRE

import System.Environment (getArgs)
import System.Directory (getHomeDirectory
                        ,getCurrentDirectory
                        )
import System.Posix.Files (fileExist
                          ,getSymbolicLinkStatus
                          ,isDirectory
                          ,createSymbolicLink
                          )
import System.Directory (removeFile
                        ,removeDirectoryRecursive
                        )

import qualified Control.Lens as L

import Storage


-- | Cleans a ${VAR} into VAR.
cleanVar :: Text
         -> Text
cleanVar = T.drop 2 . T.dropEnd 1

-- | Checks if a text is a variable.
isVar :: Text
      -> Bool
isVar v = T.unpack v =~ ("\\$\\{(.*)\\}" :: String)

-- | Replaces a var with the coresponging path.
subVar :: Map Text Text
       -> Text
       -> Text
subVar m t | isVar t = m ! cleanVar t
           | otherwise = t

-- | Converts a path with variables to one without with the variables
-- substituted.
convPath :: Map Text Text
         -> Text
         -> Text
convPath m = T.intercalate "/"
           . map (subVar m)
           . T.splitOn "/"

-- | Add a variable to the database.
addVar :: MonadIO m
       => Text -- ^ Variable name.
       -> Text -- ^ Variable value.
       -> SqlPersistT m ()
addVar var = void . insert . Var var

-- | Add a path with symlink location to the database.
addPath :: MonadIO m
        => Text -- ^ File location.
        -> Text -- ^ Symlink location.
        -> SqlPersistT m ()
addPath loc = void . insert . Symlink loc

-- | Get all variables from the database.
getAllVars :: MonadIO m
           => SqlPersistT m (Map Text Text)
getAllVars = do
    home <- liftM T.pack (liftIO getHomeDirectory)
    curDir <- liftM T.pack (liftIO getCurrentDirectory)

    other :: [Entity Var] <- select $
             from $ \var -> do
             return var

    let k = map (\x -> (x L.^. varName, x L.^. varValue)) . map entityVal $ other

    return $! M.fromList (("HOME", home) : ("WORK_DIR", curDir) : k)

-- | Takes a list of (to,from) paths and creates symlings "from" "to"
linkAll :: [(Text, Text)]
        -> IO ()
linkAll [] = return ()
linkAll (x:xs) = do
    let (t,f) = (T.unpack $ fst x, T.unpack $ snd x)

    ex <- fileExist f
          >>= (\ex' -> if ex'
                           then liftM Just $! getSymbolicLinkStatus f
                           else return Nothing)
    case ex of
        Nothing -> createSymbolicLink t f
        Just e -> if isDirectory e
                      then removeDirectoryRecursive f
                           >> createSymbolicLink t f
                      else removeFile f
                           >> createSymbolicLink t f
    linkAll xs


-- | Returns all symlinks to where they point and where they are.
getAllSym :: MonadIO m
          => SqlPersistT m [(Text, Text)]
getAllSym = do
    syms :: [Entity Symlink] <- select $
            from $ \s -> do
            return s
    return $! map (\x -> (x L.^. symlinkSource, x L.^. symlinkLink))
            . map entityVal $ syms

data AddType = CA_Variable
             | CA_Path
             deriving (Show, Eq)

data Command = AddVariable Text Text
             | AddPath Text Text
             | Version
             | ListAllSymlinks
             | ListAllVariables
             | SymlinkAll
             deriving (Show, Eq)

parseArgs :: [Text]
          -> [Command]
parseArgs [] = []
parseArgs ("--version":r)    = Version          : parseArgs r
parseArgs ("-v":r)           = Version          : parseArgs r
parseArgs ("--var":n:v:r)    = AddVariable n v  : parseArgs r
parseArgs ("--path":n:v:r)   = AddPath n v      : parseArgs r
parseArgs ("--list-var":r)   = ListAllVariables : parseArgs r
parseArgs ("--list-sym":r)   = ListAllSymlinks  : parseArgs r
parseArgs ("--run-linker":r) = SymlinkAll       : parseArgs r
parseArgs (_:r)              = parseArgs r

version :: Text
version = "0.1"

main :: IO ()
main = do
    args <- liftM (parseArgs . map T.pack) getArgs

    runSqlite "homelinker.db" $ do
        runMigration storage
        mapM_ runCommand args

    where
        runCommand :: MonadIO m
                   => Command
                   -> SqlPersistT m ()
        runCommand Version = liftIO $! T.putStrLn version
        runCommand (AddVariable n v) = addVar n v
        runCommand (AddPath n v) = addPath n v
        runCommand ListAllVariables = getAllVars >>=
                                      liftIO . mapM_ (T.putStrLn . T.pack . show)
                                             . M.toList
        runCommand ListAllSymlinks = getAllSym >>=
                                     liftIO . mapM_ (T.putStrLn . T.pack . show)
        runCommand SymlinkAll = do
            vm <- getAllVars
            l <- liftM (map (\(a, b) -> (convPath vm a, convPath vm b))) getAllSym
            liftIO $! linkAll l
