{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Storage where

import Database.Persist.TH
import Data.Text (Text)

share [mkPersist sqlSettings { mpsGenerateLenses = True }, mkMigrate "storage"] [persistLowerCase|
Symlink
    source  Text
    link    Text
    deriving Show
Var
    name Text
    value Text
    deriving Show
|]

