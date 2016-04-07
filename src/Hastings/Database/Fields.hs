{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
-- | All database fields.

module Hastings.Database.Fields where

import Database.Persist.TH
import Data.Word (Word64)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
    name String
    maxAmountOfPlayers Int
Player
    userName String
    UniqueUsername userName
OnlinePlayer
    player PlayerId
    sessionID Word64
    UniqueSession sessionID
|]
