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
import Haste.App (SessionID)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Game
    uuid                String
    name                String
    maxAmountOfPlayers  Int
    owner               SessionID
    password            String
    UniqueName          name
    UniqueUUID          uuid
PlayerInGame
    game          GameId
    player        SessionID
    deriving Show
Player
    userName       String
    UniqueUsername userName
    deriving Show
OnlinePlayer
    player        PlayerId
    sessionID     SessionID
    UniqueSession sessionID
    deriving Show
|]
