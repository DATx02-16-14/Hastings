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
    name               String
    maxAmountOfPlayers Int
    UniqueName name
PlayerInGame
    game          GameId
    player        SessionID
Player
    userName       String
    UniqueUsername userName
OnlinePlayer
    player        PlayerId
    sessionID     SessionID
    UniqueSession sessionID
|]
