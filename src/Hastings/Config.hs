module Hastings.Config where

import Data.Word

-- Client configuration
backendHostAddress = "129.16.23.58"
backendHostPort    = 24601 :: Int

-- Database configuraton
databaseHostAddress = "localhost"
databaseHostPort    = 3306 :: Word16
databaseUser        = "root"
databaseName         = "hastings"
