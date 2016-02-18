module LobbyTypes where
import qualified Control.Concurrent as CC
import Haste.App

type Name = String
type Player = (SessionID, Name)
type PlayerList = CC.MVar [Player]

type LobbyGame = CC.MVar (String, [Player])
type GamesList = CC.MVar [LobbyGame]