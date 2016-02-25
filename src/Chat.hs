module Chat
    (
    addPlayerToChat,
    createNewChatRoom,
    removePlayerFromChats,
    Chat
    ) where

import Haste.App
import Data.List
import Hastings.Utils


type Name = String
-- |A chat
type Chat = (Name,[SessionID])


-- |Adds a player to the main chat room. If it doesn't exists, do nothing.
addPlayerToChat :: SessionID -> Name -> [Chat] -> [Chat]
addPlayerToChat sid = updateLookup (\sids -> nub $ sid : sids)

removePlayerFromChat :: SessionID -> Name -> [Chat] -> [Chat]
removePlayerFromChat sid = updateLookup (delete sid)

removePlayerFromChats :: SessionID -> [Chat] -> [Chat]
removePlayerFromChats sid cs = map (removeSessionFromChat sid) cs

createNewChatRoom :: String -> (Name, [SessionID])
createNewChatRoom name = (name, [])

-- | Adds a SessinID to a chat. If SessionID already exists then do nothing.
addSessionToChat :: SessionID -> Chat -> Chat
addSessionToChat sid (n,ss) | sid `elem` ss = (n,ss)
                            | otherwise     = (n,sid:ss)

-- | Removes a SessinID from a chat. If SessionID doesnt' exists then do nothing.
removeSessionFromChat :: SessionID -> Chat -> Chat
removeSessionFromChat sid (n,ss) = (n, delete sid ss)
