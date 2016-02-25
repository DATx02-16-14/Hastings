module Chat
    (
    addPlayerToChat,
    createNewChatRoom,
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

createNewChatRoom :: String -> (Name, [SessionID])
createNewChatRoom name = (name, [])
