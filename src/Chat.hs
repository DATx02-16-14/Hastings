module Chat
    (
    addPlayerToMainChat,
    createNewChatRoom,
    Chat
    ) where

import Haste.App


type Name = String
-- |A chat
type Chat = (Name,[SessionID])


-- |Adds a player to the main chat room if it exists
addPlayerToMainChat :: SessionID -> [Chat] -> [Chat]
addPlayerToMainChat sid = map (addIfMatches "main")
  where
    addIfMatches :: Name -> Chat -> Chat
    addIfMatches name' c@(name, sids) | name == name' = (name, sid : sids)
                                      | otherwise = c

createNewChatRoom :: String -> (Name, [SessionID])
createNewChatRoom name = (name, [])
