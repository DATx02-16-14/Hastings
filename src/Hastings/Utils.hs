-- |Module for all of the util functions that does not belong elsewhere and are perhaps so generic that they could be made into a separate library
module Hastings.Utils where

-- |Method that finds with IO and then returns the value
-- together with the list to the right and left of the element
findIO :: (a -> IO Bool) -> [a] -> IO (Maybe a, [a], [a])
findIO p as = findIO' p as []
  where
    findIO' _ [] hs = return (Nothing, [], hs)
    findIO' p (a:as) hs = do
      b <- p a
      case b of
        True  -> return $ (Just a, hs, as)
        False -> findIO' p as (a:hs)
