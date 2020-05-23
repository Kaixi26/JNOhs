module Runtime where

import Discord.Handle
import qualified Command.Quote.Quote as Quote
import Data.Vector (Vector, fromList)
import Control.Concurrent.MVar

data Runtime = Runtime {
        quotes :: MVar Quote.Quotes
    }

--defaultRuntime handle = undefined
--   Runtime{
--        quotes = Just $ Quote.Quotes $ fromList []
--      , discordHandle = handle
--    }

defaultRuntime :: IO Runtime
defaultRuntime = do
  qs <- newMVar $ Quote.Quotes $ fromList []
  return Runtime
    { quotes = qs
    }

