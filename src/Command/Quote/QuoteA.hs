{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Command.Quote.QuoteA where

import qualified Data.Text as Text
import qualified Data.Vector as Vec
import Runtime
import Command.Command
import Command.Quote.Quote as Quote
import Discord.Internal.Types.Events (Event(MessageCreate))
import Discord.Internal.Types.Channel
import Discord.Internal.Types.Prelude
import Discord.Internal.Types.User
import Discord.Internal.Rest.Channel
import Control.Monad
import Data.Maybe
import Data.List
import qualified Discord.Internal.Types.User as User
import Relude.List
import GHC.Word
import Discord
import Control.Concurrent.MVar
import System.Random (randomRIO)

data QuoteA = QuoteA (ChannelId, Maybe Text.Text) deriving Show

instance Command QuoteA where
    matchEvent (MessageCreate msg) = 
        let args = Vec.fromList $ Text.words $ messageText msg
        in case "*quotea" == (Text.toLower $ args Vec.! 0) of
            True  -> Just $ QuoteA (messageChannel msg, case Vec.length args > 1 of
                True  -> Just $ Text.concat [ "Bad arguments, fag.(" , Text.pack $ show $ length $ args ,")"]
                False -> Nothing)
    matchEvent _ = Nothing

    execute dis (QuoteA (channelId, Nothing)) runtime@Runtime{Runtime.quotes=mvqs}
        = do
            wqs <- readMVar mvqs
            let qs = Quote.quotes wqs
            case Vec.length qs > 0 of
                True -> do
                    quoteIndex <- randomRIO (0, Vec.length qs - 1)
                    let quote = qs Vec.! quoteIndex
                    _ <- restCall dis (CreateMessage channelId (Text.concat 
                        [ content quote , "\n- ", fromMaybe "Unknown" (user quote)
                        ]))
                    pure ()
                False -> do
                    _ <- restCall dis (CreateMessage channelId "There are no quotes to show, fag.")
                    pure ()
    execute dis (QuoteA (channelId, Just msg)) runtime
        = do 
            _ <- restCall dis (CreateMessage channelId msg)
            pure ()