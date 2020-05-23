{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Command.Quote.QuoteAdd where

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

data QuoteAdd = QuoteAdd (ChannelId, (Either Text.Text MessageId)) deriving Show

instance Command QuoteAdd where
    matchEvent (MessageCreate msg) = 
        let args = Vec.fromList $ Text.words $ messageText msg
            parsedId =  (join $ ((fst <$>) . listToMaybe . reads . Text.unpack) <$> (args Vec.!? 1))::Maybe Word64
        in case "*quoteadd" == (Text.toLower $ args Vec.! 0) of
            False -> Nothing
            True -> Just $ QuoteAdd $ case parsedId of
                (Just messageId) -> (messageChannel msg, Right $ Snowflake $ messageId)
                Nothing -> (messageChannel msg, Left $ Text.concat [
                    "Bad arguments, fag.("
                  , Text.pack $ show $ length $ args
                  ,")"])
    matchEvent _ = Nothing

    execute dis (QuoteAdd (channelId, Right messageId)) runtime@Runtime{Runtime.quotes=mvqs}
        = do
            quoteMessage <- restCall dis $ GetChannelMessage (channelId, messageId)
            case quoteMessage of
                Right message -> do
                    let msgContents = messageText message
                    let user = messageAuthor message
                    let uId = (\(Snowflake id) -> id) $ User.userId user
                    let newquote = Quote msgContents (Just $ userName user) (Just $ uId)
                    qs <- takeMVar mvqs
                    let newqs = (Quotes . Vec.fromList . ((:)newquote) . Vec.toList . Quote.quotes) qs 
                    putMVar mvqs newqs
                    let outMsg = Text.concat $ [ "Added quote:\n", msgContents, "\n- ", userName user, "\nCurrent no. of quotes ", Text.pack $ show $ Vec.length $ Quote.quotes $ newqs, "."]
                    _ <- restCall dis (CreateMessage channelId outMsg)
                    pure ()
--                    return runtime{Runtime.quotes =
--                        (Quotes . Vec.fromList . ((:)newquote) . Vec.toList . Quote.quotes) <$> qs
--                        }
                Left _ -> do
                    _ <- restCall dis (CreateMessage channelId "How the fuck did you reach this error message?")
                    pure ()
    execute dis (QuoteAdd (channelId, Left msg)) runtime
        = do 
            _ <- restCall dis (CreateMessage channelId msg)
            pure ()