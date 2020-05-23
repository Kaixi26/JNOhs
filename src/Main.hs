{-# LANGUAGE OverloadedStrings #-}  -- allows "string literals" to be Text
module Main where

import Control.Monad
import Data.Text (isPrefixOf, toLower, Text, pack, unpack)
import Control.Concurrent (threadDelay)
import qualified Data.Text.IO as TIO

import Discord
import Discord.Types
import Command.Quote.QuoteAdd
import Command.Quote.QuoteA
import Command.Command
import Control.Concurrent
import Runtime
import qualified Discord.Requests as R
import Data.Maybe
import GHC.Word
import System.IO

import qualified Config as Config

-- | Replies "pong" to every message that starts with "ping"
--pingpongExample :: IO ()
--pingpongExample = do userFacingError <- runDiscord $ def
--                                            { discordToken = Config.discordToken
--                                            , discordOnEvent = commandHandler }
--                     TIO.putStrLn userFacingError

eventHandler :: DiscordHandle -> Event -> IO ()
eventHandler dis event = case event of
       MessageCreate m -> when (not (fromBot m) && isPing (messageText m)) $ do
               _ <- restCall dis (R.CreateReaction (messageChannel m, messageId m) "eyes")
               --threadDelay (4 * 10^6)
               _ <- restCall dis (R.CreateMessage (messageChannel m) "Pong!")
               pure ()
       _ -> pure ()

fromBot :: Message -> Bool
fromBot m = userIsBot (messageAuthor m)

isPing :: Text -> Bool
isPing = ("ping" `isPrefixOf`) . toLower


matchAllAndExecute :: Runtime -> DiscordHandle -> Event -> IO ()
matchAllAndExecute runtime dis event = do
    executeWhenJust dis runtime ((matchEvent event)::Maybe QuoteAdd)
    executeWhenJust dis runtime ((matchEvent event)::Maybe QuoteA)
    return ()

commandHandler :: Runtime -> DiscordHandle -> Event -> IO ()
commandHandler runtime dis event = do
    executeWhenJust dis runtime ((matchEvent event)::Maybe QuoteAdd)
    executeWhenJust dis runtime ((matchEvent event)::Maybe QuoteA)
    return ()

daemon :: Runtime -> IO Text
daemon state = runDiscord $ def
        { discordToken = Config.discordToken 
        , discordOnEvent = commandHandler state
        , discordOnStart = prompt
        }

prompt :: DiscordHandle -> IO ()
prompt dis = do
  putStrLn "Bot Started!"
  _ <- forkIO $ forever (do
    putStr ">> "
    hFlush stdout
    line <- getLine
    let args = map pack $ words $ line
    case args !! 0 of
        "write" -> do
            putStr ">>> "
            hFlush stdout
            line <- getLine
            _ <- restCall dis (R.CreateMessage (Snowflake $ read $ unpack $ (args !! 1)) (pack line))
            pure ()
        otherwise -> pure()
    )
  pure()

main :: IO ()
main = do
    putStrLn "Starting Bot."
    state <- defaultRuntime
    err <- daemon state
    print err
