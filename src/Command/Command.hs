module Command.Command where

import Runtime
import Discord
import Discord.Internal.Types.Events (Event)

class Command a where
    matchEvent :: Event -> Maybe a
    matchEvent = const Nothing
    execute :: DiscordHandle -> a -> Runtime -> IO ()
    execute _ _ _ = pure ()
    executeWhenJust :: DiscordHandle -> Runtime -> Maybe a -> IO ()
    executeWhenJust dis runtime (Just cmd) = execute dis cmd runtime
    executeWhenJust dis runtime Nothing = pure ()
