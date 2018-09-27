module Teleirc where

import Telegram
import Settings
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import UnliftIO.Concurrent
import Text.Printf

-- The 'Net' monad, a wrapper over IO, carrying the bot's immutable state.
type Net = ReaderT Bot IO
data Bot = Bot { socket :: Handle }

-- Set up actions to run on start and end, and run the main loop
main :: IO ()
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop st    = runReaderT run st

-- Connect to the server and return the initial bot state
connect :: IO Bot
connect = notify $ do
    h <- connectTo server (PortNumber (fromIntegral port))
    hSetBuffering h NoBuffering
    return (Bot h)
  where
    notify a = bracket_
        (printf "Connecting to %s... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :" ++ real)
    asks socket >>= listen
 
-- Process each line from the IRC server, and also check for new Telegram messages
listen :: Handle -> Net ()
listen h = forever $ do
    s <- init `fmap` io (hGetLine h)
    --io $ putStrLn s -- Print recieved message
    if ping s then pong s else command (clean s) -- Respond to ping or dispatch an IRC command
    eval (code s) -- Do something with specific IRC channel internal codes
    if ("PRIVMSG" `isInfixOf` s) && (target s == chan) then -- Send channel messages to telegram
        do
            io $ sendMessage $ message s
            printLog "IRC" $ message s
        else return ()
  where
    forever a   = a >> forever a
    clean       = drop 1 . dropWhile (/= ':') . drop 1 -- Clean IRC PRIVMSG
    code        = take 3 . drop 1 . dropWhile (/= ' ') -- IRC (server) message code
    target      = drop 1 . dropWhile (/= ' ') . init . takeWhile (/= ':') . drop 1 . dropWhile (/= ' ') -- IRC PRIVMSG target
    sender      = takeWhile (/= '!') . drop 1 -- IRC PRIVMSG message sender
    ping x      = "PING :" `isPrefixOf` x
    pong x      = write "PONG" (':' : drop 6 x)
    message s   = "<" ++ sender s ++ "> " ++ clean s

-- Check for new telegram messages in the
-- background and send them to IRC
telegramThread :: Integer -> Net ()
telegramThread o = do
    updates <- io $ getMessages o -- Get new telegram messages
    let offset = fst updates -- Store offset for telegram messages
    --io $ putStrLn ("New offset: " ++ (show offset)) -- Print new offset
    mapM (\x -> printLog "TG" x) (snd updates) -- Print all messages
    mapM privmsg $ snd updates -- Send all new telegram messages to the IRC channel
    io $ threadDelay 5000000 -- Sleep for 5 seconds
    telegramThread offset

-- Print recieved Telegram or IRC message
printLog :: String -> String -> Net ()
printLog sender message = io $ putStrLn ("{" ++ sender ++ " -> " ++ getTarget sender ++ "} " ++ message)
    where
        getTarget "TG"  = "IRC"
        getTarget _     = "TG"

-- Dispatch an IRC command (channel message)
command :: String -> Net ()
--command "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
--command x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
command _ = return () -- ignore everything else

-- Dispatch an IRC internal server code
-- Shouldn't need to modify this
eval :: String -> Net ()
eval "433" = write "NICK" (nick ++ "_") -- Nick in use. Send new nickname
eval "376" = do 
    write "JOIN" chan -- Join a channel after MOTD is recieved (some IRC networks need this)
    forkIO $ telegramThread 1 -- Start telegram thread once finally connected to IRC. Send default offset 1
    io $ putStrLn ("Started Telegram thread")
    return ()
eval _ = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the IRC server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    --io $ printf    "> %s %s\n" s t

-- Convenience
io :: IO a -> Net a
io = liftIO