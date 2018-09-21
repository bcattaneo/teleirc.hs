module Teleirc where

import Telegram
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import Data.List
import Network
import System.IO
import System.Exit
import Control.Arrow
import Control.Monad.Reader
import Control.Exception
import Text.Printf

{-

    Requirements:
    - http-conduit

    TODO:
    - Reconnection?
    - Nickserv auth?
    - Support and upload somewhere: photos, documents, and so on
    - JSON config or user input config (or both)
    - Multigroup support?
-}

-- IRC configuration
server = "irc.rizon.net"
port   = 6667
chan   = "##pepe"
nick   = "teleirc"
real   = "teleirc.hs bot"

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
        (printf "Connecting to %s ... " server >> hFlush stdout)
        (putStrLn "done.")
        a

-- We're in the Net monad now, so we've connected successfully
-- Join a channel, and start processing commands
run :: Net ()
run = do
    write "NICK" nick
    write "USER" (nick ++ " 0 * :" ++ real)
    handle <- asks socket
    listen handle 1 0 -- Main loop. Parameters: conn. handle, offset, count
 
-- Process each line from the IRC server, and also check for new Telegram messages
listen :: Handle -> Integer -> Integer -> Net ()
listen h o c = do
    let newCount = c + 1 -- TODO: Use this count as a timer, to start recieving Telegram messages at some point (not right away)
    s <- init `fmap` io (hGetLine h)
    --io (putStrLn s) -- Print recieved message
    updates <- io (getMessages o) -- Get new telegram messages
    let newOffset = fst updates -- Store new offset for telegram messages
    io ((mapM (\x -> privmsg $ x)) (snd updates)) -- Send all new telegram messages to the IRC channel
    if ping s then pong s else command (clean s) -- Respond to ping or dispatch an IRC command
    eval (code s) -- Do something with specific IRC channel internal codes
    if (isInfixOf "PRIVMSG" s) && (target s == chan) then io (sendMessage ("<" ++ sender s ++ "> " ++ clean s)) else return () -- Send channel messages to telegram
    listen h newOffset newCount -- Back to loop
  where
    forever a   = a >> forever a
    clean       = drop 1 . dropWhile (/= ':') . drop 1
    code        = take 3 . drop 1 . dropWhile (/= ' ')
    target      = drop 1 . dropWhile (/= ' ') . init . takeWhile (/= ':') . drop 1 . dropWhile (/= ' ')
    sender      = takeWhile (/= '!') . drop 1
    ping x      = "PING :" `isPrefixOf` x
    pong x      = write "PONG" (':' : drop 6 x)

-- Dispatch an IRC command (channel message)
command :: String -> Net ()
command "!quit" = write "QUIT" ":Exiting" >> io (exitWith ExitSuccess)
command "!join" = write "JOIN" chan
command x | "!id " `isPrefixOf` x = privmsg (drop 4 x)
command _ = return () -- ignore everything else

-- Dispatch an IRC internal server code
-- Shouldn't need to modify this
eval :: String -> Net ()
eval "433" = write "NICK" (nick ++ "_") -- Nick in use. Send new nickname
eval "376" = write "JOIN" chan -- Join a channel after MOTD is recieved (some IRC networks need this)
eval _ = return () -- ignore everything else

-- Send a privmsg to the current chan + server
privmsg :: String -> Net ()
privmsg s = write "PRIVMSG" (chan ++ " :" ++ s)

-- Send a message out to the IRC server we're currently connected to
write :: String -> String -> Net ()
write s t = do
    h <- asks socket
    io $ hPrintf h "%s %s\r\n" s t
    io $ printf    "> %s %s\n" s t

-- Convenience
io :: IO a -> Net a
io = liftIO