{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Telegram where

import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Reader

-- Telegram configuration
token   = "385473023:AAEUVzUI4sKHF6SlK0b75WfUVZa5JFp7KUE"
group   = "-1001155064366"
url     = "https://api.telegram.org/bot"

-- From JSON data types
data From = From
  { idF :: Integer
  , is_bot :: Bool
  , first_name :: String
  , username :: String
  , language_code :: String
  } deriving (Show)

instance FromJSON From where
  parseJSON (Object v) =
    From <$> v .: "id"
         <*> v .: "is_bot"
         <*> v .: "first_name"
         <*> v .: "username"
         <*> v .: "language_code"

-- Chat JSON data types
data Chat = Chat
  { idC :: Integer
  , title :: String
  , usernameC :: String
  , typeC :: String
  } deriving (Show)

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat <$> v .: "id"
         <*> v .: "title"
         <*> v .: "username"
         <*> v .: "type"

-- Messages JSON data types
data Message = Message
  { message_id :: Integer
  , from :: From
  , chat :: Chat
  , date :: Integer
  , text :: Maybe String
  --, photo :: [Photo] -- # TODO
  } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "message_id"
            <*> v .: "from"
            <*> v .: "chat"
            <*> v .: "date"
            <*> v .:? "text" -- optional

-- Updates JSON data types
data Update = Update
  { update_id :: Integer
  , message :: Message
  } deriving (Show)

instance FromJSON Update where
  parseJSON (Object v) =
    Update <$> v .: "update_id"
           <*> v .: "message"

{-
# TODO
-- Photo JSON data types
data Photo = Photo
  { file_id :: String
  , file_size :: Integer
  , width :: Integer
  , height :: Integer
  } deriving (Show)

instance FromJSON Photo where
  parseJSON (Object v) =
    Photo <$> v .: "file_id"
          <*> v .: "file_size"
          <*> v .: "width"
          <*> v .: "date"
-}

-- Result JSON data types
data TelegramResponse = TelegramResponse
  { ok :: Bool
  , result :: [Update]
  } deriving (Show)

instance FromJSON TelegramResponse where
  parseJSON (Object v) =
    TelegramResponse <$> v .: "ok"
                     <*> v .: "result"

{-
$(deriveJSON defaultOptions ''TelegramResponse)
$(deriveJSON defaultOptions ''Update)
$(deriveJSON defaultOptions ''Message)
$(deriveJSON defaultOptions ''From)
$(deriveJSON defaultOptions ''Chat)
-}

-- Return POST request
buildPostReq :: String -> IO Request
buildPostReq url = do
    nakedRequest <- parseRequest url
    return (nakedRequest { method = "POST" })

-- Return GET request
buildGetReq :: String -> IO Request
buildGetReq url = do
    nakedRequest <- parseRequest url
    return (nakedRequest { method = "GET" })

-- Send a message to the telegram group
sendMessage :: String -> IO ()
sendMessage text = do
    manager <- newManager tlsManagerSettings
    request <- buildPostReq (url ++ token ++ "/" ++ "sendMessage?chat_id=" ++ group ++ "&text=" ++ text)
    response <- httpLbs request manager
    return ()

-- Get updates for current group
getUpdates :: Integer -> IO (TelegramResponse)
getUpdates offset = do
    manager <- newManager tlsManagerSettings
    request <- buildGetReq (url ++ token ++ "/" ++ "getUpdates" ++ "?offset=" ++ (show offset))
    response <- httpLbs request manager
    let Just obj = Data.Aeson.decode (responseBody response)
    return (obj :: TelegramResponse)

-- Return new telegram messages
getMessages :: Integer -> IO ((Integer,[String]))
getMessages offset = do
    response <- getUpdates offset
    let parsed = parseMessages response
    return (parsed)

-- Return last update_id (new offset, Integer) and all new messages as String
-- TODO: Do something with non-text messages (maybe re-upload them and post link here)
parseMessages :: TelegramResponse -> (Integer,[String])
parseMessages (TelegramResponse { result = m }) = (newOffset allOffsets, from)
    where
      from = map (\(Update {message = (Message {from = (From {first_name = x}), text = y})}) -> "<" ++ x ++ "> " ++ (text y)) m
      allOffsets = map (\(Update {update_id = x}) -> x) m
      newOffset [] = 1
      newOffset x = (last allOffsets) + 1
      text m = case m of
        Just x -> x
        Nothing -> "[Not text. SOON]" -- TODO