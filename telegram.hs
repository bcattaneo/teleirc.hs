{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}

module Telegram where

import Settings
import Data.Aeson
import Data.Aeson.TH
import Data.Maybe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Control.Monad.Reader

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
  , usernameC :: Maybe String
  , typeC :: String
  , all_members_are_administrators :: Maybe Bool
  } deriving (Show)

instance FromJSON Chat where
  parseJSON (Object v) =
    Chat <$> v .: "id"
         <*> v .: "title"
         <*> v .:? "username"
         <*> v .: "type"
         <*> v .:? "all_members_are_administrators"

-- Thumb JSON data types
data Thumb = Thumb
  { file_idT :: String
  , file_sizeT :: Integer
  , widthT :: Integer
  , heightT :: Integer
  } deriving (Show)

instance FromJSON Thumb where
  parseJSON (Object v) =
    Thumb   <$> v .: "file_id"
            <*> v .: "file_size"
            <*> v .: "width"
            <*> v .: "height"

-- Animation JSON data types
data Animation = Animation
  { file_nameA :: String
  , mime_typeA :: String
  , durationA :: Integer
  , widthA :: Integer
  , heightA :: Integer
  , file_idA :: String
  , file_sizeA :: Integer
  , thumbA :: Thumb
  } deriving (Show)

instance FromJSON Animation where
  parseJSON (Object v) =
    Animation <$> v .: "file_name"
         <*> v .: "mime_type"
         <*> v .: "duration"
         <*> v .: "width"
         <*> v .: "height"
         <*> v .: "file_id"
         <*> v .: "file_size"
         <*> v .: "thumb"

-- Audio JSON data types
data Audio = Audio
  { durationA2 :: Integer
  , mime_typeA2 :: String
  , titleA2 :: String
  , performer :: String
  , thumbA2 :: Thumb
  , file_idA2 :: String
  , file_sizeA2 :: Integer
  } deriving (Show)

instance FromJSON Audio where
  parseJSON (Object v) =
    Audio <$> v .: "duration"
         <*> v .: "mime_type"
         <*> v .: "title"
         <*> v .: "performer"
         <*> v .: "thumb"
         <*> v .: "file_id"
         <*> v .: "file_size"

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
          <*> v .: "height"

-- Document JSON data types
data Document = Document
  { file_name :: String
  , mime_type :: String
  , file_idD :: String
  , file_sizeD :: Integer
  , thumbD :: Maybe Thumb
  } deriving (Show)

instance FromJSON Document where
  parseJSON (Object v) =
    Document <$> v .: "file_name"
         <*> v .: "mime_type"
         <*> v .: "file_id"
         <*> v .: "file_size"
         <*> v .:? "thumb"

-- Sticker JSON data types
data Sticker = Sticker
  { widthS :: Integer
  , heightS :: Integer
  , emoji :: String
  , set_name :: String
  , thumb :: Thumb
  , file_idS :: String
  , file_sizeS :: Integer
  } deriving (Show)

instance FromJSON Sticker where
  parseJSON (Object v) =
    Sticker <$> v .: "width"
            <*> v .: "height"
            <*> v .: "emoji"
            <*> v .: "set_name"
            <*> v .: "thumb"
            <*> v .: "file_id"
            <*> v .: "file_size"

-- Voice JSON data types
data Voice = Voice
  { duration :: Integer
  , mime_typeV :: String
  , file_idV :: String
  , file_sizeV :: Integer
  } deriving (Show)

instance FromJSON Voice where
  parseJSON (Object v) =
    Voice   <$> v .: "duration"
            <*> v .: "mime_type"
            <*> v .: "file_id"
            <*> v .: "file_size"

-- Entities JSON data types
data Entities = Entities
  { offset :: Integer
  , lenghtE :: Integer
  , typeE :: String
  } deriving (Show)

instance FromJSON Entities where
  parseJSON (Object v) =
    Entities  <$> v .: "offset"
              <*> v .: "length"
              <*> v .: "type"

-- ForwardFrom JSON data types
data ForwardFrom = ForwardFrom
  { idF2 :: Integer
  , is_botF2 :: Bool
  , first_nameF2 :: String
  , usernameF2 :: String
  , language_codeF2 :: String
  } deriving (Show)

instance FromJSON ForwardFrom where
  parseJSON (Object v) =
    ForwardFrom <$> v .: "id"
         <*> v .: "is_bot"
         <*> v .: "first_name"
         <*> v .: "username"
         <*> v .: "language_code"

-- ReplyToMessage JSON data types
data ReplyToMessage = ReplyToMessage
  { message_idR :: Integer
  , fromR :: From
  , chatR :: Chat
  , dateR :: Integer
  , textR :: Maybe String
  , photoR :: Maybe [Photo]
  , documentR :: Maybe Document
  , stickerR :: Maybe Sticker
  , voiceR :: Maybe Voice
  , animationR :: Maybe Animation
  , audioR :: Maybe Audio
  , entitiesR :: Maybe [Entities]
  , forward_fromR :: Maybe ForwardFrom
  , forward_dateR :: Maybe Integer
  } deriving (Show)

instance FromJSON ReplyToMessage where
  parseJSON (Object v) =
    ReplyToMessage <$> v .: "message_id"
            <*> v .: "from"
            <*> v .: "chat"
            <*> v .: "date"
            <*> v .:? "text"
            <*> v .:? "photo"
            <*> v .:? "document"
            <*> v .:? "sticker"
            <*> v .:? "voice"
            <*> v .:? "animation"
            <*> v .:? "audio"
            <*> v .:? "entities"
            <*> v .:? "forward_from"
            <*> v .:? "forward_date"

-- Messages JSON data types
data Message = Message
  { message_id :: Integer
  , from :: From
  , chat :: Chat
  , date :: Integer
  , reply_to_message :: Maybe ReplyToMessage
  , text :: Maybe String
  , photo :: Maybe [Photo]
  , document :: Maybe Document
  , sticker :: Maybe Sticker
  , voice :: Maybe Voice
  , caption :: Maybe String
  , animation :: Maybe Animation
  , audio :: Maybe Audio
  , entities :: Maybe [Entities]
  , forward_from :: Maybe ForwardFrom
  , forward_date :: Maybe Integer
  } deriving (Show)

instance FromJSON Message where
  parseJSON (Object v) =
    Message <$> v .: "message_id"
            <*> v .: "from"
            <*> v .: "chat"
            <*> v .: "date"
            <*> v .:? "reply_to_message"
            <*> v .:? "text"
            <*> v .:? "photo"
            <*> v .:? "document"
            <*> v .:? "sticker"
            <*> v .:? "voice"
            <*> v .:? "caption"
            <*> v .:? "animation"
            <*> v .:? "audio"
            <*> v .:? "entities"
            <*> v .:? "forward_from"
            <*> v .:? "forward_date"

-- Update JSON data types
data Update = Update
  { update_id :: Integer
  , message :: Message
  } deriving (Show)

instance FromJSON Update where
  parseJSON (Object v) =
    Update <$> v .: "update_id"
           <*> v .: "message"

-- TelegramResponse JSON data types
data TelegramResponse = TelegramResponse
  { ok :: Bool
  , result :: [Update]
  } deriving (Show)

instance FromJSON TelegramResponse where
  parseJSON (Object v) =
    TelegramResponse <$> v .: "ok"
                     <*> v .: "result"

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
    request <- buildPostReq (url ++ token ++ "/sendMessage?chat_id=" ++ group ++ "&text=" ++ text)
    response <- httpLbs request manager
    return ()

-- Get updates for current group
getUpdates :: Integer -> IO (TelegramResponse)
getUpdates offset = do
    manager <- newManager tlsManagerSettings
    request <- buildGetReq (url ++ token ++ "/getUpdates?offset=" ++ (show offset))
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
parseMessages :: TelegramResponse -> (Integer,[String])
parseMessages (TelegramResponse { result = m }) = (newOffset allOffsets, from)
    where
      from = map (\(Update {message = (Message {from = (From {first_name = x}), text = y})}) -> "<" ++ x ++ "> " ++ (text y)) filtered
      allOffsets = map (\(Update {update_id = x}) -> x) m -- Get all current message IDs
      newOffset [] = 1 -- If no messages, define offset 1
      newOffset x = (last allOffsets) + 1 -- New offset: (last message id) + 1
      groupInt = read group :: Integer -- Convert group to Integer
      filtered = filter (\(Update {message = (Message {chat = (Chat {idC = x})})}) -> x == groupInt) m -- Keep only group messages (discard private or other group messages)
      text m = case m of
        Just x -> x
        Nothing -> "[Not text]"