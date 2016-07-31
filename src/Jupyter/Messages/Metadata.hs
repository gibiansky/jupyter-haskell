{-|
Module      : Jupyter.Messages.Metadata
Description : Metadata and message headers for the Jupyter messaging protocol.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module contains type and class definitions pertaining to handling Jupyter messages
internally. The types defined here are generally useful for the @jupyter-kernel@ library
but will not be useful for users of the library.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jupyter.Messages.Metadata (
    -- * Message Metadata
    MessageHeader(..),
    Username(..),
    MessageType(..),
    IsMessage(..),
  ) where

import           Data.Aeson (FromJSON, ToJSON)
import           Data.Aeson.Types (Parser, Object)
import           Data.ByteString (ByteString)
import           Data.Map (Map)
import           Data.Text (Text)
import           Data.Proxy (Proxy(..))
import           Control.Applicative (Alternative(..))

import           GHC.Exts (IsString)

import           Jupyter.UUID (UUID)

-- | A message header with some metadata.
--
-- The metadata has a variety of important information that pertains to the
-- session maintained between the client and the frontend. In addition to metadata,
-- the message headers are used to establish relationships between the messages:
-- for instance, the
-- message parent is used to determine which request a reply corresponds to.
--
-- In addition, the message type is sent as a string with the message header.
--
-- For more information about the message headers, read the section of the Jupyter messaging
-- protocol about the <https://jupyter-client.readthedocs.io/en/latest/messaging.html#the-wire-protocol wire protocol>.
data MessageHeader =
       MessageHeader
         { messageIdentifiers :: [ByteString]
         -- ^ The identifiers sent with the message. These identifiers
         -- come at the front of the message, and are ZeroMQ routing prefix,
         -- which can be zero or more socket identities. 
         , messageParent :: Maybe MessageHeader
         -- ^ The parent header, if present.
         -- The parent header is used to establish relationships between request and reply
         -- messages and outputs published in response to requests.
         , messageMetadata :: Object
         -- ^ A free-form dict of metadata.
         , messageId :: UUID                     -- ^ A unique message UUID.
         , messageSession :: UUID                -- ^ A unique session UUID.
         , messageUsername :: Username           -- ^ The user who sent this message.
         , messageType :: MessageType 
         -- ^ The type of this message. This is stored as a string,
         -- and determines how to parse the content and what to do with the message
         -- once it is parsed.
         }
  deriving (Eq, Show)

-- | A username represented as 'Text', part of the 'MessageHeader'.
newtype Username = Username Text
  deriving (Eq, Ord, Show, IsString, FromJSON, ToJSON)

-- | The type of a message, internally stored as a string.
--
-- Examples include @execute_request@, @comm_open@, and @display_data@. 
newtype MessageType = MessageType { messageTypeText :: Text }
  deriving (Eq, Ord, Show, FromJSON, ToJSON, IsString)

-- | Jupyter messages are represented as a variety of datatypes, depending on where in the messaging
-- protocol the message is used (for instance, 'Jupyter.Messages.ClientRequest' or
-- 'Jupyter.Messages.Comm').
--
-- Given any message, however, you need to be able to get a 'MessageType' for it, so that when
-- encoding the message onto the wire you can include the message type in the header. The
-- 'IsMessage' typeclass provides a single method, 'getMessageType', which gets the message type of
-- any Jupyter message.
class ToJSON v => IsMessage v where
  -- | Get the message type for a Jupyter message.
  getMessageType :: v -> MessageType

  parseMessageContent :: MessageType -> Maybe (Object -> Parser v)

instance (IsMessage v1, IsMessage v2) => IsMessage (Either v1 v2) where
  getMessageType = either getMessageType getMessageType
  parseMessageContent msgType = 
    fmap3 Left (parseMessageContent msgType) <|> fmap3 Right (parseMessageContent msgType)
    where
      fmap3 :: (a -> b) -> Maybe (Object -> Parser a) -> Maybe (Object -> Parser b)
      fmap3 = fmap . fmap . fmap
