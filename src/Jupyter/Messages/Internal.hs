{-|
Module      : Jupyter.Messages.Internal
Description : Metadata and message headers for the Jupyter messaging protocol, used internally.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

This module contains type and class definitions pertaining to handling Jupyter messages
internally. The types defined here are generally useful for the @jupyter@ library
but will not be useful for users of the library.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Jupyter.Messages.Internal (
    -- * Message Metadata
    MessageHeader(..),
    Username(..),
    MessageType(..),
    IsMessage(..),
  ) where

-- Imports from 'base'
import           Control.Applicative (Alternative(..))
import           GHC.Exts (IsString)

-- Imports from 'aeson'
import           Data.Aeson (FromJSON, ToJSON)
import           Data.Aeson.Types (Parser, Object)

-- Imports from 'bytestring'
import           Data.ByteString (ByteString)

-- Imports from 'text'
import           Data.Text (Text)

-- Imports from 'jupyter'
import           Jupyter.UUID (UUID)

-- | A message header with some metadata.
--
-- The metadata has a variety of important information that pertains to the session maintained
-- between the client and the frontend. In addition to metadata, the message headers are used to
-- establish relationships between the messages: in particular, the message parent is used to
-- determine which request a reply corresponds to.
--
-- In addition, the message type is sent as a string with the message header.
--
-- For more information about the message headers, read the section of the Jupyter messaging
-- protocol about the
-- <https://jupyter-client.readthedocs.io/en/latest/messaging.html#the-wire-protocol wire protocol>.
data MessageHeader =
       MessageHeader
         { messageIdentifiers :: [ByteString]
         -- ^ The identifiers sent with the message. These identifiers come at the front of the message, and
         -- are ZeroMQ routing prefix, which can be zero or more socket identities. These are used, for
         -- instance, to know where to send /stdin/ requests (see the messaging spec).
         , messageParent :: Maybe MessageHeader
         -- ^ The parent header, if present. The parent header is used to establish relationships between
         -- request and reply messages and outputs published in response to requests.
         , messageMetadata :: Object
         -- ^ A free-form dict of metadata.
         , messageId :: UUID                     -- ^ A unique message UUID.
         , messageSession :: UUID                -- ^ A unique session UUID.
         , messageUsername :: Username           -- ^ The user who sent this message.
         , messageType :: MessageType
           -- ^ The type of this message. This is stored as a string, and determines how to parse the content
           -- and what to do with the message once it is parsed.
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

  -- | Get a parser for this message type.
  --
  -- If this message type does not correspond to one of the constructors for the data type @v@, then
  -- return 'Nothing'. Otherwise, return a parser that parses the message body into the given
  -- datatype.
  --
  -- This is a slightly unusual but necessary interface, because the message type and the message body
  -- come in separate bytestrings, as they are separate blobs sent on the communication sockets. Thus,
  -- we must look first at the message type, and choose the JSON parser based on the message type.
  parseMessageContent :: MessageType -> Maybe (Object -> Parser v)

-- | Provide an 'IsMessage' instance for the sum of two types which have 'IsMessage' instances.
--
-- This is particularly useful for dealing with the /shell/ and /iopub/ socket, where kernels and
-- clients can receive different types of messages on one socket (kernels can receive 'Comm' and
-- 'ClientRequest' messages on the /shell/ socket, while clients can receive 'KernelOutput' and
-- 'Comm' messages on the /iopub/ socket).
instance (IsMessage v1, IsMessage v2) => IsMessage (Either v1 v2) where
  getMessageType = either getMessageType getMessageType
  parseMessageContent msgType =
    fmap3 Left (parseMessageContent msgType) <|> fmap3 Right (parseMessageContent msgType)
    where
      -- I can't believe I get to write `fmap . fmap . fmap` and have it be meaningful. Don't think too
      -- hard, just look at the type signature, which is specialized to the functors that I care about in
      -- this case! (Maybe, (->) Object, and Parser, to be specific).
      fmap3 :: (a -> b) -> Maybe (Object -> Parser a) -> Maybe (Object -> Parser b)
      fmap3 = fmap . fmap . fmap
