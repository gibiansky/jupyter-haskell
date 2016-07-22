{-|
Module      : Jupyter.Kernel
Description : Functions for creating and serving a Jupyter kernel.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

The 'Jupyter.Kernel.Parser' module is responsible for parsing incoming messages from the client and converting them
into the types that @jupyter-kernel@ kernels can understand. The main entrypoint is 'parseClientMessage', which, given
all the data obtained from the ZeroMQ socket, parses the JSON blobs and decodes them as necessary (indicated by the message type).

-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module Jupyter.Kernel.Parser (parseClientMessage) where

import           Data.ByteString (ByteString)
import           Data.Aeson ((.:), (.:?), Object, eitherDecodeStrict')
import           Data.Aeson.Types (Parser, parseEither)

import qualified Data.Text as T

import           Jupyter.Messages (Client, Message(..), ClientRequest(..), Comm(..), DetailLevel(..),
                                   HistorySearchOptions(..), Restart(..), HistoryOptions(..),
                                   ExecuteOptions(..), HistoryRangeOptions(..), HistoryAccessType(..))
import           Jupyter.Messages.Metadata (MessageHeader(..), MessageType(..))

-- | Given all the data obtained from the ZeroMQ socket, attempt to parse the message header and
-- message (as directed by the message type).
--
-- This function only parses messages sent from a frontend to a kernel.
parseClientMessage :: [ByteString]
                   -> ByteString
                   -> ByteString
                   -> ByteString
                   -> ByteString
                   -> Either String (Message Client)
parseClientMessage identifiers headerData parentHeaderData metadata content = do
  header <- parseHeader identifiers headerData parentHeaderData metadata
  clientMessage <- parseClientMessageContent (messageType header) content
  return $ clientMessage header

-- | Attempt to parse a message header.
parseHeader :: [ByteString]
            -> ByteString
            -> ByteString
            -> ByteString
            -> Either String MessageHeader
parseHeader identifiers headerData parentHeaderData metadata = do
  header <- eitherDecodeStrict' headerData

  let messageIdentifiers = identifiers
  messageParent <- if parentHeaderData == "{}"
                    then return Nothing
                    else Just <$> parseHeader identifiers parentHeaderData "{}" metadata
  messageType <- parseEither (.: "msg_type") header
  messageUsername <- parseEither (.: "username") header
  messageId <- parseEither (.: "msg_id") header
  messageSession <- parseEither (.: "session") header
  messageMetadata <- eitherDecodeStrict' metadata

  return MessageHeader { .. }

parseClientMessageContent :: MessageType -> ByteString -> Either String (MessageHeader -> Message Client)
parseClientMessageContent (MessageType messageType) content = do
  decoded <- eitherDecodeStrict' content
  let parse parser = parseEither parser decoded
      parseClientRequest parser = flip ClientRequest <$> parse parser
      parseComm parser = flip Comm <$> parse parser
  case messageType of
    "execute_request"     -> parseClientRequest executeRequestParser
    "inspect_request"     -> parseClientRequest inspectRequestParser
    "history_request"     -> parseClientRequest historyRequestParser
    "complete_request"    -> parseClientRequest completeRequestParser
    "is_complete_request" -> parseClientRequest isCompleteRequestParser
    "comm_info_request"   -> parseClientRequest commInfoRequestParser
    "shutdown_request"    -> parseClientRequest shutdownRequestParser
    "connect_request"     -> Right $ flip ClientRequest ConnectRequest
    "kernel_info_request" -> Right $ flip ClientRequest KernelInfoRequest

    "comm_open"           -> parseComm commOpenParser
    "comm_msg"            -> parseComm commMsgParser
    "comm_close"          -> parseComm commCloseParser

    _                     -> Left $ "Could not parse unknown message type: " ++ T.unpack messageType


executeRequestParser :: Object -> Parser ClientRequest
executeRequestParser o = ExecuteRequest <$> o .: "code"
                                        <*> (ExecuteOptions <$> o .: "silent"
                                                            <*> o .: "store_history"
                                                            <*> o .: "allow_stdin"
                                                            <*> o .: "stop_on_error")
inspectRequestParser :: Object -> Parser ClientRequest
inspectRequestParser o = do
  detailLevelNum <- o .: "detail_level"
  detailLevel <- case detailLevelNum :: Int of
                   0 -> return DetailLow
                   1 -> return DetailHigh
                   _ -> fail $ "Unknown detail level in inspect_request: " ++ show detailLevelNum
  InspectRequest <$> o .: "code" <*> o .: "cursor_pos" <*> pure detailLevel

historyRequestParser :: Object -> Parser ClientRequest
historyRequestParser o =
  HistoryRequest <$> (HistoryOptions <$> o .: "output" <*> o .: "raw" <*> parseHistoryAccessType o)

parseHistoryAccessType :: Object -> Parser HistoryAccessType
parseHistoryAccessType o = do
  accessType <- o .: "hist_access_type"
  case accessType of
    "range" -> HistoryRange <$> (HistoryRangeOptions <$> o .: "session"
                                                      <*> o .: "start"
                                                      <*> o .: "stop")
    "tail" -> HistoryTail <$> o .: "n"
    "search" -> HistorySearch <$> (HistorySearchOptions <$> o .: "n"
                                                        <*> o .: "pattern"
                                                        <*> o .: "unique")
    _ -> fail $ "Unknown history access type in hist_access_type: " ++ accessType

completeRequestParser :: Object -> Parser ClientRequest
completeRequestParser o = CompleteRequest <$> o .: "code" <*> o .: "cursor_pos"

isCompleteRequestParser :: Object -> Parser ClientRequest
isCompleteRequestParser o = IsCompleteRequest <$> o .: "code"

commInfoRequestParser :: Object -> Parser ClientRequest
commInfoRequestParser o = CommInfoRequest <$> o .:? "target_name"

shutdownRequestParser :: Object -> Parser ClientRequest
shutdownRequestParser o = do
  restart <- o .: "restart"
  pure $ ShutdownRequest $ if restart
                             then Restart
                             else NoRestart

commOpenParser :: Object -> Parser Comm
commOpenParser o = CommOpen <$> o .: "comm_id"
                            <*> o .: "data"
                            <*> o .: "target_name"
                            <*> o .:? "target_module"

commMsgParser :: Object -> Parser Comm
commMsgParser o = CommMessage <$> o .: "comm_id" <*> o .: "data"

commCloseParser :: Object -> Parser Comm
commCloseParser o = CommClose <$> o .: "comm_id" <*> o .: "data"
