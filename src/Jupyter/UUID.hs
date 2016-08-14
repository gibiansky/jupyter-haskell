{-|
Module      : Jupyter.UUID
Description : UUID generator and type.
Copyright   : (c) Andrew Gibiansky, 2016
License     : MIT
Maintainer  : andrew.gibiansky@gmail.com
Stability   : stable
Portability : POSIX

Generate, parse, and pretty print UUIDs for use with Jupyter. 

UUIDs are stored internally as just strings, rather than parsed UUIDs, because Jupyter cares about
things like dashes and capitalization -- if they are not identical to the ones Jupyter send, Jupyter
will not recognize them. Thus, we treat them as strings rather than UUIDs to be parsed to avoid
modifying them in any way.
-}
module Jupyter.UUID (
    -- * UUID data type and conversions
    UUID,
    uuidToString,
    uuidFromString,

    -- * Generating UUIDs
    random,
    randoms,
    ) where

-- Imports from 'base'
import           Control.Monad (mzero, replicateM)

-- Imports from 'aeson'
import           Data.Aeson

-- Imports from 'text'
import           Data.Text (pack)

-- Imports from 'uuid'
import           Data.UUID.V4 (nextRandom)

-- | A UUID (universally unique identifier).
newtype UUID =
        -- We use an internal string representation because for the purposes of Jupyter, it
        -- matters whether the letters are uppercase or lowercase and whether the dashes are
        -- present in the correct locations. For the purposes of new UUIDs, it does not
        -- matter, but Jupyter expects UUIDs passed to kernels to be returned unchanged, so we
        -- cannot actually parse them.
         UUID String
  deriving (Show, Read, Eq, Ord)

-- | Convert a 'UUID' to a 'String' for transmission or display.
uuidToString :: UUID -> String
uuidToString (UUID uuid) = uuid

-- | Convert a 'String' to a 'UUID'.
uuidFromString :: String -> UUID
uuidFromString = UUID

-- | Generate a list of random UUIDs.
randoms :: Int      -- ^ Number of UUIDs to generate.
        -> IO [UUID]
randoms n = replicateM n random

-- | Generate a single random UUID.
random :: IO UUID
random = fmap (UUID . show) nextRandom

-- Allows reading and writing UUIDs as Strings in JSON.
instance FromJSON UUID where
  parseJSON val@(String _) = fmap UUID (parseJSON val)

  -- UUIDs must be Strings.
  parseJSON _ = mzero

instance ToJSON UUID where
  -- Extract the string from the UUID.
  toJSON (UUID str) = String $ pack str
