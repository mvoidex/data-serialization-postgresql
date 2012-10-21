{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | This module defines serialization for postgres
--
-- Since there is no 'source' type, serialization just defines the way to combine @RowParser a@ and @a -> [Action]@
--
module Data.Serialization.Postgresql (
    ToActions(..),
    Postgresable,
    pgRow, pgField,
    rowParser,
    rowWriter

    ) where

import Control.Applicative
import Control.Monad.Writer
import Data.Maybe (fromMaybe)

import Data.Serialization
import Data.Serialization.Serialize
import Data.Serialization.Deserialize

import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow

newtype ToActions a = ToActions {
    runToActions :: WriterT [Action] Maybe a }
        deriving (Functor, Applicative, Alternative, Monad)

type Postgresable a = Serializable () ToActions RowParser a

-- | Postgresable from 'ToRow' and 'FromRow'
pgRow :: (ToRow a, FromRow a) => Postgresable a
pgRow = serializable toRow' (Deserialize fromRow) where
    toRow' = Serialize $ ToActions . tell . toRow

-- | Postgresable from 'ToField' and 'FromField'
pgField :: (ToField a, FromField a) => Postgresable a
pgField = serializable toField' (Deserialize field) where
    toField' = Serialize $ ToActions . tell . return . toField

-- | 'FromRow' from 'Postgresable'
rowParser :: Postgresable a -> RowParser a
rowParser = runDeserialize . deserializer

-- | 'ToRow' from 'Postgresable'
rowWriter :: Postgresable a -> a -> [Action]
rowWriter p = fromMaybe [] . execWriterT . runToActions . runSerialize (serializer p)
