{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, DeriveFunctor, FlexibleContexts, OverlappingInstances #-}

module Data.Serialization.Postgresql.Types (
    ColumnType(..),
    Fields(..),
    -- * Utiliity
    AnyField(..), OptField(..),
    fromAny, opt
    ) where

import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time
import Data.Serialization
import Data.Function (fix)
import GHC.Generics

import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.Ok
import Database.PostgreSQL.Simple.Time

-- | Get column type
class ColumnType a where
    columnType :: a -> String

instance ColumnType Int where
    columnType _ = "integer"

instance ColumnType Float where
    columnType _ = "real"

instance ColumnType Double where
    columnType _ = "double precision"

instance ColumnType Bool where
    columnType _ = "boolean"

instance ColumnType String where
    columnType _ = "text"

instance ColumnType Text where
    columnType _ = "text"

instance ColumnType ByteString where
    columnType _ = "bytea"

instance ColumnType UTCTime where
    columnType _ = "timestamptz"

instance ColumnType ZonedTime where
    columnType _ = "timestamptz"

instance ColumnType LocalTime where
    columnType _ = "timestamp"

instance ColumnType Day where
    columnType _ = "date"

instance ColumnType TimeOfDay where
    columnType _ = "time"

instance ColumnType UTCTimestamp where
    columnType _ = "timestamptz"

instance ColumnType ZonedTimestamp where
    columnType _ = "timestamptz"

instance ColumnType LocalTimestamp where
    columnType _ = "timestamp"

instance ColumnType Date where
    columnType _ = "date"

instance ColumnType a => ColumnType (Maybe a) where
    columnType = columnType . fromJust

instance ColumnType a => ColumnType (OptField a) where
    columnType = columnType . opt undefined id

-- | Metadata collector
data Fields a = Fields { getFields :: [(String, String)] } deriving (Eq, Ord, Read, Show)

instance Combine Fields where
    (Fields l) .*. (Fields r) = Fields (l ++ r)
    (Fields l) .+. (Fields r) = Fields (l ++ r)
    (Fields f) .:. _ = Fields f

instance CombineM Fields where
    (Fields l) .|. (Fields r) = Fields (l ++ r)
    (Fields fs) .>> f = Fields fs
    pures _ = Fields []
    fails _ = Fields []
    p .?. (Fields fs) = Fields fs
    (Fields fs) .%. _ = Fields fs

instance GenericCombine Fields where
    genericStor name (Fields (("", tp):fs)) = Fields $ (name, tp) : fs
    genericStor _ _ = error "Impossible happened"

instance ColumnType a => Serializable Fields a where
    ser = fix $ \r -> Fields [("", columnType $ dummy r)] where
        dummy :: Fields a -> a
        dummy _ = undefined

-- | Represents any field
data AnyField = AnyField {
    anyFieldMeta :: Field,
    anyFieldValue :: Maybe ByteString }

instance Show AnyField where
    show (AnyField f m) = show m

instance Eq AnyField where
    (AnyField lm lv) == (AnyField rm rv) = and [
        typename lm == typename rm,
        name lm == name rm,
        tableColumn lm == tableColumn rm,
        lv == rv]

-- | Parse @AnyField@ to concrete type
fromAny :: (FromField r) => AnyField -> Either String r
fromAny (AnyField f m) = case fromField f m of
    Errors e -> Left $ show e
    Ok x -> Right x

instance FromField AnyField where
    fromField f m = return $ AnyField f m

-- | Optional field, used when field may not present in query
data OptField a = Has a | HasNo deriving (Eq, Ord, Read, Show, Functor)

-- | Fold @OptField@
opt :: b -> (a -> b) -> OptField a -> b
opt no _ HasNo = no
opt _ has (Has x) = has x
