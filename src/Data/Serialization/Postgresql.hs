{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, MultiParamTypeClasses, FlexibleInstances, DeriveFunctor, ConstraintKinds, FlexibleContexts, OverlappingInstances #-}

-- | This module defines serialization for postgres
--
-- Simply derive your type from @Generic@, @Decoding FromFields@ and @Encoding ToFields@.
-- If you want some field not to present in query, use @OptField@ wrapper.
-- 
-- >data Test = Test {
-- >    testInt :: Int, -- ^ This field must be in query
-- >    testOptional :: Maybe Double, -- ^ This field must be in query, but it's nullable
-- >    testString :: OptField String } -- ^ This may not be in query
-- >        deriving (Generic, Show)
-- >
-- >instance Serializable (Decoding FromFields) Test
-- >instance Serializable (Encoding ToFields) Test
-- 
-- You can collect field names with @gfields@ function:
-- 
-- >testFields :: [String]
-- >testFields = fields (gfields :: Fields Test)
-- >-- ["testInt","testOptional","testString"]
-- 
-- Example of usage:
-- 
-- >create :: IO ()
-- >create = do
-- >    con <- connect testcon
-- >    execute_ con "create table test (id integer, value double precision, name text)"
-- >    return ()
-- >
-- >runInsert :: IO ()
-- >runInsert = do
-- >    con <- connect testcon
-- >    let
-- >        acts = either undefined id $ encodeRow (Test 123 Nothing (Has "Hello, world!"))
-- >    print acts
-- >    -- [Plain "123",Plain "null",Escape "Hello, world!"]
-- >    execute con "insert into test values (?, ?, ?)" acts
-- >    return ()
-- >
-- >runSelect :: IO ()
-- >runSelect = do
-- >    con <- connect testcon
-- >    [ff] <- query_ con "select * from test limit 1"
-- >    print $ (decodeRow ff :: Either String Test)
-- >    -- Right (Test {testInt = 123, testOptional = Nothing, testString = Has "Hello, world!"})
--
module Data.Serialization.Postgresql (
    -- * Encode/decode row
    encodeRow, decodeRow,
    -- * Meta info
    Fields(..), gfields,
    -- * Serialize
    ToFields(..),
    encodeField, encodeOptField,
    -- * Deserialize
    FromFields(..),
    decodeField, decodeOptField,
    -- * Utiliity
    AnyField(..), OptField(..)
    ) where

import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import Data.Maybe (catMaybes)
import Data.Function (fix)
import Data.ByteString (ByteString)
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.Ok

import GHC.Generics

import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec

-- | Encode row
encodeRow :: (Serializable (Encoding ToFields) a) => a -> Either String [Action]
encodeRow x = fmap catMaybes $ encode (ser' x) x where
    ser' :: (Serializable (Encoding ToFields) a) => a -> Encoding ToFields a
    ser' _ = ser

-- | Decode row
decodeRow :: (Serializable (Decoding FromFields) a) => [AnyField] -> Either String a
decodeRow f = fix $ \r -> decode (ser' r) f where
    ser' :: (Serializable (Decoding FromFields) a) => Either String a -> Decoding FromFields a
    ser' _ = ser

-- | Collect field names
newtype Fields a = Fields { fields :: [String] } deriving (Eq, Ord, Read, Show)

instance Combine Fields where
    (Fields l) .*. (Fields r) = Fields (l ++ r)
    (Fields l) .+. (Fields r) = Fields (l ++ r)
    (Fields m) .:. _ = Fields m

instance GenericCombine Fields

instance Selector c => GenericSerializable Fields (Stor c a) where
    gser = fix $ \r -> Fields (return $ storName $ dummy r) where
        dummy :: Fields (Stor c a) -> Stor c a
        dummy _ = undefined

-- | Get type fields
gfields :: (Generic a, GenIsoDerivable (GenericSerializable Fields) a) => Fields a
gfields = gser .:. giso

-- | Serialize to list of @Action@
newtype ToFields a = ToFields { toFields :: EncodeTo [Maybe Action] a }
    deriving (Functor, Applicative, Alternative, Monad, MonadWriter [Maybe Action], MonadError String, Generic)

instance GenericEncode ToFields
instance Serializer ToFields [Maybe Action]

-- | Encode one field
encodeField :: ToField a => Encoding ToFields a
encodeField = encodePart $ return . return . Just . toField

-- | Encode @OptField@
encodeOptField :: ToField a => Encoding ToFields (OptField a)
encodeOptField = encodePart $ return . return . opt Nothing Just . fmap toField

instance ToField a => Serializable (Encoding ToFields) a where
    ser = encodeField

instance ToField a => Serializable (Encoding ToFields) (OptField a) where
    ser = encodeOptField

-- | Deserialize from list of @AnyField@
newtype FromFields a = FromFields { fromFields :: DecodeFrom (Int, [AnyField]) a }
    deriving (Functor, Applicative, Alternative, Monad, MonadState (Int, [AnyField]), MonadError String, Generic)

instance GenericDecode FromFields
instance Deserializer FromFields [AnyField] where
    deserialize (FromFields p) s = decodeFrom p (1, s)
    deserializeEof _ = do
        (_, fs) <- get
        when (not $ null fs) $ throwError "EOF expected"
    deserializeTail = fmap snd get

-- | Decode one field
decodeField :: FromField a => Decoding FromFields a
decodeField = Decoding $ do
    (i, fs) <- get
    case find ((== i) . tableColumn . anyFieldMeta) fs of
        Nothing -> throwError $ "Column with index " ++ show i ++ " not found"
        Just f -> do
            v <- either throwError return $ fromAny f
            put (succ i, fs)
            return v

-- | Decode @OptField@, field that may not present in query result
decodeOptField :: FromField a => Decoding FromFields (OptField a)
decodeOptField = Decoding $ do
    (i, fs) <- get
    case find ((== i) . tableColumn . anyFieldMeta) fs of
        Nothing -> do
            put (succ i, fs)
            return HasNo
        Just f -> do
            v <- either throwError return $ fromAny f
            put (succ i, fs)
            return (Has v)

instance FromField a => Serializable (Decoding FromFields) a where
    ser = decodeField

instance FromField a => Serializable (Decoding FromFields) (OptField a) where
    ser = decodeOptField

-- | Represents any field
data AnyField = AnyField {
    anyFieldMeta :: Field,
    anyFieldValue :: Maybe ByteString }

instance Show AnyField where
    show (AnyField f m) = show m

instance Eq AnyField where
    (AnyField lm lv) == (AnyField rm rv) = and [
        typename lm == typename rm,
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
