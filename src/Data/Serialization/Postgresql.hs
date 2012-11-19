{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, MultiParamTypeClasses, FlexibleInstances, ConstraintKinds, FlexibleContexts, OverlappingInstances, UndecidableInstances #-}

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
-- >instance Serializable Fields Test
-- >instance InTable Test where
-- >    table _ = "test"
-- 
-- You can specify names for columns:
--
-- >instance Serializable Pgser Test where
-- >    ser =
-- >        dat_ (ctor_ (
-- >            stor "id" ser .*.
-- >            stor "opti" ser .*.
-- >            stor "teststr" ser))
-- >        .:.
-- >        giso
--
-- Example:
--
-- >runCreate :: IO ()
-- >runCreate = do
-- >    con <- connect testcon
-- >    execute_ con "drop table test"
-- >    create con (Table :: Table Test)
-- >    return ()
-- >
-- >runInsert :: IO ()
-- >runInsert = do
-- >    con <- connect testcon
-- >    insert con (Test 1 Nothing (Has "Hello, world!"))
-- >    insert con (Test 2 (Just 10.0) (Has "Some string"))
-- >    insert con (Test 3 Nothing HasNo)
-- >    -- Test {testInt = 1, testOptional = Nothing, testString = Has "Hello, world!"}
-- >    -- Test {testInt = 2, testOptional = Just 10.0, testString = Has "Some string"}
-- >    -- Test {testInt = 3, testOptional = Nothing, testString = HasNo}
-- >    return ()
-- >
-- >runUpdate :: IO ()
-- >runUpdate = do
-- >    con <- connect testcon
-- >    -- @Nothing@ is for null, @HasNo@ is for no update
-- >    update_ con (Test 1 (Just 20.0) HasNo) " where testint = 1"
-- >    update_ con (Test 2 Nothing (Has "New string")) " where testint = 2"
-- >    update_ con (Test 3 (Just 30.0) (HasNo)) " where testint = 3"
-- >    -- Test {testInt = 1, testOptional = Just 20.0, testString = Has "Hello, world!"}
-- >    -- Test {testInt = 2, testOptional = Nothing, testString = Has "New string"}
-- >    -- Test {testInt = 3, testOptional = Just 30.0, testString = HasNo}
-- >    return ()
-- >
-- >runSelect :: IO ()
-- >runSelect = do
-- >    con <- connect testcon
-- >    vs <- select_ con "" :: IO [Test]
-- >    mapM_ print vs
--
module Data.Serialization.Postgresql (
    -- * Queries
    create,
    insert,
    update, update_,
    select, select_,
    selectFields, selectFields_,
    -- * Encode/decode row
    encodeRow, decodeRow,
    fieldsMap,
    -- * Serialize
    ToFields(..),
    encodeField, encodeOptField,
    -- * Deserialize
    FromFields(..),
    decodeField, decodeOptField,
    -- * Encode/decode
    Pgser(..), fields,
    -- * Meta info
    module Data.Serialization.Postgresql.Types
    ) where

import Control.Arrow
import Control.Applicative
import Control.Monad.Error
import Control.Monad.State
import Control.Monad.Writer

import Data.Char
import Data.List (intercalate)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Either (lefts, rights)
import Data.String
import Data.Int
import Data.Function (fix)
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as C8
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.Ok

import GHC.Generics

import Data.Serialization.Combinators (try)
import Data.Serialization.Wrap
import Data.Serialization.Generic
import Data.Serialization.Codec
import Data.Serialization.Dictionary

import Data.Serialization.Postgresql.Types

-- | Create table
--
-- >create con (Table "test" :: Table Test)
--
create :: (InTable a, Serializable Pgser a) => Connection -> Table a -> IO Int64
create con t = execute_ con $ fromString $ "create table " ++ table t ++ " (" ++ fs ++ ")" ++ inherits where
    fs = intercalate ", " $ map cat $ mapMaybe toFld flds
    inherits = if null inhs then "" else " inherits (" ++ intercalate ", " inhs ++ ")"
    inhs = rights $ map snd flds

    toFld (n, Left t) = Just (n, t)
    toFld _ = Nothing
    flds = getFields (fieldsFor t)
    cat (nm, tp) = nm ++ " " ++ tp

-- | Insert value into table
insert :: (InTable a, Serializable Pgser a) => Connection -> a -> IO Int64
insert con v = do
    x <- either (error . ("Data.Serialization.Postgresql.insert: unable to encode value: " ++)) (return . M.toList) $ encodeRow v
    execute con (fromString $ "insert into " ++ table (t v) ++ " (" ++ cols (map fst x) ++ ") values (" ++ qs x ++ ")") (map snd x) where
        t :: a -> Table a
        t _ = Table
        qs x' = intercalate ", " $ replicate (length x') "?"
        cols = intercalate ", "

-- | Update value
update :: (InTable a, Serializable Pgser a, ToRow q) => Connection -> a -> String -> q -> IO Int64
update con v condition args = do
    x <- either (error . ("Data.Serialization.Postgresql.update: unable to encode value: " ++)) (return . M.toList) $ encodeRow v
    execute con (fromString $ "update " ++ table (t v) ++ " set " ++ updates (map fst x) ++ condition) (map snd x ++ toRow args)
    where
        t :: a -> Table a
        t _ = Table
        updates = intercalate ", " . map (++ " = ?")

-- | Update value
update_ :: (InTable a, Serializable Pgser a) => Connection -> a -> String -> IO Int64
update_ con v condition = update con v condition ()

-- | Select value from table
-- select $ \t -> "select * from "
select :: (InTable a, Serializable Pgser a, ToRow q) => Connection -> String -> q -> IO [a]
select con condition args = selectFields con [] condition args

-- | Select value from table
select_ :: (InTable a, Serializable Pgser a) => Connection -> String -> IO [a]
select_ con condition = select con condition ()

-- | Select fields from table
selectFields :: (InTable a, Serializable Pgser a, ToRow q) => Connection -> [String] -> String -> q -> IO [a]
selectFields con fs condition args = fix $ \r -> do
    ff <- query con (fromString $ "select " ++ fs' ++ " from " ++ table (t r) ++ condition) args
    either (error . ("Data.Serialization.Postgresql.select: unable to decode value: " ++)) return $ mapM (decodeRow . fieldsMap) ff
    where
        t :: IO [a] -> Table a
        t _ = Table
        fs' = if null fs then "*" else intercalate ", " fs

-- | Select fields from table
selectFields_ :: (InTable a, Serializable Pgser a) => Connection -> [String] -> String -> IO [a]
selectFields_ con fs condition = selectFields con fs condition ()

fieldsFor :: (Serializable Pgser a) => Table a -> Fields a
fieldsFor _ = fields

-- | Encode row
encodeRow :: (Serializable Pgser a) => a -> Either String (M.Map String Action)
encodeRow x = encode (ser' x) x where
    ser' :: (Serializable Pgser a) => a -> Pgser a
    ser' _ = ser

-- | Decode row
decodeRow :: (Serializable Pgser a) => M.Map String AnyField -> Either String a
decodeRow f = fix $ \r -> decode (ser' r) f where
    ser' :: (Serializable Pgser a) => Either String a -> Pgser a
    ser' _ = ser

-- | Make @Map@ from field name to field
fieldsMap :: [AnyField] -> M.Map String AnyField
fieldsMap = M.fromList . map (maybe "" C8.unpack . name . anyFieldMeta &&& id)

instance Eq Action where
    l == r = show l == show r

-- | Serialize to @Action@
newtype ToFields a = ToFields { toFields :: ToDictionary String Action a }
    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Serializer (M.Map String Action), Generic)

instance GenericEncode ToFields where
    encodeStor name m = ToFields . encodeStor (map toLower name) (toFields . m)

-- | Encode one field
encodeField :: ToField a => Encoding ToFields a
encodeField = Encoding $ ToFields . runEncoding (toEntry "" (Right . toField))

-- | Encode @OptField@
encodeOptField :: ToField a => Encoding ToFields (OptField a)
encodeOptField = try encodeField .:. Iso (opt Nothing Just) (maybe HasNo Has)

instance ToField a => Serializable (Encoding ToFields) a where
    ser = encodeField

instance ToField a => Serializable (Encoding ToFields) (OptField a) where
    ser = encodeOptField

instance (Selector c, Serializable (Encoding ToFields) a) => GenericSerializable (Encoding ToFields) (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

-- | Deserialize from list of @AnyField@
newtype FromFields a = FromFields { fromFields :: FromDictionary String AnyField a }
    deriving (Functor, Applicative, Alternative, Monad, MonadError String, Deserializer (M.Map String AnyField), Generic)

instance GenericDecode FromFields where
    decodeStor name m = FromFields $ decodeStor (map toLower name) (fromFields m)

-- | Decode one field
decodeField :: FromField a => Decoding FromFields a
decodeField = Decoding $ FromFields $ runDecoding $ fromEntry "" fromAny

-- | Decode @OptField@, field that may not present in query result
decodeOptField :: FromField a => Decoding FromFields (OptField a)
decodeOptField = try decodeField .:. Iso (opt Nothing Just) (maybe HasNo Has)

instance FromField a => Serializable (Decoding FromFields) a where
    ser = decodeField

instance FromField a => Serializable (Decoding FromFields) (OptField a) where
    ser = decodeOptField

instance (Selector c, Serializable (Decoding FromFields) a) => GenericSerializable (Decoding FromFields) (Stor c (Parent a)) where
    gser = ser .:. Iso (parent . unStor) (Stor . Parent)

data Pgser a = Pgser {
    pgEncoder :: Encoding ToFields a,
    pgDecoder :: Decoding FromFields a,
    pgFields :: Fields a }

-- | Get type fields
fields :: Serializable Pgser a => Fields a
fields = pgFields ser

instance (Combine (Encoding ToFields), Combine (Decoding FromFields)) => Combine Pgser where
    ~(Pgser el dl fl) .*. ~(Pgser er dr fr) = Pgser (el .*. er) (dl .*. dr) (fl .*. fr)
    ~(Pgser el dl fl) .+. ~(Pgser er dr fr) = Pgser (el .+. er) (dl .+. dr) (fl .+. fr)
    ~(Pgser e d f) .:. iso = Pgser (e .:. iso) (d .:. iso) (f .:. iso)

instance (CombineM (Encoding ToFields), CombineM (Decoding FromFields)) => CombineM Pgser where
    ~(Pgser sl dl fl) .|. ~(Pgser sr dr fr) = Pgser (sl .|. sr) (dl .|. dr) (fl .|. fr)
    ~(Pgser s d fs) .>> f = Pgser (s .>> (pgEncoder . f)) (d .>> (pgDecoder . f)) (fs .>> (pgFields . f))
    pures v = Pgser (pures v) (pures v) (pures v)
    fails s = Pgser (fails s) (fails s) (fails s)
    p .?. ~(Pgser s d f) = Pgser (p .?. s) (p .?. d) (p .?. f)
    ~(Pgser s d f) .%. e = Pgser (s .%. e) (d .%. e) (f .%. e)

instance (GenericCombine (Encoding ToFields), GenericCombine (Decoding FromFields)) => GenericCombine Pgser where
    genericData s ~(Pgser e d f) = Pgser (genericData s e) (genericData s d) (genericData s f)
    genericCtor s ~(Pgser e d f) = Pgser (genericCtor s e) (genericCtor s d) (genericCtor s f)
    genericStor s ~(Pgser e d f) = Pgser (genericStor s e) (genericStor s d) (genericStor s f)

instance (ToField a, FromField a, ColumnType a) => Serializable Pgser a where
    ser = Pgser encodeField decodeField ser

instance (ToField a, FromField a, ColumnType a) => Serializable Pgser (OptField a) where
    ser = Pgser encodeOptField decodeOptField ser

instance Encoder (M.Map String Action) Pgser where
    encode (Pgser e _ _) = encode e

instance Decoder (M.Map String AnyField) Pgser where
    decode (Pgser _ d _) = decode d
