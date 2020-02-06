{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

{- | "postgresql-simple"-backed query ribbit implementation. -}
module Database.Ribbit.PostgreSQL (
  -- * Integrating your types.
  HasPsqlType(..),
  PsqlType(..),

  -- * Performing queries.
  execute,
  query,

  -- * Creating tables.
  createTable,
  createTableStatement,

  -- * Other types.
  -- | These type classes/families are not meant to be used directly. They
  -- are exported primarily because they appear in the type signatures
  -- of some of the above functions and documenting them can be helpful
  -- when trying to figure out how to use those functions.
  HasFields,
  HasPsqlTypes,
  HasIsNullable,
  IsSubset,
  FromRow,
  ToRow,
) where


import Control.Monad (void)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Int (Int64)
import Data.Proxy (Proxy (Proxy))
import Data.String (fromString, IsString)
import Data.Text (Text)
import Data.Time (Day)
import Data.Tuple.Only (Only (Only))
import Data.Type.Bool (If)
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (Action, ToField)
import Database.Ribbit.Args (ArgsType, ResultType)
import Database.Ribbit.Render (Render)
import Database.Ribbit.Table ((:>)((:>)), Name, DBSchema, Field,
  ValidField)
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage((:<>:),
  ShowType))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromRow as PGF
import qualified Database.PostgreSQL.Simple.ToRow as PGT
import qualified GHC.TypeLits as Lit


{- | Execute a statement. -}
execute ::
     forall m query.
     (MonadIO m, ToRow (ArgsType query), KnownSymbol (Render query))
  => Connection
  -> Proxy query
  -> ArgsType query
  -> m Int64
execute conn _theQuery args =
  liftIO $
    PG.execute
      conn
      (fromString (symbolVal (Proxy @(Render query))))
      (Wrap args)



{- | Like 'PGT.ToRow', but defined here to avoid orphan instances. -}
class ToRow a where
  toRow :: a -> [Action]
instance (ToRow a, ToRow b) => ToRow (a :> b) where
  toRow (a :> b) = toRow a ++ toRow b
instance (ToField a) => ToRow (Only a) where
  toRow = PGT.toRow
instance ToRow () where
  toRow = PGT.toRow
  

{- | Like 'PGF.FromRow', but defined here so we can avoid orphaned instances. -}
class FromRow a where
  fromRow :: PGF.RowParser a
instance (FromRow a, FromRow b) => FromRow (a :> b) where
  fromRow = 
    (:>)
      <$> fromRow
      <*> fromRow
instance (FromField a) => FromRow (Only a) where
  fromRow = Only <$> PGF.field


{- | Wrapper that helps us avoid orphan instances. -}
newtype Wrap a = Wrap {
    unWrap :: a
  }
instance (FromRow a) => PGF.FromRow (Wrap a) where
  fromRow = Wrap <$> fromRow
instance (ToRow a) => PGT.ToRow (Wrap a) where
  toRow = toRow . unWrap


{- |
  Create the indicated table in the database.

  See 'createTableStatement' for details.
-}
createTable :: forall proxy1 proxy2 key table m. (
    KnownSymbol (Name table),
    HasPsqlTypes (DBSchema table),
    HasFields (DBSchema table),
    HasFields key,
    IsSubset key (DBSchema table) ~ 'True,
    MonadIO m
  )
  => Connection
  -> proxy1 key
  -> proxy2 table
  -> m ()
createTable conn key table =
  let
    stmt :: Text
    stmt = createTableStatement key table
  in
    liftIO . void $
      PG.execute_
        conn
        (fromString . T.unpack $ stmt)


{- |
  Produce the statement used to create a table.

  In this example, we create an employee table with a multi-part primary
  key, one nullable field, and a few non-nullable fields.

  > data Employee
  > instance Table Employee where
  >   type Name = "employees"
  >   type DBSchema =
  >     Field "company_id" Int
  >     :> Field "id" Int
  >     :> Field "name" Text
  >     :> Field "quit_date" (Maybe Day)
  >
  > let
  >   primaryKey :: Proxy '["company_id", "id"]
  >   primaryKey = Proxy
  >   
  >   table :: Proxy Employee
  >   table = Proxy
  >
  > in
  >   createTableStatement primaryKey table

  This will produce the statement:

  > "create table employees (company_id integer not null, id integer not null, name text not null, quit_date date, primary key (company_id, id));"
-}
createTableStatement :: forall proxy1 proxy2 table key. (
    KnownSymbol (Name table),
    HasPsqlTypes (DBSchema table),
    HasFields (DBSchema table),
    HasFields key,
    IsSubset key (DBSchema table) ~ 'True
  )
  => proxy1 key
  -> proxy2 table
  -> Text
createTableStatement key _table =
    "create table " <> symbolVal tableName
    <> " (" <> T.intercalate ", " [
      field
      <> " "
      <> typ
      | (field, typ) <- zip (fields schema) (psqlTypes schema)
    ]
    <> (
      case fields key of
        [] -> ""
        fs -> ", primary key (" <> T.intercalate ", " fs <> ")"
      )
    <> ");"
  where
    schema :: Proxy (DBSchema table)
    schema = Proxy

    tableName :: Proxy (Name table)
    tableName = Proxy


class HasPsqlTypes a where
  psqlTypes :: proxy a -> [Text]
instance (HasIsNullable typ, HasPsqlType typ) => HasPsqlTypes (Field name typ) where
  psqlTypes _proxy =
    [
      unPsqlType (psqlType (Proxy @typ))
      <> if isNullable (Proxy @typ) then "" else " not null"
    ]
instance (HasIsNullable typ, HasPsqlType typ, HasPsqlTypes more) =>
    HasPsqlTypes (Field name typ :> more)
  where
    psqlTypes _proxy =
      psqlTypes (Proxy @(Field name typ)) ++ psqlTypes (Proxy @more)


{- |
  Given a Haskell type, produce the PostgreSQL type of columns that
  store values of that haskell type.
-}
class HasPsqlType a where
  psqlType :: proxy a -> PsqlType
instance (HasPsqlType a) => HasPsqlType (Maybe a) where
  psqlType _proxy = psqlType (Proxy @a)
instance HasPsqlType Text where
  psqlType _proxy = "text"
instance HasPsqlType Int where
  psqlType _proxy = "integer"
instance HasPsqlType Day where
  psqlType _proxy = "date"


{- | Make sure the fields in the list are actually part of the schema. -}
type family IsSubset fields schema where
  IsSubset '[] schema = 'True
  IsSubset (field:more) schema =
    If
      (ValidField field schema)
      (IsSubset more schema)
      (
        TypeError (
          'Lit.Text "field "
          ':<>: 'ShowType field
          ':<>: 'Lit.Text " is not part of the schema, so it cannot be\
                          \ used as a component of the primary key."
        )
      )


{- | Produce a list of field names from a schema. -}
class HasFields a where
  fields :: proxy a -> [Text]
instance (KnownSymbol name) => HasFields (Field name typ) where
  fields _proxy = [symbolVal (Proxy @name)]
instance (KnownSymbol name, HasFields more) =>
    HasFields (Field name typ :> more)
  where
    fields _proxy = symbolVal (Proxy @name) : fields (Proxy @more)
instance HasFields '[] where
  fields _proxy = []
instance (KnownSymbol name, HasFields more) => HasFields (name:more) where
  fields _proxy = symbolVal (Proxy @name) : fields (Proxy @more)


{- | Figure out if a Haskell type is "nullable" in sql. -}
class HasIsNullable a where
  isNullable :: proxy a -> Bool
instance HasIsNullable (Maybe a) where
  isNullable _proxy = True
instance {-# OVERLAPPABLE #-} HasIsNullable a where
  isNullable _proxy = False


{- |
  Represents the "base" PostgreSQL type. We say "base" type because
  whether the type is nullable is handle automatically.

  e.g.

  * @PsqlType "integer"@
  * @PsqlType "timestamp with time zone"@
-}
newtype PsqlType = PsqlType {
    unPsqlType :: Text
  }
  deriving newtype (IsString)


{- | Like 'Lit.symbolVal', but produce any kind of string-like thing. -}
symbolVal :: (KnownSymbol n, IsString a) => proxy n -> a
symbolVal = fromString . Lit.symbolVal


{- | Execute a query against a PostgreSQL database connection. -}
query ::
     forall m query.
     ( MonadIO m
     , KnownSymbol (Render query)
     , ToRow (ArgsType query)
     , FromRow (ResultType query)
     )
  => Connection
  -> Proxy query
  -> ArgsType query
  -> m [ResultType query]
query conn _theQuery args =
  liftIO . (fmap . fmap) unWrap $
    PG.query
      conn 
      (fromString (symbolVal (Proxy @(Render query))))
      (Wrap args)
