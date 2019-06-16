{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

{- | "postgresql-simple"-backed query ribbit implementation. -}
module Database.Ribbit.PostgreSQL (
  query,
) where


import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Proxy (Proxy)
import Data.String (fromString)
import Data.Tuple.Only (Only(Only))
import Database.PostgreSQL.Simple (Connection)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField (Action, ToField)
import Database.Ribbit (Render, render, ArgsType, ResultType, (:>)((:>)))
import qualified Data.Text as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.FromRow as PGF
import qualified Database.PostgreSQL.Simple.ToRow as PGT


query :: (
    MonadIO m,
    Render query,
    ToRow (ArgsType query),
    FromRow (ResultType query)
  )
  => Connection
  -> Proxy query
  -> ArgsType query
  -> m [ResultType query]
query conn theQuery args =
  liftIO . (fmap . fmap) unWrap $
    PG.query
      conn 
      ((fromString . T.unpack . render) theQuery)
      (Wrap args)


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


{- | Like 'PGT.ToRow', but defined here to avoid orphan instances. -}
class ToRow a where
  toRow :: a -> [Action]
instance (ToRow a, ToRow b) => ToRow (a :> b) where
  toRow (a :> b) = toRow a ++ toRow b
instance (ToField a) => ToRow (Only a) where
  toRow = PGT.toRow


{- | Wrapper that helps us avoid orphan instances. -}
newtype Wrap a = Wrap {
    unWrap :: a
  }
instance (FromRow a) => PGF.FromRow (Wrap a) where
  fromRow = Wrap <$> fromRow
instance (ToRow a) => PGT.ToRow (Wrap a) where
  toRow = toRow . unWrap


