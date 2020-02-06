{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


{- | Update statements. -}
module Database.Ribbit.Update (
  Update,
) where


import Database.Ribbit.Conditions (RenderConditions, Where)
import Database.Ribbit.Render (Render)
import Database.Ribbit.Table (Name, DBSchema)
import GHC.TypeLits (AppendSymbol, Symbol)


{- | Update statement. -}
data Update table (fields :: [Symbol])


type instance Render (Update table fields) =
  "UPDATE "
  `AppendSymbol` Name table
  `AppendSymbol` " SET "
  `AppendSymbol` RenderUpdateFields fields

type instance Render (Update table fields `Where` conditions) =
  Render (Update table fields)
  `AppendSymbol` " WHERE "
  `AppendSymbol` RenderConditions conditions (DBSchema table)


type family RenderUpdateFields fields where
  RenderUpdateFields '[field] =
    field `AppendSymbol` " = ?"
  RenderUpdateFields (field:more) =
    field
    `AppendSymbol` " = ?, "
    `AppendSymbol` RenderUpdateFields more

-- {- UPDATE -}
-- instance (KnownSymbol (Name table), RenderUpdates updates)
--   =>
--     Render (Update table updates)
--   where
--     render _proxy =
--       "UPDATE "
--       <> symbolVal (Proxy @(Name table))
--       <> " SET "
--       <> renderUpdates (Proxy @updates)
-- 
-- 
-- {- | Render the updates to a table. -}
-- class RenderUpdates a where
--   renderUpdates :: proxy a -> Text
-- instance (KnownSymbol field) => RenderUpdates '[field] where
--   renderUpdates _ = symbolVal (Proxy @field) <> " = ?"
-- instance (KnownSymbol field, RenderUpdates (m:ore))
--   =>
--     RenderUpdates (field : (m:ore))
--   where
--     renderUpdates _ =
--       symbolVal (Proxy @field) <> " = ?, "
--       <> renderUpdates (Proxy @(m:ore))
