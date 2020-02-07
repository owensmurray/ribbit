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
import Database.Ribbit.Params (ParamsType, ParamsTypeSchema,
  ProjectionType)
import Database.Ribbit.Render (Render)
import Database.Ribbit.Table (Name, DBSchema, (:>))
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


type instance ParamsType (Update relation fields) =
  ProjectionType fields (DBSchema relation)


type instance ParamsType (Update relation fields `Where` conditions) =
  ProjectionType fields (DBSchema relation)
  :> ParamsTypeSchema (DBSchema relation) conditions


