{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


{- | Delete statements. -}
module Database.Ribbit.Delete (
  DeleteFrom,
) where


import Database.Ribbit.Conditions (RenderConditions, Where)
import Database.Ribbit.Render (Render)
import Database.Ribbit.Table (Name, DBSchema)
import GHC.TypeLits (AppendSymbol)


{- | Delete statement. -}
data DeleteFrom table


type instance Render (DeleteFrom table) =
  "DELETE FROM "
  `AppendSymbol` Name table

type instance Render (DeleteFrom table `Where` conditions) =
  Render (DeleteFrom table)
  `AppendSymbol` " WHERE "
  `AppendSymbol` RenderConditions conditions (DBSchema table)


