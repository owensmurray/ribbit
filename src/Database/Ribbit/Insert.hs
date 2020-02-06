{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Ribbit.Insert (
  InsertInto,
) where


import Database.Ribbit.Render (Render)
import Database.Ribbit.Table (Name)
import GHC.TypeLits (AppendSymbol)


{- | Insert statement. -}
data InsertInto table fields


type instance Render (InsertInto table fields) =
  "insert into "
  `AppendSymbol` Name table
  `AppendSymbol` " ("
  `AppendSymbol` RendFieldList fields
  `AppendSymbol`") values ("
  `AppendSymbol` RendPlaceholderList fields
  `AppendSymbol` ");"


type family RendFieldList a where
  RendFieldList '[field] = field
  RendFieldList (f1:f2:more) =
    f1
    `AppendSymbol` ", "
    `AppendSymbol` RendFieldList (f2:more)

type family RendPlaceholderList a where
  RendPlaceholderList '[field] = "?"
  RendPlaceholderList (f1:f2:more) =
    "?"
    `AppendSymbol` ", "
    `AppendSymbol` RendPlaceholderList (f2:more)


