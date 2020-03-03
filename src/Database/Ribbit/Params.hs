{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Translate statement parameters into Haskell types. -}
module Database.Ribbit.Params (
  ParamsType,
  ResultType,
  ParamsTypeSchema,
  ProjectionType,
) where


import Data.Tuple.Only (Only)
import Database.Ribbit.Table ((:>), Field, NotInSchema)


{- |
  Produce the type represeting the placeholder ("?") values in a
  paramaterized query.

  This type family is open and extendable.
-}
type family ParamsType query


{- | Produce the type of rows return by a query. -}
type family ResultType query


{- |
  Produce the parameters type in relation to a particiular schema.

  This type family is open and extendable.
-}
type family ParamsTypeSchema schema a


type family ProjectionType proj schema where
  ProjectionType '[name] schema =
    LookupType name schema schema
  ProjectionType (name:more) schema =
    LookupType name schema schema
    :> ProjectionType more schema


type family LookupType name schema context where
  LookupType name (Field name typ) _ = Only typ
  LookupType name (Field name typ :> _) _ = Only typ
  LookupType name (Field _ typ) context = NotInSchema name context
  LookupType name (_ :> more) context = LookupType name more context
  LookupType name a context = NotInSchema name context


