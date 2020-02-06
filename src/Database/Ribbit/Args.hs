{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Translate statement parameters into Haskell types. -}
module Database.Ribbit.Args (
  ArgsType,
  ResultType,
) where


import Data.Tuple.Only (Only)
import Data.Type.Bool (If)
import Database.Ribbit.Conditions (Where, Equals, NotEquals, Lt, Lte,
  Gt, Gte, Not, And, Or, type (?))
import Database.Ribbit.Delete (DeleteFrom)
import Database.Ribbit.Insert (InsertInto)
import Database.Ribbit.Select (From, Select)
import Database.Ribbit.Table (DBSchema, Flatten, (:>), Field, ValidField,
  NotInSchema)
import Database.Ribbit.Update (Update)
import GHC.TypeLits (TypeError, ErrorMessage(ShowType, (:$$:)))
import qualified GHC.TypeLits as Lit


{- |
  Produce the type represeting the placeholder ("?") values in a
  paramaterized query.
-}
type family ArgsType query where
  ArgsType (_ `From` relation `Where` conditions) =
    ArgsType (DBSchema relation, conditions)

  ArgsType (InsertInto relation fields) =
    ProjectionType fields (DBSchema relation)

  ArgsType (DeleteFrom relation `Where` conditions) =
    ArgsType (DBSchema relation, conditions)

  ArgsType (Update relation fields) =
    ProjectionType fields (DBSchema relation)
  ArgsType (Update relation fields `Where` conditions) =
    ProjectionType fields (DBSchema relation)
    :> ArgsType (DBSchema relation, conditions)

  ArgsType (schema, And a b) =
    StripUnit (Flatten (ArgsType (schema, a) :> ArgsType (schema, b)))
  ArgsType (schema, Or a b) =
    StripUnit (Flatten (ArgsType (schema, a) :> ArgsType (schema, b)))
  ArgsType (schema, Condition field (?)) =
    ProjectionType '[field] schema
  ArgsType (schema, Condition (?) field) =
    ProjectionType '[field] schema
  ArgsType (schema, Condition l r) =
    If
      (ValidField r schema)
      (If (ValidField l schema) () (NotInSchema l schema))
      (NotInSchema r schema)
  ArgsType (schema, Equals l r) = ArgsType (schema, Condition l r)
  ArgsType (schema, NotEquals l r) = ArgsType (schema, Condition l r)
  ArgsType (schema, Lt l r) = ArgsType (schema, Condition l r)
  ArgsType (schema, Lte l r) = ArgsType (schema, Condition l r)
  ArgsType (schema, Gt l r) = ArgsType (schema, Condition l r)
  ArgsType (schema, Gte l r) = ArgsType (schema, Condition l r)
  ArgsType (schema, Not a) = ArgsType (schema, a)
  ArgsType _ = ()


{- |
  Helper for 'ArgsType'. Reduces the number of equations required, because
  'ArgsType' doesn't actually care about which conditionl operator it
  is inspecting.
-}
data Condition l r


{- |
  Strip redundant unit types out of a string of types. This is used
  mainly to help simplify the implementation of 'ArgsType'.
-}
type family StripUnit a where
  StripUnit (() :> a) = StripUnit a
  StripUnit (a :> ()) = StripUnit a
  StripUnit (a :> b) = a :> StripUnit b
  StripUnit a = a


type family ProjectionType proj schema where
  ProjectionType '[name] schema =
    LookupType name schema schema
  ProjectionType (name:more) schema =
    LookupType name schema schema
    :> ProjectionType more schema


{- | Produce the type of rows return by a query. -}
type family ResultType query where
  ResultType (Select fields `From` relation) =
    ProjectionType fields (DBSchema relation)
  ResultType (query `Where` conditions) = ResultType query
  ResultType query =
    TypeError ('Lit.Text "Malformed Query" ':$$: 'ShowType query)


type family LookupType name schema context where
  LookupType name (Field name typ) _ = Only typ
  LookupType name (Field name typ :> _) _ = Only typ
  LookupType name (Field _ typ) context = NotInSchema name context
  LookupType name (_ :> more) context = LookupType name more context
  LookupType name a context = NotInSchema name context


