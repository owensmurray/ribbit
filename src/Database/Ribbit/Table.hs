{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Ribbit.Table (
  Table(..),
  Field,
  (:>)(..),

  Flatten,
  ValidField,
  Validate,
  NotInSchema,
) where


import Data.Type.Bool (type (||), If)
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(ShowType, (:<>:)))
import qualified GHC.TypeLits as Lit


{- |
  Type class for defining your own tables. The primary way for you to
  introduce a new schema is to instantiate this type class for one of
  your types.

  E.g.:

  > data MyTable
  > instance Table MyTable where
  >   type Name MyTable = "my_table"
  >   type DBSchema MyTable =
  >     Field "id" Int
  >     :> Field "my_non_nullable_text_field" Text
  >     :> Field "my_nullable_int_field" (Maybe Int)
    
-}
class Table relation where
  type Name relation :: Symbol
  type DBSchema relation


{- |
  Define a field in a database schema, where:

  - @name@: is the name of the database column, expressed as a type-level
    string literal, and

  - @typ@: is the Haskell type whose values get stored in the column.

  E.g:

  - @'Field' "company_name" 'Text'@
  - @'Field' "expiration_date" ('Maybe' 'Data.Time.Day')@

-}
data Field name typ


{- |
  String two types together. 'Int' ':>' 'Int' ':>' 'Int' is similar in
  principal to the nested tuple ('Int', ('Int', 'Int')), but looks a
  whole lot nicer when the number of elements becomes large.

  This is how you build up a schema from a collection of 'Field' types.

  E.g.:

  > Field "foo" Int
  > :> Field "bar" Text
  > :> Field "baz" (Maybe Text)

  It also the mechanism by which this library builds up the Haskell
  types for query parameters and resulting rows that get returned. So
  if you have a query that accepts three text query parameters, that
  type represented in Haskell is going to be @('Only' 'Text' ':>' 'Only'
  'Text' ':>' 'Only' 'Text')@.

  If that query returns rows that contain a Text, an Int, and a Text,
  then the type of the rows will be @('Only' 'Text' ':>' 'Only' 'Int'
  ':>' 'Only' 'Text')@.

-}
data a :> b = a :> b
  deriving (Eq, Ord, Show)
infixr 5 :>

{- |
  Normalize nested type strings to be right associative. Mainly used to
  help simplify the implementation of other type families.
-}
type family Flatten a where
  Flatten ((a :> b) :> c) = Flatten (a :> b :> c)
  Flatten (a :> b) = a :> Flatten b
  Flatten a = a


{- | Type level check to see if the field is actually contained in the schema -}
type family ValidField field schema where
  ValidField name (Field name typ) = 'True
  ValidField name (Field _ typ) = 'False
  ValidField name (a :> b) = ValidField name a || ValidField name b


type Validate field schema result =
  If (ValidField field schema)
    result
    (NotInSchema field schema)


type family NotInSchema field schema where
  NotInSchema field schema =
    TypeError (
      'Lit.Text "name ("
      ':<>: 'ShowType field
      ':<>: 'Lit.Text ") not found in schema: "
      ':<>: 'ShowType schema
    )

