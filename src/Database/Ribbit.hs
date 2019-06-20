{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


{- |
  This module attepts to define a type-level language for describing
  database shcemas (i.e. schemas "as a type"), and the queries
  that operate on them in such a way that the meaning of a query is
  immediately obvious to anyone who knows SQL, and that can be extended
  and deconstructed by library users for their own purposes.
-}
module Database.Ribbit (
  -- * Quick Start
  -- ** Defining a Table
  -- $definetable

  -- ** Building a Query
  -- $query

  -- ** Using a Query
  -- $usequery

  -- * Schema Definition Types
  (:>)(..),
  Table(..),
  Field,

  -- * Query Combinators
  Select,
  From,
  X,
  As,
  Where,

  -- ** Condition Conbinators
  And,
  Or,
  Equals,

  -- ** Query Parameters
  type (?),

  -- * Transformations on Query Types
  ArgsType,
  ResultType,
  
  -- * Query Rendering
  Render(..)

) where


import Data.Proxy (Proxy(Proxy))
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Tuple.Only (Only)
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage((:<>:),
  (:$$:), ShowType), AppendSymbol, Symbol)
import qualified GHC.TypeLits as Lit


-- $definetable
-- To define a table you need a type:
-- 
-- > data Company
--
-- (Note: It is not required that the type contain any data, but it can if
-- you like. Unlike some db frameworks, columns stored in the table
-- represented by this type is not directly tied to the Haskell record
-- fields it contains. It is mainly used as a type-level symbol to
-- reference your table.)
-- 
-- And you need a type class instance 'Table':
-- 
-- > instance Table Company where
-- >
-- >   type Name Company = "companies"
-- >
-- >   type DBSchema Company =
-- >     Field "id" Int
-- >     :> Field "name" Text
-- >     :> Field "address" (Maybe Text)
--   
-- The different parts of this typeclass instance include:
--
-- * A Name:
--
-- >   type Name Company = "companies"
--
-- * And a schema definition:
--
-- >   type DBSchema Company =
-- >     Field "id" Int
-- >     :> Field "name" Text
-- >     :> Field "address" (Maybe Text)
-- 
-- Let's go ahead and define another table. We will use these two tables
-- in the following examples:
--
-- > data Employee
-- >
-- > instance Table Employee where
-- >
-- >   type Name Employee = "employees"
-- >
-- >   type DBSchema Employee =
-- >     Field "id" Int
-- >     :> Field "company_id" Int
-- >     :> Field "name" Text
-- >     :> Field "salary" (Maybe Int)
-- >     :> Field "birth_date" Day

-- $query
-- To write queries against these tables, use the query combinators
-- defined in this module:
-- 
-- > -- Given a company name as a query parameter, return all the
-- > -- employees that work at that company along with their salary.
-- > type MyQuery =
-- >   Select '["e.name", "e.salary"]
-- >   `From`
-- >     Company `As` "c" `X` Employee `As` "e"
-- >   `Where`
-- >     "c.id" `Equals` "e.company_id"
-- >     `And` "c.name" `Equals` (?)

-- $usequery
-- Now that we have some tables and a query, how do we make use of
-- them? Well, the first thing to notice is that a query like this needs
-- inputs (the query parameter), and provides outputs (the selected
-- rows). These inputs and outputs need to be typed, and indeed they are
-- thanks to a couple of special type families:
--
-- * 'ArgsType' - Given a query, produce the type of the embedded query
--   parameters.
-- * 'ResultType' - Given a query, produce the type of rows produced by
--   that query.
-- 
-- +----------------------+-------------------------------------------+
-- | Example              | Resulting type                            |
-- +======================+===========================================+
-- | 'ArgsType' MyQuery   | 'Only' 'Text'                             |
-- +----------------------+-------------------------------------------+
-- | 'ResultType' MyQuery | 'Only' 'Text' ':>' 'Only' ('Maybe' 'Int') |
-- +----------------------+-------------------------------------------+
--




{- | "SELECT" combinator, used for starting a @SELECT@ statement. -}
data Select fields



{- |
  "FROM" combinator, used for attaching a SELECT projection to a relation
  in the database.
-}
data From proj relation
infixl 6 `From`


{- | "WHERE" combinator, used for attaching conditions to a query. -}
data Where query conditions
infixl 6 `Where`



{- | "=" combinator for conditions. -}
data Equals l r
infix 9 `Equals`


{- | "AND" combinator for conditions. -}
data And l r
infixr 8 `And`


{- | "OR" combinator for conditions. -}
data Or l r
infixr 7 `Or`


{- | Cross product operator for FROM clauses. -}
data X l r
infixr 7 `X`


{- | "AS" combinator, used for attaching a name to a table in a FROM clause. -}
data As relation name
infix 8 `As`


{- | "?" combinator, used to indicate the presence of a query parameter. -}
data (?)


{- | Define a field in a database schema. -}
data Field name typ


{- |
  String two types together. 'Int' ':>' 'Int' ':>' 'Int' is similar in
  principal to the nested tuple ('Int', ('Int', 'Int')), but looks a
  whole lot nicer when the number of elements becomes large.
-}
data a :> b = a :> b
  deriving (Eq, Ord, Show)
infixr 5 :>


data Expr a

type family ProjectionType proj schema where
  ProjectionType '[name] (Field name typ) = Only typ
  ProjectionType '[name] (Field name2 typ) =
    TypeError (
      'Lit.Text "name ("
      ':<>: 'ShowType name
      ':<>: 'Lit.Text ") not found in relation."
    )
  ProjectionType '[name] (Field name typ :> _) = Only typ
  ProjectionType '[name] (_ :> more) = ProjectionType '[name] more

  ProjectionType (name:more) relation =
    ProjectionType '[name] relation :> ProjectionType more relation

class Table relation where
  type DBSchema relation
  type Name relation :: Symbol

{- | Cross product -}
instance (Table l, Table r, KnownSymbol lname, KnownSymbol rname) => Table (l `As` lname `X` r `As` rname) where
  type DBSchema (l `As` lname `X` r `As` rname) =
    Flatten (
      AliasAs lname (DBSchema l)
      :> AliasAs rname (DBSchema r)
    )
  type Name (l `As` lname `X` r `As` rname) =
    Name l
    `AppendSymbol` " as "
    `AppendSymbol` lname
    `AppendSymbol` ", "
    `AppendSymbol` Name r
    `AppendSymbol` " as "
    `AppendSymbol` rname


{- |
  Rename the fields in a given schema to reflect an applied table
  alias. For instance, data Foo
-}
type family AliasAs prefix schema where
  AliasAs prefix (Field name typ) =
    Field
      (prefix `AppendSymbol` "." `AppendSymbol` name)
      typ
  AliasAs prefix (Field name typ :> more) =
    Field
      (prefix `AppendSymbol` "." `AppendSymbol` name)
      typ
    :> AliasAs prefix more


{- | Produce the type of rows return by a query. -}
type family ResultType query where
  ResultType (Select fields `From` relation) =
    ProjectionType fields (DBSchema relation)
  ResultType (query `Where` conditions) = ResultType query
  ResultType query =
    TypeError ('Lit.Text "Malformed Query" ':$$: 'ShowType query)


{- |
  Produce the type represeting the placeholder ("?") values in a
  paramaterized query.
-}
type family ArgsType query where
  ArgsType (_ `From` relation `Where` conditions) =
    ArgsType (DBSchema relation, conditions)
  ArgsType (schema, And a b) =
    StripUnit (Flatten (ArgsType (schema, a) :> ArgsType (schema, b)))
  ArgsType (schema, Or a b) =
    StripUnit (Flatten (ArgsType (schema, a) :> ArgsType (schema, b)))
  ArgsType (schema, Equals field (?)) =
    ProjectionType '[field] schema
  ArgsType _ = ()


{- |
  Normalize nested type strings to be right associative. Mainly used to
  help simplify the implementation of other type families.
-}
type family Flatten a where
  Flatten ((a :> b) :> c) = Flatten (a :> b :> c)
  Flatten (a :> b) = a :> Flatten b
  Flatten a = a


{- |
  Strip redundant unit types out of a string of types. This is used
  mainly to help simplify the implementation of 'ArgsType'.
-}
type family StripUnit a where
  StripUnit (() :> a) = StripUnit a
  StripUnit (a :> ()) = StripUnit a
  StripUnit (a :> b) = a :> StripUnit b
  StripUnit a = a


{- | Like 'Lit.symbolVal', but produce any kind of string-like thing. -}
symbolVal :: (KnownSymbol n, IsString a) => proxy n -> a
symbolVal = fromString . Lit.symbolVal


{- | Render a type-level query as text. -}
class Render query where
  render :: proxy query -> Text

{- SELECT -}
instance (Render fields) => Render (Select fields) where
  render _proxy =
    "SELECT "
    <> render (Proxy @fields)

{- Field list -}
instance {-# OVERLAPS #-} (KnownSymbol field) => Render '[field] where
  render _proxy = symbolVal (Proxy @field)
instance (KnownSymbol field, Render more) => Render (field:more) where
  render _proxy =
    symbolVal (Proxy @field) <>  ", " <> render (Proxy @more)

{- FROM -}
instance (KnownSymbol (Name relation), Render proj, Table relation) => Render (From proj relation) where
  render _proxy =
    render (Proxy @proj)
    <> " FROM "
    <> symbolVal (Proxy @(Name relation))

{- WHERE -}
instance (Render query, Render conditions) => Render (Where query conditions) where
  render _proxy =
    render (Proxy @query)
    <> " WHERE "
    <> render (Proxy @conditions)

{- Equals -}
instance (Render (Expr l), Render (Expr r)) => Render (Equals l r) where
  render _proxy =
    render (Proxy @(Expr l))
    <> " = "
    <> render (Proxy @(Expr r))

{- AND -}
instance (Render l, Render r) => Render (And l r) where
  render _proxy =
    "( "
    <> render (Proxy @l)
    <> " AND "
    <> render (Proxy @r)
    <> " )"

{- OR -}
instance (Render l, Render r) => Render (Or l r) where
  render _proxy =
    "( "
    <> render (Proxy @l)
    <> " AND "
    <> render (Proxy @r)
    <> " )"

{- Expr -}
instance Render (Expr (?)) where
  render _proxy = "?"
instance (KnownSymbol a) => Render (Expr a) where
  render _proxy = symbolVal (Proxy @a)

{- (?) -}
instance Render (?) where
  render _proxy = "?"


