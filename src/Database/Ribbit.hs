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
  database shcemas (i.e. schemas "as a type"), and the queries that
  operate on them in such a way that:
  
  1) The schema and/or query is completely defined at the type level
     (sans runtime arguments to query parameters).

  2) The meaning of a schema and/or query is immediately obvious to
     anyone who knows SQL, and

  3) The schema and/or query can be extended, deconstructed, or
     interpreted by third parties for their own purposes.

  To that end, each schema is a new type, defined by you, using the
  combinators provided by this library. The same goes for queries. Each
  query is a separate type defined with combinators from this library.

  We provide a PostgreSQL backend so that real work can be accomplished,
  but if the backend is missing something you need, then the idea is
  that you can use your own type families and type classes to extend
  the schema and query languages, or interpret them differently for your
  own needs including writing entirely new backends if need be.
-}
module Database.Ribbit (
  -- * Quick Start
  -- ** Defining a Table
  -- $definetable

  -- ** Building a Query
  -- $query

  -- ** Using a Query
  -- $usequery

  -- ** Inserting values
  -- $insert

  -- ** Deleting values
  -- $delete

  -- * Schema Definition Types
  (:>)(..),
  Table(..),
  Field,

  -- * Query Combinators
  Select,
  From,
  As,
  Where,

  -- * Insert Combinators
  InsertInto,
  
  -- * Delete Combinators
  DeleteFrom,

  -- ** Condition Conbinators
  And,
  Or,
  Equals,
  NotEquals,
  Gt,
  Gte,
  Lt,
  Lte,
  Not,

  -- ** Query Parameters
  type (?),

  -- * Transformations on Query Types
  ArgsType,
  ResultType,
  ValidField,
  ProjectionType,

  -- * Query Rendering
  Render(..)

) where


import Data.Proxy (Proxy(Proxy))
import Data.String (IsString, fromString)
import Data.Text (Text)
import Data.Tuple.Only (Only)
import Data.Type.Bool (If, type (||))
import GHC.TypeLits (KnownSymbol, TypeError, ErrorMessage((:<>:),
  (:$$:), ShowType), AppendSymbol, Symbol)
import qualified Data.Text as T
import qualified GHC.TypeLits as Lit


-- $definetable
-- To define a table you need a type:
-- 
-- > data Company
--
-- (Note: It is not required that the type contain any data, but it can
-- if you like. Unlike some db frameworks, the set of columns stored
-- in the table represented by this type is not directly tied to the
-- Haskell record fields it contains. It is mainly used as a type-level
-- symbol to reference your table.)
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
-- >
-- > type MyQuery =
-- >   Select '["e.name", "e.salary"]
-- >   `From`
-- >       '[
-- >         Company `As` "c",
-- >         Employee `As` "e"
-- >       ]
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
-- The "Database.Ribbit.PostgreSQL" module provides a
-- 'Database.Ribbit.PostgreSQL.query' function:
-- 
-- > query :: (
-- >     MonadIO m,
-- >     Render query,
-- >     ToRow (ArgsType query),
-- >     FromRow (ResultType query)
-- >   )
-- >   => Connection
-- >   -> Proxy query
-- >   -> ArgsType query
-- >   -> m [ResultType query]
-- 
-- Notice that it accepts an @('ArgsType' query)@ argument, and returns a
-- list of @('ResultType' query)@ values.
-- 
-- Therefore, we can invoke the query thusly:
-- 
-- > results <- query conn (Proxy :: Proxy MyQuery) (Only "Some Company")
-- 
-- The @('Only' "Some Company")@ argument fulfils the query parameters,
-- and the results will be a list of rows which can be deconstructed
-- using pattern matching. E.g.:
-- 
-- > sequence_
-- >   [
-- >     putStrLn (show name <> " - " <> show sallary)
-- >     | (Only name :> Only salary) <- results
-- >   ]

-- $insert
-- To insert values into our example tables above, we need to write a
-- couple of insert statements:
-- 
-- E.g.:
-- 
-- > type InsertCompany = InsertInto Company '["id", "name", "address"]
-- > type InsertEmployee = InsertInto Employee '["company_id", "id", "name", "birth_date"]
--
-- That's it really.  Insert statements are much simpler than select
-- queries.  These statement will automatically be parameterized according
-- to the listed fields.
--
-- There is a little bit of important nuance here: Note that
-- 'InsertEmployee' omits the "salary" field. That field is nullable,
-- and so the database will insert a null value when this insert statement
-- is used.
--
-- This can be particularly useful for allowing the database to supply
-- default values, such as auto-incremented id fields. This library is
-- not (yet) sophisticated enough understand which fields can safely be
-- omitted, so it lets you omit any field. If you omit a field for which
-- the database cannot supply a default value then that will result in a
-- runtime error. This is a problem we plan to fix in a future version. On
-- the other hand if you try to include a field that is not part of the
-- schema, you will get a /compile time/ error like you are supposed to.
--
-- To execute these insert statements, use "Database.Ribbit.PostgreSQL"'s
-- 'Database.Ribbit.PostgreSQL.execute' function:
--
-- > do
-- >   let
-- >     myBirthday :: Day
-- >     myBirthday = ...
-- >   execute
-- >     conn
-- >     (Proxy :: Proxy InsertCompany)
-- >     (Only 1 :> Only "Owens Murray" :> Only (Just "Austin, Tx"))
-- >   execute
-- >     conn
-- >     (Proxy :: Proxy InsertEmployee)
-- >     (Only 1 :> Only 1 :> Only "Rick" :> Only myBirthday)

-- $delete
-- Deleting a value is similar to inserting a value, but simpler because
-- you only have to specify the delete conditions (if there are any).
-- 
-- e.g.:
--  
-- > type DeleteAllEmployees = DeleteFrom Employee
-- > type DeleteEmployeeById =
-- >   DeleteFrom Employee
-- >   `Where` "id" `Equals` (?)
-- 
-- Then just execute the query, providing the appropriate query params.
-- 
-- > do
-- >   let
-- >     employeeId :: Int
-- >     employeeId = ...
-- >   execute
-- >     conn
-- >     (Proxy :: Proxy DeleteEmployeeById)
-- >     (Only employeeId)
-- >
-- >   -- Or maybe delete all employees.
-- >   execute
-- >     conn
-- >     (Proxy :: Proxy DeleteAllEmployees)
-- >     ()


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


{- | "!=" combinator for conditions. -}
data NotEquals l r
infix 9 `NotEquals`


{- | "<" combinator for conditions. -}
data Lt l r
infix 9 `Lt`


{- | "<=" combinator for conditions. -}
data Lte l r
infix 9 `Lte`


{- | ">" combinator for conditions. -}
data Gt l r
infix 9 `Gt`


{- | ">=" combinator for conditions. -}
data Gte l r
infix 9 `Gte`


{- | "AND" combinator for conditions. -}
data And l r
infixr 8 `And`


{- | "OR" combinator for conditions. -}
data Or l r
infixr 7 `Or`


{- | "AS" combinator, used for attaching a name to a table in a FROM clause. -}
data As relation name
infix 8 `As`


{- | NOT conditional combinator. -}
data Not a


{- | "?" combinator, used to indicate the presence of a query parameter. -}
data (?)


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


data Expr a


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

{- | Cross product -}
instance
    (Table table, KnownSymbol name)
  =>
    Table ((table `As` name) : moreTables)
  where
    type DBSchema ((table `As` name) : moreTables) =
      CrossProductSchema ((table `As` name) : moreTables)
    type Name ((table `As` name) : moreTables) =
      CrossProductName ((table `As` name) : moreTables)


{- | Produce the schema of a cross product. -}
type family CrossProductSchema cp where
  CrossProductSchema '[table `as` name] =
    Flatten (
      AliasAs name (DBSchema table)
    )
  CrossProductSchema ((table `As` name) : moreTables) =
    Flatten (
      AliasAs name (DBSchema table)
      :> CrossProductSchema moreTables
    )


{- | Product the renderable "name" of a cross product. -}
type family CrossProductName cp where
  CrossProductName '[table `As` name] = 
    Name table
    `AppendSymbol` " as "
    `AppendSymbol` name
  CrossProductName ((table `As` name) : moreTables) = 
    Name table
    `AppendSymbol` " as "
    `AppendSymbol` name
    `AppendSymbol` ", "
    `AppendSymbol` CrossProductName moreTables


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
  ArgsType (DeleteFrom relation `Where` conditions) =
    ArgsType (DBSchema relation, conditions)
  ArgsType (InsertInto relation '[]) =
    TypeError ('Lit.Text "Insert statement must specify at least one column.")
  ArgsType (InsertInto relation fields) =
    ProjectionType fields (DBSchema relation)

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


type family NotInSchema field schema where
  NotInSchema field schema =
    TypeError (
      'Lit.Text "name ("
      ':<>: 'ShowType field
      ':<>: 'Lit.Text ") not found in schema: "
      ':<>: 'ShowType schema
    )


{- | Type level check to see if the field is actually contained in the schema -}
type family ValidField field schema where
  ValidField name (Field name typ) = 'True
  ValidField name (Field _ typ) = 'False
  ValidField name (a :> b) = ValidField name a || ValidField name b


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
instance (KnownSymbol field, ReflectFields (field:more)) => Render (field:more) where
  render _proxy = T.intercalate "," (reflectFields (Proxy @(field:more)))

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

{- Not Equals -}
instance (Render (Expr l), Render (Expr r)) => Render (NotEquals l r) where
  render _proxy =
    render (Proxy @(Expr l))
    <> " != "
    <> render (Proxy @(Expr r))

{- Not -}
instance (Render a) => Render (Not a) where
  render _proxy =
    "not ("
    <> render (Proxy @a)
    <> ")"

{- Gt -}
instance (Render (Expr l), Render (Expr r)) => Render (Gt l r) where
  render _proxy =
    render (Proxy @(Expr l))
    <> " > "
    <> render (Proxy @(Expr r))

{- Gte -}
instance (Render (Expr l), Render (Expr r)) => Render (Gte l r) where
  render _proxy =
    render (Proxy @(Expr l))
    <> " >= "
    <> render (Proxy @(Expr r))

{- Lt -}
instance (Render (Expr l), Render (Expr r)) => Render (Lt l r) where
  render _proxy =
    render (Proxy @(Expr l))
    <> " < "
    <> render (Proxy @(Expr r))

{- Lte -}
instance (Render (Expr l), Render (Expr r)) => Render (Lte l r) where
  render _proxy =
    render (Proxy @(Expr l))
    <> " <= "
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

{- INSERT -}
instance
    (ReflectFields fields, KnownSymbol (Name table))
  =>
    Render (InsertInto table fields)
  where
    render _proxy =
      let
        fields :: [Text]
        fields = reflectFields (Proxy @fields)
      in
        "insert into " <> symbolVal (Proxy @(Name table))
        <> " (" <> T.intercalate ", " fields <> ")"
        <> " values (" <> T.intercalate ", " (const "?" <$> fields) <> ");"


{- DELETE -}
instance (KnownSymbol (Name table)) => Render (DeleteFrom table) where
  render _proxy =
    "delete from " <> symbolVal (Proxy @(Name table))


{- | Insert statement. -}
data InsertInto table fields


{- | Convert a type-level list of strings into a value. -}
class ReflectFields a where
  reflectFields :: proxy a -> [Text]
instance ReflectFields '[] where
  reflectFields _proxy = []
instance (KnownSymbol a, ReflectFields more) => ReflectFields (a:more) where
  reflectFields _proxy = symbolVal (Proxy @a) : reflectFields (Proxy @more)


{- | Delete statement. -}
data DeleteFrom table


