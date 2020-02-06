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
  constructors provided by this library. The same goes for queries. Each
  query is a separate type defined with constructors from this library.

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

  -- ** Updating values
  -- $update

  -- * Schema Definition Types
  Table(..),
  Field,
  (:>)(..),

  -- * SQL Statement Constructors
  -- ** Query Constructors
  {- |
    Types in the section are used to construct SELECT queries.

    e.g.

    List all employees who have quit, for all companies. (Left Joined
    version, which shows companies that have had no employees quit.)

    > Select '["c.name", "e.name", "e.quit_date"]
    > `From`
    >   Company `As` "c"
    >   `LeftJoin`  Employee `As` "e" `On` "c.id" `Equals` "e.company_id"
    > `Where`
    >   NotNull "e.quit_date"

    List all employees who have quit, for all companies. (Inner join
    version, which omits companies from which no employee has quit.)

    > Select '["c.name", "e.name", "e.quit_date"]
    > `From` '[Company `As` "c", Employee `As` "e"]
    > `Where`
    >   "c.id" `Equals` "e.company_id"
    >   `And` NotNull "e.quit_date"
  -}
  Select,
  From,
  As,
  On,
  LeftJoin,

  -- ** Insert Constructors
  {- |
    Construct insert statements.

    e.g.

    Insert one row into the Employee Table.

    > InsertInto Employee '["id", "company_id", "name"]

    The values which are inserted into the specified fields are provided
    as query parameters.
  -}
  InsertInto,

  -- ** Delete Constructors
  {- |
    Construct delete statements.

    e.g.

    Delete all rows from the Company table:
    
    > type Statement = DeleteFrom Company

    Delete a specific row from the Company table:

    > type Statement = DeleteFrom Company `Where` "id" `Equals` (?)
  -}
  DeleteFrom,

  -- ** Update Constructors
  {- |
    Construct update statements

    e.g.

    Update a specific employee's salary:

    > type Statement =
    >   Update Employee '["salary"]
    >   `Where`
    >     "company_id" `Equals` (?)
    >     `And` "id" `Equals` (?)

    The values which are inserted into the specified fields are provided
    as query parameters.

    e.g.

    > execute
    >   conn
    >   Statement
    >   (Only newSalary :> Only companyId :> Only employeeId)
  -}
  Update,

  -- ** Condition Constructors
  {- | Use these types to construct a @WHERE@ clause. -}

  Where,
  Equals,
  NotEquals,
  Lt,
  Lte,
  Gt,
  Gte,
  And,
  Or,
  Not,
  IsNull,
  NotNull,
  type (?),

  -- * Transformations on Statement Types
  {- |
    These type families are useful for transforming the query types in various
    ways, or extracting certain information from them.

    e.g.

    Given the query:

    > type Query = Select '["name"] `From` Company `Where` "id" `Equals` (?)

    Render the query as a 'String' value (using 'GHC.TypeLits.symbolVal'):

    > symbolVal (Proxy :: Proxy (Render Query)) == "SELECT name FROM companies WHERE id = ?"

    Produce the Haskell type corresponding to the query parameters for a select
    statement:

    > ArgsType Query ~ Only Int -- Our statement has only one parameter, which is an int.

    Produce the Haskell type corresponding to the rows produced by the query:

    > ResultType Query ~ Only Text -- Our query procudes only one column, a text.

  -}
  ArgsType,
  ResultType,
  Render,
  -- ValidField,
  -- ProjectionType,

  -- -- * Statement Rendering
  -- Render,

) where


import Database.Ribbit.Args (ArgsType, ResultType)
import Database.Ribbit.Conditions (Where, Equals, NotEquals, Lt, Lte,
  Gt, Gte, And, Or, Not, IsNull, NotNull, type (?))
import Database.Ribbit.Delete (DeleteFrom)
import Database.Ribbit.Insert (InsertInto)
import Database.Ribbit.Render (Render)
import Database.Ribbit.Select (Select, From, As, On, LeftJoin)
import Database.Ribbit.Table (Table(Name, DBSchema), Field, (:>)((:>)))
import Database.Ribbit.Update (Update)

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
-- >   type Name Company = "companies"
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
-- > instance Table Employee where
-- >   type Name Employee = "employees"
-- >   type DBSchema Employee =
-- >     Field "company_id" Int
-- >     :> Field "id" Int
-- >     :> Field "name" Text
-- >     :> Field "salary" (Maybe Int)
-- >     :> Field "quit_date" (Maybe Day)
-- >     :> Field "birth_date" Day

-- $query
-- To write queries against these tables, use the query constructors
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
-- > query ::
-- >      forall m query.
-- >      ( MonadIO m
-- >      , KnownSymbol (Render query)
-- >      , ToRow (ArgsType query)
-- >      , FromRow (ResultType query)
-- >      )
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
-- >     putStrLn (show name <> " - " <> show salary)
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
-- >   `Where`
-- >     "company_id" `Equals` (?)
-- >     `And` "id" `Equals` (?)
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

-- $update
-- Updating values is almost the same as inserting values. Instead of
-- specifying the fields that get inserted, you specify the fields that get
-- updated, along with the conditions that match the rows to be updated.
--
-- > {- Update an employee's salary (hopefully a raise!). -}
-- > type UpdateSalary =
-- >   Update '[ "salary" ]
-- >   `Where` 
-- >     "company_id" `Equals` (?)
-- >     "id" `Equals` (?)
-- > 
-- > ...
-- > 
-- > let
-- >   newSalary :: Int
-- >   newSalary = 2
-- > 
-- >   targetCompany :: Int
-- >   targetCompany = 1
-- > 
-- >   targetEmployee :: Int
-- >   targetEmployee = 1
-- > 
-- > in
-- >   execute
-- >     conn
-- >     (Proxy :: Proxy UpdateSalary)
-- >     (Only newSalary :> Only targetCompany :> Only targetEmployee)


