# Ribbit

- [Ribbit](#ribbit)
    - [Status](#status)
        - [Current Features](#current-features)
            - [Basic @Select .. From ..](#basic-select--from-)
            - [Cross product](#cross-product)
            - [Limited Conditionals](#limited-conditionals)
    - [Roadmap](#roadmap)
        - [More conditional operatores.](#more-conditional-operatores)
        - [`CREATE TABLE` support.](#create-table-support)
        - [`INSERT INTO` support.](#insert-into-support)
    - [How it compares with other libraries.](#how-it-compares-with-other-libraries)
    - [The name: Ribbit](#the-name-ribbit)

Ribbit is yet another type safe relational database
library for Haskell, heavily inspired by the amazing
[Servant](http://hackage.haskell.org/package/servant) library. The goal
is to create a type-level language for defining table schemas "as a type",
queries that operate on those schemas, and, tangentially, "backends" that
can do something useful with those types like talk to an actual database.


Using Ribbit, you might expect to see something like this:

```haskell
type PeopleTable =
  Field "id" Int
  :> Field "name" Text
  :> Field "age" Int
  

type MyQuery = Select '["id", "name"] `From` PeopleTable `Where` "age" `Equals` (?)

matchingPeople <-
  query
    dbConn
    (Proxy :: Proxy MyQuery)
    (Only 21) -- argument that fills in the (?) placeholder

  :: IO [Only Int :> Only Text]

```

## Status

The status of Ribbit "Very Incomplete". My goal is to take a "depth first"
approach, where every feature added is production ready before moving on to the
next feature. Featured back-ends include `postgres-simple` at this time.

### Current Features

These are the features that are currently implemented.

#### Basic @Select .. From ..

We support queries of the form:

> type MyQuery = Select '["field1", "field2"] `From` MyTable

#### Cross product

We support queries of the form:

type MyQuery = Select '["t1.field1", "t2.field2"] `From` MyTable1 `As` "t1" `X` MyTable2 `As` "t2"

#### Limited Conditionals

We support queries of the form:

> type MyQuery = Select '["field1", "field2"] `From` MyTable `Where` <condition>

Where <condition> can include:

- a `And` b: Basic intersection.
- a `Or` b: Basic union.
- "field" `Equals` (?): Test for equality. This introduces a query parameter that must be supplied at runtime.
- "field1" `Equals` "field2": Test the equality of two fields (that both must exist in the schema)

## Roadmap

This is what I plan to work on next:

### More conditional operatores.

I want to fill in the conditional operators with the usual suspects (`<`, `<=`, `>`, `>=`, `!=`, etc.).

### `CREATE TABLE` support.

One source of programming bugs is when the schema in the database and the
schema described by your schema types get out of sync. It is never possible to
always ensure at compile time that your database will match your program when
actually run, but the addition of `CREATE TABLE` support will at least make it
possible for the database schema to have one source of truth in your codebase
(as opposed to having to maintain Ribbit schemas and also a corresponding
schema embedded in some database initialization script somewhere).

### `INSERT INTO` support.

Obviously, we want to do more with our DB than just read from it.

## How it compares with other libraries.

The short answer is there are a lot of other libraries and I'm not sure.
Persistent and esquelleto are ones I've used, but if you search "relational" or
"sql" in Hackage there seems to be a lot of other options. Part of the goals
for this library are to flesh out this approach myself, so I can have a better
context for understanding everything else available. In other words, it is part
research project. With that in mind, there are at least a couple of specific
goals I have in mind:

- Avoid template Haskell. Persistent is amazing, but the use of Template
  Haskell makes certain things difficult, like documenting (or for large
  projects even understanding) everything that is produced by the Template
  Haskell.

- Make the language easy to understand. If you have some basic SQL knowledge,
  it should be immediately obvious what is going on even if you are a beginner
  Haskeller.

- Try to make as much stuff happen at the type level as possible. The ability
  to write your own type classes or type families over Servant API types is, I feel,
  part of what makes Servant so amazing. I want to replicate that success here.
  So, for instance, if someone somewhere defines a schema type that looks like
  this:

  ```haskell
  type MySchema =
    Field "id" Int
    :> Field "name" Text
    :> Field "address" (Maybe Text)
  ```

  Then you would be free to deconstruct this type (using type families),
  transform it into another schema, generate customized `CREATE TABLE`
  statements if the (forthcoming) ones provided aren't good enough for your
  back-end or use case... that sort of thing. As a somewhat contrived example,
  maybe, for who knows what reason, you never want to allow null values in your
  database. You can write a type family that can inspect every field in an
  arbitrary schema, replacing all the `Maybe a` with just `a`, like:

  ```haskell
  -- With -XPolyKinds
  type family NoNulls schema where
    NoNulls (Field name (Maybe typ)) = Field name typ
    NoNulls (a :> b) = NoNulls a :> NoNulls b
    NoNulls a = a

  NoNulls MySchema 
  -- Same as:
  --   Field "id" Int
  --   :> Field "name" Text
  --   :> Field "address" Text <--- note the lack of Maybe
  ```


## The name: Ribbit

The name means nothing except I kindof like the sound of it. There are so many
"sql", "relational", "query", etc. package names already that I didn't want to:

1) get lost in the mix.
2) step on anyone's toes by choosing too similar a name.
3) create confusion by seeming to be associated with some other package with
   which I am not.

