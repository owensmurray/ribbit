# Ribbit

- [Ribbit](#ribbit)
    - [Status](#status)
        - [Current Features](#current-features)
            - [Basic @Select .. From ..](#basic-select--from-)
            - [Cross product](#cross-product)
            - [Conditionals](#conditionals)
            - [Limited `CREATE TABLE` support.](#limited-create-table-support)
            - [`INSERT INTO` support.](#insert-into-support)
            - [`DELETE FROM` support.](#delete-from-support)
    - [Roadmap](#roadmap)
        - [UPDATE support](#update-support)
        - [Flesh out Haskell to PostgreSQL type mapping.](#flesh-out-haskell-to-postgresql-type-mapping)
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

The status of Ribbit "Very Incomplete". My goal is to take a "depth
first" approach, where every feature added is production ready
before moving on to the next feature. Featured back-ends include
[postgresql-simple](https://hackage.haskell.org/package/postgresql-simple)
at this time.

### Current Features

These are the features that are currently implemented.

#### Basic @Select .. From ..

We support queries of the form:

```haskell
type MyQuery = Select '["field1", "field2"] `From` MyTable
```

#### Cross product

We support queries of the form:

```haskell
type MyQuery = Select '["t1.field1", "t2.field2"] `From` '[MyTable1 `As` "t1", MyTable2 `As` "t2"]
```

#### Conditionals

We support queries of the form:

```haskell
type MyQuery = Select '["field1", "field2"] `From` MyTable `Where` <condition>
```

Where ```<condition>``` can include:

- ```a `And` b```: Basic intersection.
- ```a `Or` b```: Basic union.
- ```"field" `Equals` (?)```: Test for equality. This introduces a query parameter that must be supplied at runtime.
- ```"field1" `Equals` "field2"```: Test the equality of two fields (that both must exist in the schema)
- ```a `Lt` b```: Less than operator.
- ```a `Lte` b```: Less than or equal to operator.
- ```a `Gt` b```: Greater than operator.
- ```a `Gte` b```: Greater than or equal to operator.
- ```Not a```: Not operator.
- ```"field" `NotEquals` (?)```: Test for inequality against a query parameter.
- ```"field1" `NotEquals` "field2"```: Test for inequality between two fields.

#### Limited `CREATE TABLE` support.

The postgresql-simple backend supports creating tables in the
database. This support is "limited" because it misses the following
features:

- PostgreSQL column types are know for only a small number of Haskell types.
  This is extensible by the user (by implementing a type class), but it would
  be nice to have a more comprehensive set of mappings out of the box.

- Foreign key constraints are not yet supported.

- Arbitrary non-primary-key indexes are not yet supported.

What *IS* supported already is:

- Determining whether a field is nullable, based on whether the corresponding
  Haskell type is wrapped in a `Maybe`.

- Compound primary keys. I.e. primary keys consisting of more than one
  component.

#### `INSERT INTO` support.

Basic inserts are supported:

```haskell
type MyInsert = InsertInto PeopleTable '["id", "name", "age"]

execute
  conn
  (Proxy :: Proxy MyInsert)
  (Only 1 :> Only "Bob Marley" :> Only 36)
```

#### `DELETE FROM` support.

Basic deletes are supported:

```haskell
type MyDeleteEveryone = DeleteFrom PeopleTable
type MyDeleteById = DeleteFrom PeopleTable `Where` id `Equals` (?)

execute
  conn
  (Proxy :: Proxy MyDeleteEveryone)
  ()

execute
  conn
  (Proxy :: Proxy MyDeleteById)
  (Only 1)
```

## Roadmap

This is what I plan to work on next:

### UPDATE support

Support update operations.

### Flesh out Haskell to PostgreSQL type mapping.

As mentioned above, only a small number of Haskell types are mapped
to PostgreSQL types out of the box. We would like to make this a more
comprehensive mapping out of the box.

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
  statements if the ones provided aren't good enough for your back-end or
  use case... that sort of thing. As a somewhat contrived example: maybe,
  for who knows what reason, you never want to allow null values in your
  database. You can write a type family that can inspect every field in
  an arbitrary schema, replacing all the `Maybe a` with just `a`, like:

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

