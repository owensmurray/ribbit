{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Ribbit.Select (
  -- * Query Structure
  Select,
  From,
  As,
  On,
  LeftJoin,

  -- * Type families
  RenderFieldList,
  RenderField,
  Expr,
) where


import Database.Ribbit.Conditions (RenderJoinConditions, Where,
  RenderConditions)
import Database.Ribbit.Params (ParamsType, ResultType, ParamsTypeSchema,
  ProjectionType)
import Database.Ribbit.Render (Render)
import Database.Ribbit.Table (Name, DBSchema, Field, (:>), Table,
  Flatten, Validate)
import GHC.TypeLits (AppendSymbol, Symbol)


{- | "SELECT" constructor, used for starting a @SELECT@ statement. -}
data Select fields


{- |
  "FROM" constructor, used for attaching a SELECT projection to a relation
  in the database.
-}
data From proj relation
infixl 6 `From`


{- | "AS" constructor, used for attaching a name to a table in a FROM clause. -}
data As relation (name :: Symbol)
infix 9 `As`

{- | Cross Product -}
instance (Table table) => Table ((table `As` alias) : more) where
  type Name ((table `As` alias):more) =
    CrossProductName ((table `As` alias) : more)

  type DBSchema ((table `As` alias) : more) =
    CrossProductSchema ((table `As` alias) : more)


{- | Product the renderable "name" of a cross product. -}
type family CrossProductName cp where
  CrossProductName '[table `As` name] = 
    Name table
    `AppendSymbol` " AS "
    `AppendSymbol` name
  CrossProductName ((table `As` name) : moreTables) = 
    Name table
    `AppendSymbol` " AS "
    `AppendSymbol` name
    `AppendSymbol` ", "
    `AppendSymbol` CrossProductName moreTables

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


{- | "ON" keyword, for joins.  -}
data On join (conditions :: *)
infix 7 `On`


{- | Left Joins. -}
data LeftJoin left right
infix 8 `LeftJoin`


type instance Render (From (Select proj) table) =
  "SELECT "
  `AppendSymbol` RenderFieldList proj (DBSchema table)
  `AppendSymbol` " FROM "
  `AppendSymbol` Name table

type family RenderFieldList fields schema where
  RenderFieldList '[field] schema =
    RenderField field schema

  RenderFieldList (f1:f2:more) schema =
    RenderField f1 schema
    `AppendSymbol` ", "
    `AppendSymbol` RenderFieldList (f2:more) schema

type family RenderField field schema where
  RenderField field schema =
    Validate field schema field


data Expr (a :: k)

instance Table (((l `As` lname) `LeftJoin` (r `As` rname)) `On` conditions) where
  type Name (((l `As` lname) `LeftJoin` (r `As` rname)) `On` conditions) =
    Name l
    `AppendSymbol` " AS "
    `AppendSymbol` lname
    `AppendSymbol` " LEFT JOIN "
    `AppendSymbol` Name r
    `AppendSymbol` " AS "
    `AppendSymbol` rname
    `AppendSymbol` " ON "
    `AppendSymbol`
      RenderJoinConditions
        conditions
        (
          LeftJoinSchema
            (AliasAs lname (DBSchema l))
            (AliasAs rname (DBSchema r))
        )

  type DBSchema (((l `As` lname) `LeftJoin` (r `As` rname)) `On` conditions) =
    LeftJoinSchema
      (AliasAs lname (DBSchema l))
      (AliasAs rname (DBSchema r))


{- | Produce the schema for a left join. -}
type family LeftJoinSchema l r where
  LeftJoinSchema l r =
    Flatten (l :> Nullable r)


{- | Make all the fields of a schema nullable. -}
type family Nullable schema where
  Nullable (Field name (Maybe typ)) =
    Field name (Maybe typ)

  Nullable (Field name typ) =
    Field name (Maybe typ)

  Nullable (a :> b) =
    Flatten (Nullable a :> Nullable b)


type instance Render (proj `From` table `Where` conditions) =
  Render (proj `From` table)
  `AppendSymbol` " WHERE "
  `AppendSymbol` RenderConditions conditions (DBSchema table)


type instance ParamsType (_ `From` relation `Where` conditions) =
  ParamsTypeSchema (DBSchema relation) conditions


type instance ParamsType (Select proj `From` relation) = ()


type instance ResultType (Select fields `From` relation) =
    ProjectionType fields (DBSchema relation)


