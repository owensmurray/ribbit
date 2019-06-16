{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


{- | No documentation yet. See README for now. -}
module Database.Ribbit (
  Select,
  From,
  X,
  Where,
  As,
  Equals,
  (:>)(..),
  ReflectRelation(..),
  Render(..),
  ArgsType,
  ResultType,
  Field,
  And,
  Or,
  type (?),
) where


import Data.Proxy (Proxy(Proxy))
import Data.Text (Text)
import Data.Tuple.Only (Only(Only))
import GHC.TypeLits (symbolVal, KnownSymbol, TypeError,
  ErrorMessage((:<>:), (:$$:), ShowType), AppendSymbol)
import qualified Data.Text as T
import qualified GHC.TypeLits as Lit


{-
  Select
    '[Foo :> "foo", Bar :> "bar"]
  `From` Foo `X` Bar
  `Where`
    Foo :> id `Equals` Bar :> "foo_id"
    `And` Foo :> "baz" `Equals` (?)
-}

class Render query where
  render :: proxy query -> Text

{- SELECT -}
instance (Render fields) => Render (Select fields) where
  render _proxy =
    "SELECT "
    <> render (Proxy @fields)

{- Field list -}
instance {-# OVERLAPS #-} (KnownSymbol field) => Render '[field] where
  render _proxy = T.pack $ symbolVal (Proxy @field)
instance (KnownSymbol field, Render more) => Render (field:more) where
  render _proxy =
    T.pack (symbolVal (Proxy @field)) <>  ", " <> render (Proxy @more)

{- FROM -}
instance (Render proj, ReflectRelation relation) => Render (From proj relation) where
  render _proxy =
    render (Proxy @proj)
    <> " FROM "
    <> reflectRelation (Proxy @relation)

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
  render _proxy = T.pack (symbolVal (Proxy @a))

{- (?) -}
instance Render (?) where
  render _proxy = "?"


data Select fields

data From proj relation
infixl 6 `From`

data Where query conditions
infixl 6 `Where`

data Equals l r
infix 9 `Equals`

data And l r
infixr 8 `And`

data Or l r
infixr 7 `Or`

data Field name typ

data X l r
infixr 7 `X`

data As relation name
infix 8 `As`

data a :> b = a :> b
  deriving (Eq, Ord, Show)
infixr 5 :>

data (?)

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

class ReflectRelation relation where
  type DBSchema relation
  reflectRelation :: proxy relation -> Text


{- | Cross product -}
instance (ReflectRelation l, ReflectRelation r, KnownSymbol lname, KnownSymbol rname) => ReflectRelation (l `As` lname `X` r `As` rname) where
  type DBSchema (l `As` lname `X` r `As` rname) =
    AliasAs lname (DBSchema l)
    :> AliasAs rname (DBSchema r)
  reflectRelation _prxoy =
    reflectRelation (Proxy @l)
    <> " as "
    <> T.pack (symbolVal (Proxy @lname))
    <> ", "
    <> reflectRelation(Proxy @r)
    <> " as "
    <> T.pack (symbolVal (Proxy @rname))

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


class TestVal a where
  testVal :: a

instance (TestVal a, TestVal b) => TestVal (a :> b) where
  testVal = testVal :> testVal
instance TestVal (Only Int) where
  testVal = Only 0
instance TestVal (Only Text) where
  testVal = Only "foo"


type family ResultType query where
  ResultType (Select fields `From` relation) =
    ProjectionType fields (DBSchema relation)
  ResultType (query `Where` conditions) = ResultType query
  ResultType query =
    TypeError ('Lit.Text "Malformed Query" ':$$: 'ShowType query)

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

type family Flatten a where
  Flatten ((a :> b) :> c) = Flatten (a :> b :> c)
  Flatten (a :> b) = a :> Flatten b
  Flatten a = a

type family StripUnit a where
  StripUnit (() :> a) = StripUnit a
  StripUnit (a :> ()) = StripUnit a
  StripUnit (a :> b) = a :> StripUnit b
  StripUnit a = a


