{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

{- | SQL statement conditions. -}
module Database.Ribbit.Conditions (
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

  RenderConditions,
  RenderJoinConditions,
) where


import Database.Ribbit.Table (Validate)
import GHC.TypeLits (Symbol, AppendSymbol, TypeError, ErrorMessage((:<>:),
  ShowType))
import qualified GHC.TypeLits as Lit


{- | "WHERE" constructor, used for attaching conditions to a query. -}
data Where query conditions
infixl 6 `Where`


{- | "=" constructor for conditions. -}
data Equals (l :: k1) (r :: k2)
infix 9 `Equals`


{- | "!=" constructor for conditions. -}
data NotEquals l r
infix 9 `NotEquals`


{- | "<" constructor for conditions. -}
data Lt l r
infix 9 `Lt`


{- | "<=" constructor for conditions. -}
data Lte l r
infix 9 `Lte`


{- | ">" constructor for conditions. -}
data Gt l r
infix 9 `Gt`


{- | ">=" constructor for conditions. -}
data Gte l r
infix 9 `Gte`


{- | "AND" constructor for conditions. -}
data And l r
infixr 8 `And`


{- | "OR" constructor for conditions. -}
data Or (l :: k1) (r :: k2)
infixr 7 `Or`


{- | NOT conditional constructor. -}
data Not a


{- | Is a field null? -}
data IsNull (field :: Symbol)


{- | Is a field not null? -}
data NotNull (field :: Symbol)


type family RenderConditions a schema where
  RenderConditions (Or l r) schema =
    "( "
    `AppendSymbol` RenderConditions l schema
    `AppendSymbol` " ) OR ("
    `AppendSymbol` RenderConditions r schema
    `AppendSymbol` " )"

  RenderConditions (And l r) schema =
    "( "
    `AppendSymbol` RenderConditions l schema
    `AppendSymbol` " ) AND ( "
    `AppendSymbol` RenderConditions r schema
    `AppendSymbol` " )"

  RenderConditions condition schema = RenderCondition condition schema


type family RenderCondition condition schema where
  RenderCondition (Equals l r) schema = SimpleCondition schema (Expr l) "=" (Expr r)
  RenderCondition (NotEquals l r) schema = SimpleCondition schema (Expr l) "!=" (Expr r)
  RenderCondition (Lt l r) schema = SimpleCondition schema (Expr l) "<" (Expr r)
  RenderCondition (Lte l r) schema = SimpleCondition schema (Expr l) "<=" (Expr r)
  RenderCondition (Gt l r) schema = SimpleCondition schema (Expr l) ">" (Expr r)
  RenderCondition (Gte l r) schema = SimpleCondition schema (Expr l) ">=" (Expr r)
  RenderCondition (IsNull field) schema = Validate field schema (
      field `AppendSymbol` " IS NULL"
    )
  RenderCondition (NotNull field) schema = Validate field schema (
      field `AppendSymbol` " IS NOT NULL"
    )
  RenderCondition a _ = TypeError ('Lit.Text "Invalid condition: " ':<>: 'ShowType a)


type family SimpleCondition schema l op r where
  SimpleCondition schema (Expr (?)) op (Expr r) =
    Validate r schema (
      "? "
      `AppendSymbol` op
      `AppendSymbol` " "
      `AppendSymbol` r
    )
  SimpleCondition schema (Expr l) op (Expr (?)) =
    Validate l schema (
      l
      `AppendSymbol` " "
      `AppendSymbol` op
      `AppendSymbol` " ?"
    )
  SimpleCondition schema (Expr l) op (Expr r) =
    Validate l schema (
      Validate r schema (
        l
        `AppendSymbol` " "
        `AppendSymbol` op
        `AppendSymbol` " "
        `AppendSymbol` r
      )
    )


data Expr (a :: k)


type family RenderJoinConditions a schema where
  RenderJoinConditions (Or l r) schema =
    "( "
    `AppendSymbol` RenderJoinConditions l schema
    `AppendSymbol` " ) OR ("
    `AppendSymbol` RenderJoinConditions r schema
    `AppendSymbol` " )"

  RenderJoinConditions (And l r) schema =
    "( "
    `AppendSymbol` RenderJoinConditions l schema
    `AppendSymbol` " ) AND ( "
    `AppendSymbol` RenderJoinConditions r schema
    `AppendSymbol` " )"

  RenderJoinConditions condition schema = RenderJoinCondition condition schema


type family RenderJoinCondition condition schema where
  RenderJoinCondition (Equals l r) schema = ClosedCondition schema (Expr l) "=" (Expr r)
  RenderJoinCondition (NotEquals l r) schema = ClosedCondition schema (Expr l) "!=" (Expr r)
  RenderJoinCondition (Lt l r) schema = ClosedCondition schema (Expr l) "<" (Expr r)
  RenderJoinCondition (Lte l r) schema = ClosedCondition schema (Expr l) "<=" (Expr r)
  RenderJoinCondition (Gt l r) schema = ClosedCondition schema (Expr l) ">" (Expr r)
  RenderJoinCondition (Gte l r) schema = ClosedCondition schema (Expr l) ">=" (Expr r)
  RenderJoinCondition (IsNull field) schema = Validate field schema (
      field `AppendSymbol` " IS NULL"
    )
  RenderJoinCondition (NotNull field) schema = Validate field schema (
      field `AppendSymbol` " IS NOT NULL"
    )
  RenderJoinCondition a _ = TypeError ('Lit.Text "Invalid condition: " ':<>: 'ShowType a)


{- | A closed condition is one that does not allow query parameters. -}
type family ClosedCondition schema l op r where
  ClosedCondition schema (Expr l) op (Expr r) =
    Validate l schema (
      Validate r schema (
        l
        `AppendSymbol` " "
        `AppendSymbol` op
        `AppendSymbol` " "
        `AppendSymbol` r
      )
    )


{- | "?" constructor, used to indicate the presence of a query parameter. -}
data (?)


