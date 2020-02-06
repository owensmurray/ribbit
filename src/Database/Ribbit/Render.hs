{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}


{- | Render queries. -}
module Database.Ribbit.Render (
  Render
) where


import GHC.TypeLits (Symbol)


{- | Render a query. -}
type family Render a :: Symbol


