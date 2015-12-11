{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE GADTs         #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Serv.Internal.Rec where

import           Data.Proxy
import           Serv.Internal.Pair

-- | An HList collecting heterogenous types matched up to labeling information
data Rec rs where
  Nil :: Rec '[]
  Cons :: ty -> Rec rs -> Rec ( name '::: ty ': rs )

-- | Append a new header value on to a record
(-:) :: Proxy name -> ty -> Rec rs -> Rec (name '::: ty ': rs)
(-:) _ = Cons
