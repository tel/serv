{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Serv.Internal.Rec where

import           Data.Proxy
import           Data.Tagged
import           GHC.Exts
import           Serv.Internal.Pair

-- | An HList collecting heterogenous types matched up to labeling information
data Rec rs where
  Nil :: Rec '[]
  Cons :: ty -> Rec rs -> Rec ( name ::: ty ': rs )

-- | Append a new header value on to a record
(-:) :: Proxy name -> ty -> Rec rs -> Rec (name ::: ty ': rs)
(-:) _ = Cons

type family AllC (c :: k1 -> k2 -> Constraint) (rs :: [ (k1, k2) ]) :: Constraint where
  AllC c '[] = ()
  AllC c (name ::: ty ': xs) = (c name ty, AllC c xs)

class Elem name e es where
  eGet :: Rec es -> Tagged name e
  eMap :: Tagged name (e -> e) -> Rec es -> Rec es

instance {-# OVERLAPPING #-} Elem name e (name ::: e ': rs) where
  eGet (Cons x _) = Tagged x
  eMap (Tagged f) (Cons x rs) = Cons (f x) rs

instance {-# OVERLAPPABLE #-} Elem name e rs => Elem name e (r ': rs) where
  eGet (Cons _ rs) = eGet rs
  eMap f (Cons x rs) = Cons x (eMap f rs)

class Subset rs qs where
  project :: Rec qs -> Rec rs

instance Subset '[] qs where
  project _ = Nil

instance (Elem name r qs, Subset rs qs) => Subset (name ::: r ': rs) qs where
  project r =
    let Tagged v = (eGet r :: Tagged name r)
    in Cons v (project r)

class RecordIso rs qs where
  reorder :: Rec rs -> Rec qs

instance (Subset rs qs, Subset qs rs) => RecordIso rs qs where
  reorder = project
