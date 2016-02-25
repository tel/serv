{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE PolyKinds          #-}

-- | Re-exports of useful "Data.Vinyl" 'Rec' types
module Serv.Wai.Rec (

  -- * Specialized records

  -- ** 'FieldRec'
    ElField (..)
  , FieldRec

  -- ** 'HList'
  , Identity (..)
  , HList

  , (=:)
  , Rec (..)
  , (<+>)
  , (++)

  -- * Type-level methods
  , type (++)

) where

import           Data.Functor.Identity
import           Data.Singletons
import           Data.Vinyl.Core
import           Data.Vinyl.TypeLevel

-- FieldRec
-- ----------------------------------------------------------------------------

-- | A more kind polymorphic element field than what's normally available
-- in "Data.Vinyl"
data ElField field where
  ElField :: Sing k -> !a -> ElField '(k, a)

-- | A 'FieldRec' is a record of types tagged by some kind of "name".
type FieldRec hs = Rec ElField hs

(=:) :: Sing a -> v -> FieldRec '[ '(a, v) ]
s =: v = ElField s v :& RNil

-- HList
-- ----------------------------------------------------------------------------

type HList = Rec Identity
