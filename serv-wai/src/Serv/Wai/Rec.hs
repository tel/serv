{-# LANGUAGE DataKinds, PolyKinds, GADTs #-}

-- | Re-exports of useful "Data.Vinyl" 'Rec' types
module Serv.Wai.Rec (

    ElField (..)
  , FieldRec
  , (=:)
  , Rec (..)
  , (<+>)
  , (++)

) where

import Data.Vinyl.Core

-- | A more kind polymorphic element field than what's normally available
-- in "Data.Vinyl"
data ElField field where
  ElField :: !a -> ElField '(k, a)

-- | A 'FieldRec' is a record of types tagged by some kind of "name".
type FieldRec hs = Rec ElField hs

(=:) :: sing a -> v -> FieldRec '[ '(a, v) ]
_ =: v = ElField v :& RNil
