{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}

-- | Co-records are sum types over an extensible set of options.
module Serv.Wai.Corec where

-- | A @'Corec' f rs@ is a value @f (r, s)@ for exactly one choice of @r@
-- and @s@ such that @(r, s)@ is an element of @rs@.
data Corec (f :: (k1, k2) -> *) (rs :: [(k1, k2)]) where
  Skip :: Corec f rs -> Corec f ( '(r, s) ': rs )
  Stop :: f '(r, s) -> Corec f ( '(r, s) ': rs )

-- | The judgement @'ElemOf' rs (r, s)@ is satisified when there's an
-- equality between @(r, s)@ and some element of @rs@. More than this,
-- however, we assume that the first match of @r@ in @rs@ will work and
-- then defer matching the @s@ component until later. This helps inference
-- so long as the assumption that @r@ is "index-like" holds
class ElemOf rs x where
  -- | Given a valid variant of a 'Corec' "forget" its identity into the
  -- larger 'Corec'.
  inject :: f x -> Corec f rs

instance {-# OVERLAPS #-} (s' ~ s) => ElemOf ( '(r, s) ': rs ) '(r, s') where
  inject = Stop

instance ElemOf rs '(r, s) => ElemOf ( '(r', s') ': rs) '(r, s) where
  inject = Skip . inject
