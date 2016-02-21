{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeFamilies  #-}
{-# LANGUAGE TypeOperators #-}

module Serv.Internal.TypeLevel where

import           Data.Singletons
import           GHC.Exts

type family AllC (c :: TyFun k Constraint -> *) (rs :: [k]) :: Constraint where
  AllC c '[] = ()
  AllC c (r ': rs) = (c @@ r, AllC c rs)
