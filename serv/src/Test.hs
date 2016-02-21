{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test where

import           Data.Singletons
import           Data.Singletons.TH
import           Data.Singletons.TypeLits
import           GHC.Exts

data Corec :: [*] -> * where
  Here :: a -> Corec (a ': as)
  There :: Corec as -> Corec (a ': as)

type family AllShow (as :: [*]) :: Constraint where
  AllShow '[] = ()
  AllShow (a ': as) = (Show a, AllShow as)

deriving instance AllShow as => Show (Corec as)

class Ok a where
  ok :: a -> ()

instance Ok Bool where ok _ = ()
instance Ok Int where ok _ = ()
instance Ok String where ok _ = ()
instance Ok () where ok _ = ()

type family AllOk (a :: [*]) :: Constraint where
  AllOk '[] = ()
  AllOk (a ': as) = (Ok a, AllOk as)

okCorec :: AllOk as => Corec as -> ()
okCorec (Here a) = ok a
okCorec (There next) = okCorec next

class Inject as a where
  inject :: a -> Corec as

instance {-# OVERLAPPING #-} Inject (a ': as) a where
  inject = Here

instance Inject as a => Inject (x ': as) a where
  inject = There . inject

injected :: Corec [Bool, Int, String, ()]
injected = inject ()

isOk :: ()
isOk = okCorec injected
