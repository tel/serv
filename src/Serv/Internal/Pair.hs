{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Serv.Internal.Pair where

import           Data.Singletons.TH

-- | Equivalent to a tuple at both the type and kind levels,
-- but has a nicer syntax!

singletons [d| data Pair a b = a ::: b |]

infixr 6 :::
