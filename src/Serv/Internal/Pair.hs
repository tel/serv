{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Serv.Internal.Pair where

-- | Equivalent to a tuple at both the type and kind levels,
-- but has a nicer syntax!
data Pair a b = a ::: b

infixr 6 :::
