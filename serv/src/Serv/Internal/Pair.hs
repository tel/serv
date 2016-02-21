{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE PolyKinds     #-}
{-# LANGUAGE TypeOperators #-}

-- | Type synonym that's a bit nicer to read than normal type-tuples
module Serv.Internal.Pair where

type a ::: b = '( a, b )
infixr 6 :::
