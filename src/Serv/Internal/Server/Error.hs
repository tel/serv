{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs     #-}
{-# LANGUAGE ScopedTypeVariables     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Serv.Internal.Server.Error where

-- | Errors which arise during the "handling" portion of dealing with a response.
data RoutingError
  = NotFound
  | BadRequest (Maybe String)
  | UnsupportedMediaType

-- | An ignorable error is one which backtracks the routing search
-- instead of forcing a response.
ignorableErr :: RoutingError -> Bool
ignorableErr NotFound = True
ignorableErr _ = False
