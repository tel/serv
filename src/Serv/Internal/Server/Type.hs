{-# LANGUAGE TypeOperators #-}

module Serv.Internal.Server.Type where


import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Either
import qualified Network.Wai                  as Wai
import           Serv.Internal.Server.Context (Context)
import           Serv.Internal.Server.Error   (RoutingError)
import qualified Serv.Internal.Server.Error   as Error

-- | A server implementation which always results in a "Not Found" error. Used to
-- give semantics to "pathological" servers like @'OneOf '[]@ and @Endpoint '[]@.
--
-- These servers could be statically disallowed but (1) they have a semantic
-- sense as described by this type exactly and (2) to do so would require the
-- creation and management of either larger types or non-empty proofs which would
-- be burdensome to carry about.
data NotHere = NotHere

-- | Either one thing or the other. In particular, often this is used when we are
-- describing either one server implementation or the other. Used to give
-- semantics to @'OneOf@ and @'Endpoint@.
data a :<|> b = a :<|> b

-- A server executing in a given monad. We construct these from 'Api'
-- descriptions and corresponding 'Impl' descriptions for said 'Api's.
-- Ultimately, a 'Server', or at least a 'Server IO', is destined to be
-- transformed into a Wai 'Wai.Appliation', but 'Server' tracks around more
-- information useful for interpretation and route finding.
newtype Server m =
  Server
  { runServer :: ReaderT Context (EitherT RoutingError m) Wai.Response }

-- | Run a server to its roots
rootServer :: Context -> Server m -> m (Either RoutingError Wai.Response)
rootServer ctx = runEitherT . flip runReaderT ctx . runServer

-- |
orElse :: Monad m => Server m -> Server m -> Server m
orElse a b = Server $ do
  ctx <- ask
  va <- lift (lift (rootServer ctx a))
  case va of
    Left e
      | Error.ignorable e -> runServer b
      | otherwise -> runServer a
    Right _ -> runServer a
