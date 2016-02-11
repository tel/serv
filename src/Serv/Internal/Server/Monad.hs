{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE KindSignatures              #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}

module Serv.Internal.Server.Monad where

import           Control.Monad.IO.Class
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Singletons
import           Data.Text                          (Text)
import           GHC.TypeLits
import qualified Network.HTTP.Types                 as HTTP
import           Serv.Internal.Api
import qualified Serv.Internal.Cors                 as Cors
import           Serv.Internal.Header               (HeaderType)
import           Serv.Internal.Header.Serialization (HeaderDecode (..))
import qualified Serv.Internal.Query                as Q
import qualified Serv.Internal.Server.Context       as Ctx
import           Serv.Internal.Verb

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

newtype InContext m a
  = InContext { _runInContext :: StateT Ctx.Context m a }
  deriving ( Functor, Applicative, Monad
           , MonadTrans
           , MonadState Ctx.Context
           , MonadIO
           )

runInContext :: Monad m => InContext m a -> Ctx.Context -> m (a, Ctx.Context)
runInContext = runStateT . _runInContext

instance Monad m => MonadReader Ctx.Context (InContext m) where
  ask = get
  local f m = do
    (_ctx, a) <- fork (modify f >> m)
    return a

mapInContext :: (forall x . m x -> n x) -> InContext m a -> InContext n a
mapInContext phi (InContext r) = InContext (mapStateT phi r)

getVerb :: Monad m => InContext m (Maybe Verb)
getVerb = fmap parseVerb (asks Ctx.method)

pathIsEmpty :: Monad m => InContext m Bool
pathIsEmpty = asks Ctx.pathIsEmpty

fork :: Monad m => InContext m a -> InContext m (Ctx.Context, a)
fork (InContext m) = do
  ctx <- ask
  (a, newCtx) <- lift (runStateT m ctx)
  return (newCtx, a)

restore :: Monad m => (Ctx.Context, a) -> InContext m a
restore (ctx, a) = put ctx >> return a

takeSegment :: Monad m => InContext m (Maybe Text)
takeSegment = InContext (state $ swap . Ctx.takeSegment)

takeAllSegments :: Monad m => InContext m [Text]
takeAllSegments = InContext (state $ swap . Ctx.takeAllSegments)

addCorsPolicy :: Monad m => Cors.Policy -> InContext m ()
addCorsPolicy policy = modify $ \ctx ->
  ctx { Ctx.corsPolicies = policy : Ctx.corsPolicies ctx }

examineHeader :: (Monad m, HeaderDecode n a) => Sing n -> InContext m (Either String a)
examineHeader s = state (swap . Ctx.examineHeader s)

expectHeader :: forall m (n :: HeaderType Symbol) . Monad m => Sing n -> Text -> InContext m Bool
expectHeader s value = state (swap . Ctx.expectHeader s value)

examineQuery :: (Monad m, Q.QueryDecode s a) => Sing s -> InContext m (Either String a)
examineQuery s = state (swap . Ctx.examineQuery s)

corsHeaders
  :: forall m (hs :: [Handler Nat Symbol *])
  . Monad m => Sing hs -> Cors.IncludeMethods -> InContext m (Maybe [HTTP.Header])
corsHeaders s incl = do
  ctx <- get
  return (Ctx.corsHeaders s incl ctx)
