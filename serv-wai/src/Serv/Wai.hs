{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Serv.Wai where

import           Data.Singletons
import           Data.Singletons.Prelude.List
import           Data.Singletons.TypeLits
import           Data.Text                     (Text)
import           GHC.Exts
import           Network.HTTP.Kinder.Header    (AllHeaderDecodes,
                                                AllHeaderEncodes,
                                                HeaderDecode (..))
import           Network.HTTP.Kinder.MediaType (AllMimeEncode)
import           Network.HTTP.Kinder.Query     (AllQueryDecodes)
import           Network.HTTP.Kinder.URI       (URIDecode (..))
import           Network.HTTP.Kinder.Status (Status)
import           Network.Wai                   (Application)
import           Serv.Api
import           Serv.Wai.Rec
import           Serv.Wai.Response             (SomeResponse)
import           Serv.Wai.Type

type family Impl (m :: * -> *) api where
  Impl m Abstract = m (Context -> Application)

  Impl m (OneOf apis) = HList (AllImpl m apis)
  Impl m (Endpoint ann hs) = HList (AllHandlers m hs)

  Impl m (Const s :> api) = Impl m api
  Impl m (HeaderAs s v :> api) = Impl m api
  Impl m (Seg s a :> api) = a -> Impl m api
  Impl m (Header n a :> api) = a -> Impl m api
  Impl m (Wildcard :> api) = [Text] -> Impl m api

type family AllImpl m apis where
  AllImpl m '[] = '[]
  AllImpl m (api ': apis) = Impl m api ': AllImpl m apis

type family AllHandlers m hs where
  AllHandlers m '[] = '[]
  AllHandlers m (h ': hs) = ImplHandler m h ': AllHandlers m hs

type family ImplHandler m h where
  ImplHandler m (CaptureBody ts a h) = a -> ImplHandler m h
  ImplHandler m (CaptureHeaders hs h) = FieldRec hs -> ImplHandler m h
  ImplHandler m (CaptureQuery qs h) = FieldRec qs -> ImplHandler m h
  ImplHandler m (Method v os) = m (SomeResponse os)

type family Constrain a :: Constraint where
  Constrain Abstract = ()

  Constrain (OneOf '[]) = ()
  Constrain (OneOf (api ': apis)) =
    (Constrain api, Constrain (OneOf apis))

  Constrain (Endpoint ann '[]) = ()
  Constrain (Endpoint ann (h ': hs)) =
    (ConstrainHandler h, Constrain (Endpoint ann hs))

  Constrain (Const s :> api) = Constrain api
  Constrain (HeaderAs s v :> api) = Constrain api
  Constrain (Seg s a :> api) = (Constrain api, URIDecode a)
  Constrain (Header n a :> api) = (Constrain api, HeaderDecode n a)
  Constrain (Wildcard :> api) = Constrain api

type family ConstrainHandler h :: Constraint where
  ConstrainHandler (Method verb os) =
    ConstrainOutputs os
  ConstrainHandler (CaptureBody ctypes a h) =
    ConstrainHandler h -- TODO
  ConstrainHandler (CaptureHeaders hs h) =
    (AllHeaderDecodes hs, ConstrainHandler h)
  ConstrainHandler (CaptureQuery qs h) =
    (AllQueryDecodes qs, ConstrainHandler h)

type family ConstrainOutputs (os :: [(Status, Output *)]) :: Constraint where
  ConstrainOutputs '[] = ()
  ConstrainOutputs (s ::: Respond hs Empty ': os) =
    (AllHeaderEncodes hs, ConstrainOutputs os)
  ConstrainOutputs (s ::: Respond hs (HasBody ts a) ': os) =
    (AllMimeEncode a ts, AllHeaderEncodes hs, ConstrainOutputs os)

server :: (Constrain api, Monad m) => Sing api -> Impl m api -> Server m
server SAbstract mApp = returnServer (fmap Application mApp)
server (SOneOf SNil) RNil = notFound
server (SOneOf (SCons api apis)) (Identity impl :& impls) =
  server api impl `orElse` server (SOneOf apis) impls
server (path :%> api) impl =
  Server $ case path of
    SConst sym -> withKnownSymbol sym $ do
      maySeg <- popSegment
      runServer $ case maySeg of
        Nothing -> notFound
        Just seg
          | seg /= fromString (symbolVal sym) -> notFound
          | otherwise -> server api impl
    SWildcard -> do
      segs <- popAllSegments
      runServer (server api (impl segs))
    SHeaderAs h sExp -> do
      let expected = fromString (withKnownSymbol sExp (symbolVal sExp))
      ok <- expectHeader h expected
      runServer $ if ok
        then server api impl
        else notFound
    SSeg _name _ty -> do
      trySeg <- popSegment
      runServer $ case trySeg of
        Nothing -> notFound
        Just seg ->
          case uriDecode seg of
            Left err -> badRequest (Just err)
            Right val -> server api (impl val)
    SHeader hdr _ty -> do
      tryVal <- getHeader hdr
      runServer $ case tryVal of
        Left err -> badRequest (Just err)
        Right val -> server api (impl val)
