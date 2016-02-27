{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

-- | Unlike most other HTTP kinds descrbed in @http-kinder@, 'MediaType's
-- are special in that they're expected to be /very/ open to extension and
-- therefore are constrained only by kind @*@.
module Network.HTTP.Kinder.MediaType (

  -- * Classes for encoding and decoding
    HasMediaType (..)
  , MimeEncode (..)
  , MimeDecode (..)

  -- ** Listing constraints to type-level lists
  , AllMimeEncode
  , AllMimeDecode

  -- * Common content types

  , TextPlain
  , JSON

  -- ** Content type modifiers

  , Ver (..)

  -- * Content negotiation
  , negotiatedMimeEncode
  , negotiatedMimeDecode

  -- ** Utilities for negotiation
  , NegotiatedDecodeResult (..)
  , encodersOf
  , decodersOf

  -- * Re-exports from "Network.HTTP.Media"
  , MediaType (), (//), (/:)
  , mainType, subType, (/?), (/.)
  , Quality ()

) where

import qualified Data.Aeson                   as Aeson
import qualified Data.ByteString              as S
import qualified Data.ByteString.Lazy         as Sl
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.Prelude.List (Sing (SCons, SNil))
import           Data.Text                    (Text)
import qualified Data.Text.Encoding           as Text
import           GHC.Exts
import           GHC.TypeLits
import           Network.HTTP.Media           (MediaType (), Quality (),
                                               mainType, matchQuality, subType,
                                               (/.), (//), (/:), (/?))

-- Encoding and decoding
-- ----------------------------------------------------------------------------

-- | Any Haskell type which instantiates 'HasMediaType' can be thought of
-- as a representative of that 'MediaType'. Users can construct their own
-- types and then instantiate 'HasMediaType' to extend the media type system.
class HasMediaType t where
  mediaType :: sing t -> MediaType

-- | 'MediaType's represent ways of encoding data as a bytestream---this
-- class encodes that representation.
class HasMediaType t => MimeEncode t a where
  mimeEncode :: sing t -> a -> S.ByteString

-- | 'MediaType's represent ways of encoding data as a bytestream---this
-- class provides parsers for this representation.
class HasMediaType t => MimeDecode t a where
  mimeDecode :: sing t -> S.ByteString -> Either String a

-- Special constraints
-- ----------------------------------------------------------------------------

-- | For a given concrete type @a@, a list of types @ts@ satisfies
-- @AllMimeEncode a ts@ if each @t@ in @ts@ has @'MimeEncode' t a@.
type family AllMimeEncode (a :: *) (ts :: [*]) :: Constraint where
  AllMimeEncode a '[] = ()
  AllMimeEncode a (t ': ts) = (MimeEncode t a, AllMimeEncode a ts)

-- | For a given concrete type @a@, a list of types @ts@ satisfies
-- @MAllMimeDecode a ts@ if each @t@ in @ts@ has @'MimeDecode' t a@.
type family AllMimeDecode (a :: *) (ts :: [*]) :: Constraint where
  AllMimeDecode a '[] = ()
  AllMimeDecode a (t ': ts) = (MimeDecode t a, AllMimeDecode a ts)

-- Content negotiation
-- ----------------------------------------------------------------------------

-- | Provided a 'Sing' representing a type-level list of mediatypes,
-- produce a concrete mapping from 'MediaType's to encoding functions.
encodersOf
  :: AllMimeEncode a ts
  => Sing ts -> Map MediaType (a -> S.ByteString)
encodersOf s =
  case s of
    SNil -> Map.empty
    SCons r rs -> Map.insert (mediaType r) (mimeEncode r) (encodersOf rs)

-- | Provided a 'Sing' representing a type-level list of mediatypes,
-- produce a concrete mapping from 'MediaType's to decoding functions.
decodersOf
  :: AllMimeDecode a ts
  => Sing ts -> Map MediaType (S.ByteString -> Either String a)
decodersOf s =
  case s of
    SNil -> Map.empty
    SCons r rs -> Map.insert (mediaType r) (mimeDecode r) (decodersOf rs)

-- | Encode a value using a list of valid media types and a list of
-- @Accept@able media types. If no provided media type is acceptable then
-- the first of the provided is chosen by default. If the valid media type
-- listing is empty then no encoder can be negotiated ever---we fail early.
negotiatedMimeEncode
  :: AllMimeEncode a ts
  => Sing ts
  -> Maybe ([Quality MediaType] -> a -> (MediaType, S.ByteString))
negotiatedMimeEncode SNil = Nothing
negotiatedMimeEncode valid@(SCons defaultMt _) =
  -- This memoizes the construction of the encoders map so we do less
  -- type-level list work.
  Just (encode defaultEnc (Map.keys encoderMap) encoderMap)
  where
    encoderMap = encodersOf valid
    defaultEnc = (mediaType defaultMt, mimeEncode defaultMt)

    encode (theDefaultMt, theDefaultEnc) provided theEncMap acceptable a =
      maybe (theDefaultMt, theDefaultEnc a) id $ do
        mt <- matchQuality provided acceptable
        enc <- Map.lookup mt theEncMap
        return (mt, enc a)

-- | Negoatiated decodes can fail for two reasons: it could be that the
-- decoder failed (malformed input) or that the negotiation failed (a
-- content type was provided which isn't accepted by the server).
data NegotiatedDecodeResult a
  = NegotiatedDecode a
  | NegotiatedDecodeError String
  | DecodeNegotiationFailure MediaType
  deriving (Eq, Ord, Show)

resultDecode :: Either String a -> NegotiatedDecodeResult a
resultDecode res =
  case res of
    Left err -> NegotiatedDecodeError err
    Right val -> NegotiatedDecode val

-- | Decode a value using a list of valid media types and (maybe)
-- a provided @Content-Type@ 'MediaType'. If the @Content-Type@ is not
-- provided then the decoder for the first valid content type is attempted.
-- If the valid media type listing is empty then no decoder could ever be
-- negotiated---we fail early.
negotiatedMimeDecode
  :: AllMimeDecode a ts
  => Sing ts
  -> Maybe (Maybe MediaType -> S.ByteString -> NegotiatedDecodeResult a)
negotiatedMimeDecode SNil = Nothing
negotiatedMimeDecode valid@(SCons defaultMt _) =
  -- This memoizes the construction of the decoders map so we do less
  -- type-level list work.
  Just (decode defaultDec decoderMap)
  where
    decoderMap = decodersOf valid
    defaultDec = (mediaType defaultMt, mimeDecode defaultMt)

    decode (_theDefaultMt, theDefaultDec) theDecMap maybeCt bytes =
      case maybeCt of
        Nothing -> resultDecode (theDefaultDec bytes)
        Just ct ->
          case Map.lookup ct theDecMap of
            Nothing -> DecodeNegotiationFailure ct
            Just dec -> resultDecode (dec bytes)

-- | Versions a media type using mime type parameterization. @'Ver' 1 JSON@
-- has a media type like @"application/json; version=1"@. To use 'Ver'
-- create instances, e.g., for @'MimeEncode' ('Ver' n t) a@ which
-- specialize encoders for type @t@
newtype Ver (n :: Nat) a = Ver { getVer :: a }
  deriving (Eq, Ord, Show, Functor)

instance Applicative (Ver n) where
  pure = Ver
  Ver f <*> Ver a = Ver (f a)

instance Monad (Ver n) where
  return = pure
  Ver x >>= f = f x

instance (HasMediaType t, KnownNat n) => HasMediaType (Ver n t) where
  mediaType _ = mediaType (Proxy :: Proxy t) /: ("version", fromString (show (natVal (Proxy :: Proxy n))))

-- | The 'TextPlain' media type ("text/plain") is unformatted, raw text.
data TextPlain

instance HasMediaType TextPlain where
  mediaType _ = "text" // "plain"

instance MimeEncode TextPlain Text where
  mimeEncode _ = Text.encodeUtf8

-- | The 'JSON' media type ("application/json") is JSON formatted text. Any
-- Haskell type with 'Aeson.ToJSON' or 'Aeson.FromJSON' values can
-- participate.
data JSON

instance HasMediaType JSON where
  mediaType _ = "application" // "json"

instance Aeson.ToJSON a => MimeEncode JSON a where
  mimeEncode _ = Sl.toStrict . Aeson.encode

instance Aeson.FromJSON a => MimeDecode JSON a where
  mimeDecode _ bs = Aeson.eitherDecodeStrict bs

-- | Instances which handle potentially missing JSON values
instance {-# OVERLAPPING #-} Aeson.ToJSON a => MimeEncode JSON (Maybe a) where
  mimeEncode _ Nothing = S.empty
  mimeEncode _ (Just v) = Sl.toStrict (Aeson.encode v)

-- | Instances which handle potentially missing JSON values
instance {-# OVERLAPPING #-} Aeson.FromJSON a => MimeDecode JSON (Maybe a) where
  mimeDecode _ bs
    | S.null bs = Right Nothing
    | otherwise = fmap Just (Aeson.eitherDecodeStrict bs)
