{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}

module Serv.Internal.Header.Serialization where

import qualified Data.ByteString             as S
import qualified Data.CaseInsensitive        as CI
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import           Data.Singletons
import           Data.Singletons.Prelude
import           Data.Singletons.TypeRepStar
import           Data.Text                   (Text)
import qualified Data.Text                   as Text
import qualified Data.Text.Encoding          as Text
import           Data.Time
import           GHC.TypeLits
import GHC.Exts
import           Network.HTTP.Media          (MediaType, Quality, parseQuality)
import qualified Network.HTTP.Types          as HTTP
import           Serv.Internal.Header
import           Serv.Internal.Pair
import           Serv.Internal.RawText
import           Serv.Internal.Rec
import           Serv.Internal.Verb

-- | Encode a header type and a corresponding value into a full header pair.
headerPair :: HeaderEncode h v => Sing h -> v -> Maybe HTTP.Header
headerPair sing v = (headerName sing, ) <$> headerEncodeRaw sing v

type family HeaderEncodeAll rs :: Constraint where
  HeaderEncodeAll '[] = ()
  HeaderEncodeAll ( n '::: ty ': rs) = (HeaderEncode n ty, HeaderEncodeAll rs)

-- recDestruct
--   :: forall (rs :: [ Pair (HeaderType Symbol) * ]) r
--   . SingI rs
--   => (forall (h :: HeaderType Symbol) v . Sing h -> v -> r -> r) -> r
--   -> (Rec rs -> r)
-- recDestruct cons nil rec =
--   withSing $ \ (sRs :: Sing rs) ->
--     case sRs of
--       SNil -> nil
--       SCons (hd :: Sing (hdr '::: ty)) tl -> _
  -- case rec of
  --   Nil -> nil
  --   Cons a tl -> withSing (\s -> cons s a (recDestruct cons nil tl))

-- Q: How to realize HeaderEncode dictionary along the entire record
-- Q: How to pattern match on the record to properly capture the inner bits

-- encodeHeaders
--   :: forall (rs :: [ Pair (HeaderType Symbol) * ])
--   . HeaderEncodeAll rs
--   => Rec rs -> [HTTP.Header]
-- encodeHeaders rec =
--   case rec of
--     Nil -> []
--     Cons val rest ->
--       withSing $ \(singHeader ::
--       case headerPair sHead val of
--         Just v -> v : encodeHeaders rest
--         Nothing -> encodeHeaders rest


-- Classes
-- ----------------------------------------------------------------------------

-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Encode n t@ captures a mechanism for treating values of type
-- @t@ as valid data to substantiate the header @n@.
--
-- Note: While this class allows the encoding of any value into a full Unicode
-- Text value, Headers do not generally accept most Unicode code points. Be
-- conservative in implementing this class.
class HeaderEncode (n :: HeaderType Symbol) a where
  headerEncode :: Sing n -> a -> Maybe Text

-- | Handles encoding a header all the way to /raw/ bytes.
headerEncodeRaw :: HeaderEncode n a => Sing n -> a -> Maybe S.ByteString
headerEncodeRaw sing = fmap Text.encodeUtf8 . headerEncode sing


-- | Represents mechanisms to interpret data types as header-compatible values.
--
-- An instance of @Decode n t@ captures a mechanism for reading values of type
-- @t@ from header data stored at header @n@.
class HeaderDecode (n :: HeaderType Symbol) a where
  headerDecode :: Sing n -> Maybe Text -> Either String a

headerDecode' :: HeaderDecode n a => Sing n -> Text -> Either String a
headerDecode' p = headerDecode p . Just

-- | Handles decoding a header all the way from /raw/ bytes.
headerDecodeRaw :: HeaderDecode n a => Sing n -> Maybe S.ByteString -> Either String a
headerDecodeRaw proxy mays =
  case mays of
    Nothing -> headerDecode proxy Nothing
    Just s ->
      case Text.decodeUtf8' s of
        Left err -> Left (show err)
        Right t -> headerDecode' proxy t


-- Instances
-- ----------------------------------------------------------------------------

-- | Output a set of text items as a comma-delimited list OR return nothing
-- if the set is empty
displaySetOpt :: Set Text -> Maybe Text
displaySetOpt s
  | Set.null s = Nothing
  | otherwise = Just (Text.intercalate "," (Set.toList s))

-- | Extend an instance on @'Set' v@ to @[v]@.
uniqueSet :: (Ord v, HeaderEncode n (Set v)) => Sing n -> [v] -> Maybe Text
uniqueSet s = headerEncode s . Set.fromList

trueFalse :: Sing n -> Bool -> Maybe Text
trueFalse _ ok = Just (if ok then "true" else "false")

instance HeaderEncode n RawText where
  headerEncode _ (RawText text) = Just text

instance HeaderEncode 'Allow (Set Verb) where
  headerEncode _ = displaySetOpt . Set.map standardName

instance HeaderEncode 'Allow [Verb] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlExposeHeaders (Set HTTP.HeaderName) where
  headerEncode _ = displaySetOpt . Set.map (Text.decodeUtf8 . CI.original)

instance HeaderEncode 'AccessControlExposeHeaders [HTTP.HeaderName] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlAllowHeaders (Set HTTP.HeaderName) where
  headerEncode _ = displaySetOpt . Set.map (Text.decodeUtf8 . CI.original)

instance HeaderEncode 'AccessControlAllowHeaders [HTTP.HeaderName] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlMaxAge NominalDiffTime where
  headerEncode _ ndt = Just $ Text.pack (show (round ndt :: Int))

instance HeaderEncode 'AccessControlAllowOrigin Text where
  headerEncode _ org = Just org

instance HeaderEncode 'AccessControlAllowMethods (Set Verb) where
  headerEncode _ = displaySetOpt . Set.map standardName

instance HeaderEncode 'AccessControlAllowMethods [Verb] where
  headerEncode = uniqueSet

instance HeaderEncode 'AccessControlAllowCredentials Bool where
  headerEncode = trueFalse

instance {-# OVERLAPPABLE #-} HeaderEncode n Bool where
  headerEncode = trueFalse

instance {-# OVERLAPPABLE #-} HeaderEncode n Int where
  headerEncode _ i = Just (Text.pack (show i))

instance {-# OVERLAPPABLE #-} HeaderEncode h t => HeaderEncode h (Maybe t) where
  headerEncode p v = v >>= headerEncode p



-- | Fail to decode if there is no header. For headers which lack default
-- values. If a header lacks a natural default then avoiding failure should
-- be /explicitly/ requested in the types by wrapping it with a 'Maybe'.
required :: (Text -> Either String a) -> Maybe Text -> Either String a
required _ Nothing = Left "missing header value"
required f (Just t) = f t

-- | For headers with natural notions of default values.
withDefault :: a -> (Text -> Either String a) -> Maybe Text -> Either String a
withDefault def _ Nothing = Right def
withDefault _ f (Just a) = f a

-- | 'RawText' enables capturing the data untouched from the header
instance HeaderDecode n RawText where
  headerDecode _ = required $ \text -> Right (RawText text)

instance HeaderDecode 'Accept [Quality MediaType] where
  headerDecode _ = withDefault [] $ \text ->
    case parseQuality (Text.encodeUtf8 text) of
      Nothing -> Left "could not parse media type specification"
      Just qs -> Right qs

instance {-# OVERLAPPABLE #-} HeaderDecode h t => HeaderDecode h (Maybe t) where
  headerDecode _ Nothing = Right Nothing
  headerDecode p (Just t) = fmap Just (headerDecode' p t)
