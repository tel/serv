{-# LANGUAGE DataKinds #-}

module Serv.Header (

    HeaderType (..)

  -- Encoding and Decoding Semantics

  , HeaderEncode (..)
  , HeaderDecode (..)
  , headerDecode'

  , HeaderEncodes

  -- Singleton values

  , Sing
    ( SAccept
    , SAcceptCharset
    , SAcceptEncoding
    , SAcceptLanguage
    , SAcceptPatch
    , SAcceptRanges
    , SAccessControlAllowCredentials
    , SAccessControlAllowHeaders
    , SAccessControlAllowMethods
    , SAccessControlAllowOrigin
    , SAccessControlExposeHeaders
    , SAccessControlMaxAge
    , SAccessControlRequestHeaders
    , SAccessControlRequestMethod
    , SAge
    , SAllow
    , SAuthorization
    , SCacheControl
    , SConnection
    , SContentDisposition
    , SContentEncoding
    , SContentLanguage
    , SContentLength
    , SContentLocation
    , SContentRange
    , SContentSecurityPolicy
    , SContentType
    , SCookie
    , SDate
    , SETag
    , SExpect
    , SExpires
    , SFrom
    , SHost
    , SIfMatch
    , SIfModifiedSince
    , SIfNoneMatch
    , SIfRange
    , SIfUnmodifiedSince
    , SLastModified
    , SLink
    , SLocation
    , SMaxForwards
    , SOrigin
    , SPragma
    , SProxyAuthenticate
    , SProxyAuthorization
    , SPublicKeyPins
    , SRange
    , SReferer
    , SRetryAfter
    , SSetCookie
    , SStrictTransportSecurity
    , STE
    , STrailer
    , STransferEncoding
    , SUpgrade
    , SUserAgent
    , SVary
    , SVia
    , SWWWAuthenticate
    , SWarning
    , SXCsrfToken
    , SXForwardedFor
    , SXForwardedHost
    , SXForwardedProto

    )

  -- Type synonyms (so as to avoid unneeded quotes)

  , Accept
  , AcceptCharset
  , AcceptEncoding
  , AcceptLanguage
  , AcceptPatch
  , AcceptRanges
  , AccessControlAllowCredentials
  , AccessControlAllowHeaders
  , AccessControlAllowMethods
  , AccessControlAllowOrigin
  , AccessControlExposeHeaders
  , AccessControlMaxAge
  , AccessControlRequestHeaders
  , AccessControlRequestMethod
  , Age
  , Allow
  , Authorization
  , CacheControl
  , Connection
  , ContentDisposition
  , ContentEncoding
  , ContentLanguage
  , ContentLength
  , ContentLocation
  , ContentRange
  , ContentSecurityPolicy
  , ContentType
  , Cookie
  , Date
  , ETag
  , Expect
  , Expires
  , From
  , Host
  , IfMatch
  , IfModifiedSince
  , IfNoneMatch
  , IfRange
  , IfUnmodifiedSince
  , LastModified
  , Link
  , Location
  , MaxForwards
  , Origin
  , Pragma
  , ProxyAuthenticate
  , ProxyAuthorization
  , PublicKeyPins
  , Range
  , Referer
  , RetryAfter
  , SetCookie
  , StrictTransportSecurity
  , TE
  , Trailer
  , TransferEncoding
  , Upgrade
  , UserAgent
  , Vary
  , Via
  , WWWAuthenticate
  , Warning
  , XCsrfToken
  , XForwardedFor
  , XForwardedHost
  , XForwardedProto

  ) where

import           Data.Singletons                    (Sing)
import           Serv.Internal.Header
import           Serv.Internal.Header.Serialization
