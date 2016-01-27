{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Module defining a set of types corresponding to HTTP status codes
-- along with their singleton values. Only the most common codes are
-- supported by name, although fully custom codes are generally supported.
module Serv.Internal.StatusCode where

import Data.Singletons
import Data.Singletons.TH
import GHC.TypeLits

singletons
  [d|
    data StatusCode code

      ---- Custom codes

      = CustomStatus code
        -- ^ Inject an arbitatry code number as a status code.

      ---- 1xx Informational

      | Continue
      | SwitchingProtocols

      ---- 2xx Success

      | Ok
      | Created
      | Accepted
      | NonAuthoritiveInformation
      | NoContent
      | ResetContent
      | PartialContent
      | IMUsed

      ---- 3xx Redirection

      | MultipleChoices
      | MovedPermanently
      | Found
      | SeeOther
      | NotModified
      | TemporaryRedirect
      | PermanentRedirect

      ---- 4xx Client Error

      | BadRequest
      | Unauthorized
      | PaymentRequired
      | Forbidden
      | NotFound
      | MethodNotAllowed
      | NotAcceptable
      | ProxyAuthenticationRequired
      | RequestTimeout
      | Conflict
      | Gone
      | LengthRequired
      | PreconditionFailed
      | PayloadTooLarge
      | RequestURITooLong
      | UnsupportedMediaType
      | RequestedRangeNotSatisfiable
      | ExpectationFailed
      | MisdirectedRequest
      | UnprocessableEntity
      | Locked
      | FailedDependency
      | UpgradeRequired
      | PreconditionRequired
      | TooManyRequests
      | RequestHeaderFieldsTooLarge
      | UnavailableForLegalReasons

      ---- 5xx Server Error

      | InternalServerError
      | NotImplemented
      | BadGateway
      | ServiceUnavailable
      | GatewayTimeout
      | HTTPVersionNotSupported
      | VariantAlsoNegotiates
      | InsufficientStorage
      | LoopDetected
      | NotExtended
      | NetworkAuthenticationRequired

  |]

deriving instance Show a => Show (StatusCode a)
deriving instance Read a => Read (StatusCode a)
deriving instance Eq a => Eq (StatusCode a)
deriving instance Ord a => Ord (StatusCode a)
deriving instance Functor StatusCode

type CustomStatus code = 'CustomStatus code
type Continue = 'Continue
type SwitchingProtocols = 'SwitchingProtocols
type Ok = 'Ok
type Created = 'Created
type Accepted = 'Accepted
type NonAuthoritiveInformation = 'NonAuthoritiveInformation
type NoContent = 'NoContent
type ResetContent = 'ResetContent
type PartialContent = 'PartialContent
type IMUsed = 'IMUsed
type MovedPermanently = 'MovedPermanently
type Found = 'Found
type SeeOther = 'SeeOther
type NotModified = 'NotModified
type TemporaryRedirect = 'TemporaryRedirect
type PermanentRedirect = 'PermanentRedirect
type Unauthorized = 'Unauthorized
type PaymentRequired = 'PaymentRequired
type Forbidden = 'Forbidden
type NotFound = 'NotFound
type MethodNotAllowed = 'MethodNotAllowed
type NotAcceptable = 'NotAcceptable
type ProxyAuthenticationRequired = 'ProxyAuthenticationRequired
type RequestTimeout = 'RequestTimeout
type Conflict = 'Conflict
type Gone = 'Gone
type LengthRequired = 'LengthRequired
type PreconditionFailed = 'PreconditionFailed
type PayloadTooLarge = 'PayloadTooLarge
type RequestURITooLong = 'RequestURITooLong
type UnsupportedMediaType = 'UnsupportedMediaType
type RequestedRangeNotSatisfiable = 'RequestedRangeNotSatisfiable
type ExpectationFailed = 'ExpectationFailed
type MisdirectedRequest = 'MisdirectedRequest
type UnprocessableEntity = 'UnprocessableEntity
type Locked = 'Locked
type FailedDependency = 'FailedDependency
type UpgradeRequired = 'UpgradeRequired
type PreconditionRequired = 'PreconditionRequired
type TooManyRequests = 'TooManyRequests
type RequestHeaderFieldsTooLarge = 'RequestHeaderFieldsTooLarge
type UnavailableForLegalReasons = 'UnavailableForLegalReasons
type NotImplemented = 'NotImplemented
type BadGateway = 'BadGateway
type ServiceUnavailable = 'ServiceUnavailable
type GatewayTimeout = 'GatewayTimeout
type HTTPVersionNotSupported = 'HTTPVersionNotSupported
type VariantAlsoNegotiates = 'VariantAlsoNegotiates
type InsufficientStorage = 'InsufficientStorage
type LoopDetected = 'LoopDetected
type NotExtended = 'NotExtended
type NetworkAuthenticationRequired = 'NetworkAuthenticationRequired

statusCode :: forall (c :: StatusCode Nat) . Sing c -> StatusCode Integer
statusCode = fromSing

codeNum :: StatusCode Integer -> Integer
codeNum c =
  case c of
    CustomStatus int -> int

    Continue -> 100
    SwitchingProtocols -> 101

    Ok -> 200
    Created -> 201
    Accepted -> 202
    NonAuthoritiveInformation -> 203
    NoContent -> 204
    ResetContent -> 205
    PartialContent -> 206
    IMUsed -> 226

    MultipleChoices -> 300
    MovedPermanently -> 301
    Found -> 302
    SeeOther -> 303
    NotModified -> 304
    TemporaryRedirect -> 307
    PermanentRedirect -> 308

    BadRequest -> 400
    Unauthorized -> 401
    PaymentRequired -> 402
    Forbidden -> 403
    NotFound -> 404
    MethodNotAllowed -> 405
    NotAcceptable -> 406
    ProxyAuthenticationRequired -> 407
    RequestTimeout -> 408
    Conflict -> 409
    Gone -> 410
    LengthRequired -> 411
    PreconditionFailed -> 412
    PayloadTooLarge -> 413
    RequestURITooLong -> 414
    UnsupportedMediaType -> 415
    RequestedRangeNotSatisfiable -> 416
    ExpectationFailed -> 417
    MisdirectedRequest -> 421
    UnprocessableEntity -> 422
    Locked -> 423
    FailedDependency -> 424
    UpgradeRequired -> 426
    PreconditionRequired -> 428
    TooManyRequests -> 429
    RequestHeaderFieldsTooLarge -> 431
    UnavailableForLegalReasons -> 451

    InternalServerError -> 500
    NotImplemented -> 501
    BadGateway -> 502
    ServiceUnavailable -> 503
    GatewayTimeout -> 504
    HTTPVersionNotSupported -> 505
    VariantAlsoNegotiates -> 506
    InsufficientStorage -> 507
    LoopDetected -> 508
    NotExtended -> 510
    NetworkAuthenticationRequired -> 511

