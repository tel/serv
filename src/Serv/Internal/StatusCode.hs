{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE OverloadedStrings    #-}
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

import           Data.Singletons.TH
import qualified Network.HTTP.Types.Status as S

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
type InternalServerError = 'InternalServerError
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

httpStatus :: StatusCode Integer -> S.Status
httpStatus c =
  case c of
    CustomStatus int -> S.mkStatus (fromInteger int) ""

    Continue -> S.status100
    SwitchingProtocols -> S.status101

    Ok -> S.status200
    Created -> S.status201
    Accepted -> S.status202
    NonAuthoritiveInformation -> S.status203
    NoContent -> S.status204
    ResetContent -> S.status205
    PartialContent -> S.status206
    IMUsed -> S.mkStatus 226 "IM Used"

    MultipleChoices -> S.status300
    MovedPermanently -> S.status301
    Found -> S.status302
    SeeOther -> S.status303
    NotModified -> S.status304
    TemporaryRedirect -> S.status307
    PermanentRedirect -> S.status308

    BadRequest -> S.status400
    Unauthorized -> S.status401
    PaymentRequired -> S.status402
    Forbidden -> S.status403
    NotFound -> S.status404
    MethodNotAllowed -> S.status405
    NotAcceptable -> S.status406
    ProxyAuthenticationRequired -> S.status407
    RequestTimeout -> S.status408
    Conflict -> S.status409
    Gone -> S.status410
    LengthRequired -> S.status411
    PreconditionFailed -> S.status412
    PayloadTooLarge -> S.status413
    RequestURITooLong -> S.status414
    UnsupportedMediaType -> S.status415
    RequestedRangeNotSatisfiable -> S.status416
    ExpectationFailed -> S.status417
    MisdirectedRequest -> S.mkStatus 421 "Misdirected Request"
    UnprocessableEntity -> S.mkStatus 422 "Unprocessable Entity"
    Locked -> S.mkStatus 423 "Locked"
    FailedDependency -> S.mkStatus 424 "Failed Dependency"
    UpgradeRequired -> S.mkStatus 426 "Upgrade Required"
    PreconditionRequired -> S.status428
    TooManyRequests -> S.status429
    RequestHeaderFieldsTooLarge -> S.status431
    UnavailableForLegalReasons -> S.mkStatus 451 "Unavailable for Legal Reasons"

    InternalServerError -> S.status500
    NotImplemented -> S.status501
    BadGateway -> S.status502
    ServiceUnavailable -> S.status503
    GatewayTimeout -> S.status504
    HTTPVersionNotSupported -> S.status505
    VariantAlsoNegotiates -> S.mkStatus 506 "Variant Also Negotiates"
    InsufficientStorage -> S.mkStatus 507 "Insufficient Storage"
    LoopDetected -> S.mkStatus 508 "Loop Detected"
    NotExtended -> S.mkStatus 510 "Not Extended"
    NetworkAuthenticationRequired -> S.status511

