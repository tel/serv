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
import           Data.Singletons.TypeLits
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

httpStatus :: forall (c :: StatusCode Nat) . Sing c -> S.Status
httpStatus c =
  case c of
    SCustomStatus s -> S.mkStatus (fromInteger (withKnownNat s (natVal s))) ""

    SContinue -> S.status100
    SSwitchingProtocols -> S.status101

    SOk -> S.status200
    SCreated -> S.status201
    SAccepted -> S.status202
    SNonAuthoritiveInformation -> S.status203
    SNoContent -> S.status204
    SResetContent -> S.status205
    SPartialContent -> S.status206
    SIMUsed -> S.mkStatus 226 "IM Used"

    SMultipleChoices -> S.status300
    SMovedPermanently -> S.status301
    SFound -> S.status302
    SSeeOther -> S.status303
    SNotModified -> S.status304
    STemporaryRedirect -> S.status307
    SPermanentRedirect -> S.status308

    SBadRequest -> S.status400
    SUnauthorized -> S.status401
    SPaymentRequired -> S.status402
    SForbidden -> S.status403
    SNotFound -> S.status404
    SMethodNotAllowed -> S.status405
    SNotAcceptable -> S.status406
    SProxyAuthenticationRequired -> S.status407
    SRequestTimeout -> S.status408
    SConflict -> S.status409
    SGone -> S.status410
    SLengthRequired -> S.status411
    SPreconditionFailed -> S.status412
    SPayloadTooLarge -> S.status413
    SRequestURITooLong -> S.status414
    SUnsupportedMediaType -> S.status415
    SRequestedRangeNotSatisfiable -> S.status416
    SExpectationFailed -> S.status417
    SMisdirectedRequest -> S.mkStatus 421 "Misdirected Request"
    SUnprocessableEntity -> S.mkStatus 422 "Unprocessable Entity"
    SLocked -> S.mkStatus 423 "Locked"
    SFailedDependency -> S.mkStatus 424 "Failed Dependency"
    SUpgradeRequired -> S.mkStatus 426 "Upgrade Required"
    SPreconditionRequired -> S.status428
    STooManyRequests -> S.status429
    SRequestHeaderFieldsTooLarge -> S.status431
    SUnavailableForLegalReasons -> S.mkStatus 451 "Unavailable for Legal Reasons"

    SInternalServerError -> S.status500
    SNotImplemented -> S.status501
    SBadGateway -> S.status502
    SServiceUnavailable -> S.status503
    SGatewayTimeout -> S.status504
    SHTTPVersionNotSupported -> S.status505
    SVariantAlsoNegotiates -> S.mkStatus 506 "Variant Also Negotiates"
    SInsufficientStorage -> S.mkStatus 507 "Insufficient Storage"
    SLoopDetected -> S.mkStatus 508 "Loop Detected"
    SNotExtended -> S.mkStatus 510 "Not Extended"
    SNetworkAuthenticationRequired -> S.status511

