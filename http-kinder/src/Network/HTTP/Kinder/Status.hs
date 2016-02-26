{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}

-- | Defines the types and kinds for working with type and value level HTTP
-- status codes.
--
-- In particular, this module exports a datatype, 'Status' which is meant
-- to be used with @DataKinds@ providing one type, e.g. ''Ok' for every
-- common HTTP response status (and an override one 'CustomStatus'). It
-- also exports 'Sing' values for each 'Status'-kinded type each merely
-- being the name of that type prepended with @S@.
--
-- Finally, it exports a set of type synonms for each 'Status'-kinded type
-- so that they can be referenced without the quote prefix @'@.
module Network.HTTP.Kinder.Status (

  -- * Functions and types for working with 'HeaderName' 'Sing's
    SomeStatus (SomeStatus)
  , httpStatus
  , statusCode
  , parseStatus

  -- * The 'Status' type/kind
  , Status (..)
  , Sing (
      SCustomStatus
    , SContinue
    , SSwitchingProtocols
    , SOk
    , SCreated
    , SAccepted
    , SNonAuthoritiveInformation
    , SNoContent
    , SResetContent
    , SPartialContent
    , SIMUsed
    , SMultipleChoices
    , SMovedPermanently
    , SFound
    , SSeeOther
    , SNotModified
    , STemporaryRedirect
    , SPermanentRedirect
    , SBadRequest
    , SUnauthorized
    , SPaymentRequired
    , SForbidden
    , SNotFound
    , SMethodNotAllowed
    , SNotAcceptable
    , SProxyAuthenticationRequired
    , SRequestTimeout
    , SConflict
    , SGone
    , SLengthRequired
    , SPreconditionFailed
    , SPayloadTooLarge
    , SRequestURITooLong
    , SUnsupportedMediaType
    , SRequestedRangeNotSatisfiable
    , SExpectationFailed
    , SMisdirectedRequest
    , SUnprocessableEntity
    , SLocked
    , SFailedDependency
    , SUpgradeRequired
    , SPreconditionRequired
    , STooManyRequests
    , SRequestHeaderFieldsTooLarge
    , SUnavailableForLegalReasons
    , SInternalServerError
    , SNotImplemented
    , SBadGateway
    , SServiceUnavailable
    , SGatewayTimeout
    , SHTTPVersionNotSupported
    , SVariantAlsoNegotiates
    , SInsufficientStorage
    , SLoopDetected
    , SNotExtended
    , SNetworkAuthenticationRequired
  )

  -- * Type synonyms for more convenient use of 'Status'es

  , CustomStatus
  , Continue
  , SwitchingProtocols
  , Ok
  , Created
  , Accepted
  , NonAuthoritiveInformation
  , NoContent
  , ResetContent
  , PartialContent
  , IMUsed
  , MultipleChoices
  , MovedPermanently
  , Found
  , SeeOther
  , NotModified
  , TemporaryRedirect
  , PermanentRedirect
  , BadRequest
  , Unauthorized
  , PaymentRequired
  , Forbidden
  , NotFound
  , MethodNotAllowed
  , NotAcceptable
  , ProxyAuthenticationRequired
  , RequestTimeout
  , Conflict
  , Gone
  , LengthRequired
  , PreconditionFailed
  , PayloadTooLarge
  , RequestURITooLong
  , UnsupportedMediaType
  , RequestedRangeNotSatisfiable
  , ExpectationFailed
  , MisdirectedRequest
  , UnprocessableEntity
  , Locked
  , FailedDependency
  , UpgradeRequired
  , PreconditionRequired
  , TooManyRequests
  , RequestHeaderFieldsTooLarge
  , UnavailableForLegalReasons
  , InternalServerError
  , NotImplemented
  , BadGateway
  , ServiceUnavailable
  , GatewayTimeout
  , HTTPVersionNotSupported
  , VariantAlsoNegotiates
  , InsufficientStorage
  , LoopDetected
  , NotExtended
  , NetworkAuthenticationRequired

) where

import           Data.Singletons
import           Data.Singletons.TypeLits
import qualified Network.HTTP.Types.Status as S

-- | It's difficult to get ahold of values of 'Status' directly since they
-- may contain 'Nat' values which do not exist. Instead, we can get almost
-- the same effect through an existential which holds a singleton at the
-- 'Status' kind
--
-- Note that while the Haddocks show this as taking any 'Sing' type it is
-- actually constrained only to have 'Sing's of kind 'Status'.
data SomeStatus where
  SomeStatus :: forall (s :: Status) . Sing s -> SomeStatus

-- | Convert a 'Status' 'Sing'-value into a normal @http-types@ 'S.Status'
httpStatus :: forall (sc :: Status) . Sing sc -> S.Status
httpStatus c =
  case c of
    SCustomStatus int -> S.mkStatus (fromInteger (withKnownNat int (natVal int))) ""

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

-- | Get the 'Int' status code for a given 'Status' 'Sing'.
statusCode :: forall (sc :: Status) . Sing sc -> Int
statusCode = S.statusCode . httpStatus

-- | Given a particular 'Int' status create a 'Status' 'Sing' to match.
-- Attempts to parse to a meaningful 'Status' 'Sing' but defaults to
-- 'CustomStatus' if necessary.
parseStatus :: Int -> SomeStatus
parseStatus c =
  case c of
    100 -> SomeStatus SContinue
    101 -> SomeStatus SSwitchingProtocols

    200 -> SomeStatus SOk
    201 -> SomeStatus SCreated
    202 -> SomeStatus SAccepted
    204 -> SomeStatus SNoContent
    205 -> SomeStatus SResetContent
    206 -> SomeStatus SPartialContent
    226 -> SomeStatus SIMUsed

    300 -> SomeStatus SMultipleChoices
    301 -> SomeStatus SMovedPermanently
    302 -> SomeStatus SFound
    303 -> SomeStatus SSeeOther
    304 -> SomeStatus SNotModified
    307 -> SomeStatus STemporaryRedirect
    308 -> SomeStatus SPermanentRedirect

    400 -> SomeStatus SBadRequest
    401 -> SomeStatus SUnauthorized
    402 -> SomeStatus SPaymentRequired
    403 -> SomeStatus SForbidden
    404 -> SomeStatus SNotFound
    405 -> SomeStatus SMethodNotAllowed
    406 -> SomeStatus SNotAcceptable
    407 -> SomeStatus SProxyAuthenticationRequired
    408 -> SomeStatus SRequestTimeout
    409 -> SomeStatus SConflict
    410 -> SomeStatus SGone
    411 -> SomeStatus SLengthRequired
    412 -> SomeStatus SPreconditionFailed
    413 -> SomeStatus SPayloadTooLarge
    414 -> SomeStatus SRequestURITooLong
    415 -> SomeStatus SUnsupportedMediaType
    416 -> SomeStatus SRequestedRangeNotSatisfiable
    417 -> SomeStatus SExpectationFailed
    421 -> SomeStatus SMisdirectedRequest
    422 -> SomeStatus SUnprocessableEntity
    423 -> SomeStatus SLocked
    424 -> SomeStatus SFailedDependency
    426 -> SomeStatus SUpgradeRequired
    428 -> SomeStatus SPreconditionRequired
    429 -> SomeStatus STooManyRequests
    431 -> SomeStatus SRequestHeaderFieldsTooLarge
    451 -> SomeStatus SUnavailableForLegalReasons

    500 -> SomeStatus SInternalServerError
    501 -> SomeStatus SNotImplemented
    502 -> SomeStatus SBadGateway
    503 -> SomeStatus SServiceUnavailable
    504 -> SomeStatus SGatewayTimeout
    505 -> SomeStatus SHTTPVersionNotSupported
    506 -> SomeStatus SVariantAlsoNegotiates
    507 -> SomeStatus SInsufficientStorage
    508 -> SomeStatus SLoopDetected
    510 -> SomeStatus SNotExtended
    511 -> SomeStatus SNetworkAuthenticationRequired

    other ->
      case toSing (fromIntegral other) :: SomeSing ('KProxy :: KProxy Nat) of
        SomeSing code -> SomeStatus (SCustomStatus code)

-- | A data type representing HTTP statuses (codes). Much more importantly,
-- with @DataKinds@ enabled this becomes a kind describing types, one for
-- each such status.
--
-- It's worth noting that values of this type can be had, but one branch,
-- 'CustomStatus' will not work since it requires 'Nat' values which cannot
-- be had. For this reason prefer using values of 'SomeStatus' instead of
-- values of 'Status' directly.
data Status

  ---- Custom codes

  = CustomStatus Nat
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
type MultipleChoices = 'MultipleChoices
type MovedPermanently = 'MovedPermanently
type Found = 'Found
type SeeOther = 'SeeOther
type NotModified = 'NotModified
type TemporaryRedirect = 'TemporaryRedirect
type PermanentRedirect = 'PermanentRedirect
type BadRequest = 'BadRequest
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

-- ----------------------------------------------------------------------------
-- These could be generated by TH, but we're inlining them! Not only is
-- this faster but it also properly handles the fact that there is no
-- reasonable 'DemoteRep' available
-- ----------------------------------------------------------------------------

data instance Sing (s :: Status)
  = forall i . s ~ CustomStatus i => SCustomStatus (Sing i)
  | s ~ Continue => SContinue
  | s ~ SwitchingProtocols => SSwitchingProtocols
  | s ~ Ok => SOk
  | s ~ Created => SCreated
  | s ~ Accepted => SAccepted
  | s ~ NonAuthoritiveInformation => SNonAuthoritiveInformation
  | s ~ NoContent => SNoContent
  | s ~ ResetContent => SResetContent
  | s ~ PartialContent => SPartialContent
  | s ~ IMUsed => SIMUsed
  | s ~ MultipleChoices => SMultipleChoices
  | s ~ MovedPermanently => SMovedPermanently
  | s ~ Found => SFound
  | s ~ SeeOther => SSeeOther
  | s ~ NotModified => SNotModified
  | s ~ TemporaryRedirect => STemporaryRedirect
  | s ~ PermanentRedirect => SPermanentRedirect
  | s ~ BadRequest => SBadRequest
  | s ~ Unauthorized => SUnauthorized
  | s ~ PaymentRequired => SPaymentRequired
  | s ~ Forbidden => SForbidden
  | s ~ NotFound => SNotFound
  | s ~ MethodNotAllowed => SMethodNotAllowed
  | s ~ NotAcceptable => SNotAcceptable
  | s ~ ProxyAuthenticationRequired => SProxyAuthenticationRequired
  | s ~ RequestTimeout => SRequestTimeout
  | s ~ Conflict => SConflict
  | s ~ Gone => SGone
  | s ~ LengthRequired => SLengthRequired
  | s ~ PreconditionFailed => SPreconditionFailed
  | s ~ PayloadTooLarge => SPayloadTooLarge
  | s ~ RequestURITooLong => SRequestURITooLong
  | s ~ UnsupportedMediaType => SUnsupportedMediaType
  | s ~ RequestedRangeNotSatisfiable => SRequestedRangeNotSatisfiable
  | s ~ ExpectationFailed => SExpectationFailed
  | s ~ MisdirectedRequest => SMisdirectedRequest
  | s ~ UnprocessableEntity => SUnprocessableEntity
  | s ~ Locked => SLocked
  | s ~ FailedDependency => SFailedDependency
  | s ~ UpgradeRequired => SUpgradeRequired
  | s ~ PreconditionRequired => SPreconditionRequired
  | s ~ TooManyRequests => STooManyRequests
  | s ~ RequestHeaderFieldsTooLarge => SRequestHeaderFieldsTooLarge
  | s ~ UnavailableForLegalReasons => SUnavailableForLegalReasons
  | s ~ InternalServerError => SInternalServerError
  | s ~ NotImplemented => SNotImplemented
  | s ~ BadGateway => SBadGateway
  | s ~ ServiceUnavailable => SServiceUnavailable
  | s ~ GatewayTimeout => SGatewayTimeout
  | s ~ HTTPVersionNotSupported => SHTTPVersionNotSupported
  | s ~ VariantAlsoNegotiates => SVariantAlsoNegotiates
  | s ~ InsufficientStorage => SInsufficientStorage
  | s ~ LoopDetected => SLoopDetected
  | s ~ NotExtended => SNotExtended
  | s ~ NetworkAuthenticationRequired => SNetworkAuthenticationRequired

instance SingI i => SingI ('CustomStatus i) where sing = SCustomStatus sing
instance SingI 'Continue where sing = SContinue
instance SingI 'SwitchingProtocols where sing = SSwitchingProtocols
instance SingI 'Ok where sing = SOk
instance SingI 'Created where sing = SCreated
instance SingI 'Accepted where sing = SAccepted
instance SingI 'NonAuthoritiveInformation where sing = SNonAuthoritiveInformation
instance SingI 'NoContent where sing = SNoContent
instance SingI 'ResetContent where sing = SResetContent
instance SingI 'PartialContent where sing = SPartialContent
instance SingI 'IMUsed where sing = SIMUsed
instance SingI 'MultipleChoices where sing = SMultipleChoices
instance SingI 'MovedPermanently where sing = SMovedPermanently
instance SingI 'Found where sing = SFound
instance SingI 'SeeOther where sing = SSeeOther
instance SingI 'NotModified where sing = SNotModified
instance SingI 'TemporaryRedirect where sing = STemporaryRedirect
instance SingI 'PermanentRedirect where sing = SPermanentRedirect
instance SingI 'BadRequest where sing = SBadRequest
instance SingI 'Unauthorized where sing = SUnauthorized
instance SingI 'PaymentRequired where sing = SPaymentRequired
instance SingI 'Forbidden where sing = SForbidden
instance SingI 'NotFound where sing = SNotFound
instance SingI 'MethodNotAllowed where sing = SMethodNotAllowed
instance SingI 'NotAcceptable where sing = SNotAcceptable
instance SingI 'ProxyAuthenticationRequired where sing = SProxyAuthenticationRequired
instance SingI 'RequestTimeout where sing = SRequestTimeout
instance SingI 'Conflict where sing = SConflict
instance SingI 'Gone where sing = SGone
instance SingI 'LengthRequired where sing = SLengthRequired
instance SingI 'PreconditionFailed where sing = SPreconditionFailed
instance SingI 'PayloadTooLarge where sing = SPayloadTooLarge
instance SingI 'RequestURITooLong where sing = SRequestURITooLong
instance SingI 'UnsupportedMediaType where sing = SUnsupportedMediaType
instance SingI 'RequestedRangeNotSatisfiable where sing = SRequestedRangeNotSatisfiable
instance SingI 'ExpectationFailed where sing = SExpectationFailed
instance SingI 'MisdirectedRequest where sing = SMisdirectedRequest
instance SingI 'UnprocessableEntity where sing = SUnprocessableEntity
instance SingI 'Locked where sing = SLocked
instance SingI 'FailedDependency where sing = SFailedDependency
instance SingI 'UpgradeRequired where sing = SUpgradeRequired
instance SingI 'PreconditionRequired where sing = SPreconditionRequired
instance SingI 'TooManyRequests where sing = STooManyRequests
instance SingI 'RequestHeaderFieldsTooLarge where sing = SRequestHeaderFieldsTooLarge
instance SingI 'UnavailableForLegalReasons where sing = SUnavailableForLegalReasons
instance SingI 'InternalServerError where sing = SInternalServerError
instance SingI 'NotImplemented where sing = SNotImplemented
instance SingI 'BadGateway where sing = SBadGateway
instance SingI 'ServiceUnavailable where sing = SServiceUnavailable
instance SingI 'GatewayTimeout where sing = SGatewayTimeout
instance SingI 'HTTPVersionNotSupported where sing = SHTTPVersionNotSupported
instance SingI 'VariantAlsoNegotiates where sing = SVariantAlsoNegotiates
instance SingI 'InsufficientStorage where sing = SInsufficientStorage
instance SingI 'LoopDetected where sing = SLoopDetected
instance SingI 'NotExtended where sing = SNotExtended
instance SingI 'NetworkAuthenticationRequired where sing = SNetworkAuthenticationRequired
