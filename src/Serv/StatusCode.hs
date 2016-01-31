{-# LANGUAGE DataKinds #-}

module Serv.StatusCode (

    StatusCode (..)

  -- Singleton values

  , Sing
    ( SCustomStatus
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

    -- Type synonyms (so as to avoid unneeded quotes)

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

import           Data.Singletons                    (Sing)
import           Serv.Internal.StatusCode
