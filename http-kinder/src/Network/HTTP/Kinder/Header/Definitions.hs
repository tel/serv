{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeInType                #-}

-- | Defines the types and kinds for working with type and value level headers.
--
-- In particular, this module exports a datatype, 'HeaderName' which is
-- meant to be used with @DataKinds@ providing one type, e.g. ''Accept' for
-- every common header name (and an override one 'CustomHeader'). It also
-- exports 'Sing' values for each 'HeaderName'-kinded type each merely
-- being the name of that type prepended with @S@.
--
-- Finally, it exports a set of type synonms for each 'HeaderName'-kinded
-- type so that they can be referenced without the quote prefix @'@.
module Network.HTTP.Kinder.Header.Definitions (

  -- * Functions and types for working with 'HeaderName' 'Sing's
    SomeHeaderName (SomeHeaderName)
  , headerName
  , parseHeaderName

  -- * The 'HeaderName' type/kind
  , HeaderName (..)
  , Sing (
      SCustomHeader
    , SAccept
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

  -- * Type synonyms for more convenient use of 'HeaderName's

  , CustomHeader
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

import qualified Data.CaseInsensitive as CI
import Data.Singletons
import Data.Singletons.TypeLits
import Data.String
import Data.Text (Text)
import qualified Data.Text as T

-- | It's difficult to get ahold of values of 'HeaderName' directly since
-- they may contain 'Symbol' values which do not exist. Instead, we can get
-- almost the same effect through an existential which holds a singleton at
-- the 'HeaderName' kind
--
-- Note that while the Haddocks show this as taking any 'Sing' type it is
-- actually constrained only to have 'Sing's of kind 'HeaderName'.
data SomeHeaderName where
  SomeHeaderName :: forall (h :: HeaderName) . Sing h -> SomeHeaderName

instance Show SomeHeaderName where
  show (SomeHeaderName h) = "SomeHeaderName " ++ headerName h

-- | Equality is slightly strange for 'SomeHeaderName'---we equate, e.g.,
-- @'Accept'@ and @'CustomHeader' "accept"@.
instance Eq SomeHeaderName where
  a == b = proj a == proj b where
    proj :: SomeHeaderName -> CI.CI String
    proj (SomeHeaderName h) = headerName h

instance Ord SomeHeaderName where
  a `compare` b = proj a `compare` proj b where
    proj :: SomeHeaderName -> CI.CI String
    proj (SomeHeaderName h) = headerName h

instance IsString SomeHeaderName where
  fromString = parseHeaderName . fromString

-- | Extract a string-like representation of a 'HeaderName' 'Sing'.
headerName :: forall s (h :: HeaderName) . IsString s => Sing h -> s
headerName h =
  case h of
    SCustomHeader sym              -> withKnownSymbol sym (fromString $ symbolVal sym)
    SAccept                        -> "Accept"
    SAcceptCharset                 -> "Accept-Charset"
    SAcceptEncoding                -> "Accept-Encoding"
    SAcceptLanguage                -> "Accept-Language"
    SAcceptPatch                   -> "Accept-Patch"
    SAcceptRanges                  -> "Accept-Ranges"
    SAccessControlAllowCredentials -> "Access-Control-Allow-Credentials"
    SAccessControlAllowHeaders     -> "Access-Control-Allow-Headers"
    SAccessControlAllowMethods     -> "Access-Control-Allow-Methods"
    SAccessControlAllowOrigin      -> "Access-Control-Allow-Origin"
    SAccessControlExposeHeaders    -> "Access-Control-Expose-Headers"
    SAccessControlMaxAge           -> "Access-Control-Max-Age"
    SAccessControlRequestHeaders   -> "Access-Control-Request-Headers"
    SAccessControlRequestMethod    -> "Access-Control-Request-Method"
    SAge                           -> "Age"
    SAllow                         -> "Allow"
    SAuthorization                 -> "Authorization"
    SCacheControl                  -> "Cache-Control"
    SConnection                    -> "Connection"
    SContentDisposition            -> "Content-Disposition"
    SContentEncoding               -> "Content-Encoding"
    SContentLanguage               -> "Content-Language"
    SContentLength                 -> "Content-Length"
    SContentLocation               -> "Content-Location"
    SContentRange                  -> "Content-Range"
    SContentSecurityPolicy         -> "Content-Security-Policy"
    SContentType                   -> "Content-Type"
    SCookie                        -> "Cookie"
    SDate                          -> "Date"
    SETag                          -> "ETag"
    SExpect                        -> "Expect"
    SExpires                       -> "Expires"
    SFrom                          -> "From"
    SHost                          -> "Host"
    SIfMatch                       -> "If-Match"
    SIfModifiedSince               -> "If-Modified-Since"
    SIfNoneMatch                   -> "If-None-Match"
    SIfRange                       -> "If-Range"
    SIfUnmodifiedSince             -> "If-Unmodified-Since"
    SLastModified                  -> "Last-Modified"
    SLink                          -> "Link"
    SLocation                      -> "Location"
    SMaxForwards                   -> "Max-Forwards"
    SOrigin                        -> "Origin"
    SPragma                        -> "Pragma"
    SProxyAuthenticate             -> "Proxy-Authenticate"
    SProxyAuthorization            -> "Proxy-Authorization"
    SPublicKeyPins                 -> "Public-Key-Pins"
    SRange                         -> "Range"
    SReferer                       -> "Referer"
    SRetryAfter                    -> "Retry-After"
    SSetCookie                     -> "Set-Cookie"
    SStrictTransportSecurity       -> "Strict-Transport-Security"
    STE                            -> "TE"
    STrailer                       -> "Trailer"
    STransferEncoding              -> "Transfer-Encoding"
    SUpgrade                       -> "Upgrade"
    SUserAgent                     -> "User-Agent"
    SVary                          -> "Vary"
    SVia                           -> "Via"
    SWWWAuthenticate               -> "WWW-Authenticate"
    SWarning                       -> "Warning"
    SXCsrfToken                    -> "X-Csrf-Token"
    SXForwardedFor                 -> "X-Forwarded-For"
    SXForwardedHost                -> "X-Forwarded-Host"
    SXForwardedProto               -> "X-Forwarded-Proto"

-- | Construct a 'HeaderType' 'Sing' from a string representation. This
-- will try to use a standard header type if possible or will default to
-- 'CustomHeader'.
parseHeaderName :: CI.CI Text -> SomeHeaderName
parseHeaderName n =
  case n of
    "Accept" -> SomeHeaderName SAccept
    "Accept-Charset" -> SomeHeaderName SAcceptCharset
    "Accept-Encoding" -> SomeHeaderName SAcceptEncoding
    "Accept-Language" -> SomeHeaderName SAcceptLanguage
    "Accept-Patch" -> SomeHeaderName SAcceptPatch
    "Accept-Ranges" -> SomeHeaderName SAcceptRanges
    "Access-Control-Allow-Credentials" -> SomeHeaderName SAccessControlAllowCredentials
    "Access-Control-Allow-Headers" -> SomeHeaderName SAccessControlAllowHeaders
    "Access-Control-Allow-Methods" -> SomeHeaderName SAccessControlAllowMethods
    "Access-Control-Allow-Origin" -> SomeHeaderName SAccessControlAllowOrigin
    "Access-Control-Expose-Headers" -> SomeHeaderName SAccessControlExposeHeaders
    "Access-Control-Max-Age" -> SomeHeaderName SAccessControlMaxAge
    "Access-Control-Request-Headers" -> SomeHeaderName SAccessControlRequestHeaders
    "Access-Control-Request-Method" -> SomeHeaderName SAccessControlRequestMethod
    "Age" -> SomeHeaderName SAge
    "Allow" -> SomeHeaderName SAllow
    "Authorization" -> SomeHeaderName SAuthorization
    "Cache-Control" -> SomeHeaderName SCacheControl
    "Connection" -> SomeHeaderName SConnection
    "Content-Disposition" -> SomeHeaderName SContentDisposition
    "Content-Encoding" -> SomeHeaderName SContentEncoding
    "Content-Language" -> SomeHeaderName SContentLanguage
    "Content-Length" -> SomeHeaderName SContentLength
    "Content-Location" -> SomeHeaderName SContentLocation
    "Content-Range" -> SomeHeaderName SContentRange
    "Content-Security-Policy" -> SomeHeaderName SContentSecurityPolicy
    "Content-Type" -> SomeHeaderName SContentType
    "Cookie" -> SomeHeaderName SCookie
    "Date" -> SomeHeaderName SDate
    "ETag" -> SomeHeaderName SETag
    "Expect" -> SomeHeaderName SExpect
    "Expires" -> SomeHeaderName SExpires
    "From" -> SomeHeaderName SFrom
    "Host" -> SomeHeaderName SHost
    "If-Match" -> SomeHeaderName SIfMatch
    "If-Modified-Since" -> SomeHeaderName SIfModifiedSince
    "If-None-Match" -> SomeHeaderName SIfNoneMatch
    "If-Range" -> SomeHeaderName SIfRange
    "If-Unmodified-Since" -> SomeHeaderName SIfUnmodifiedSince
    "Last-Modified" -> SomeHeaderName SLastModified
    "Link" -> SomeHeaderName SLink
    "Location" -> SomeHeaderName SLocation
    "Max-Forwards" -> SomeHeaderName SMaxForwards
    "Origin" -> SomeHeaderName SOrigin
    "Pragma" -> SomeHeaderName SPragma
    "Proxy-Authenticate" -> SomeHeaderName SProxyAuthenticate
    "Proxy-Authorization" -> SomeHeaderName SProxyAuthorization
    "Public-Key-Pins" -> SomeHeaderName SPublicKeyPins
    "Range" -> SomeHeaderName SRange
    "Referer" -> SomeHeaderName SReferer
    "Retry-After" -> SomeHeaderName SRetryAfter
    "Set-Cookie" -> SomeHeaderName SSetCookie
    "Strict-Transport-Security" -> SomeHeaderName SStrictTransportSecurity
    "TE" -> SomeHeaderName STE
    "Trailer" -> SomeHeaderName STrailer
    "Transfer-Encoding" -> SomeHeaderName STransferEncoding
    "Upgrade" -> SomeHeaderName SUpgrade
    "User-Agent" -> SomeHeaderName SUserAgent
    "Vary" -> SomeHeaderName SVary
    "Via" -> SomeHeaderName SVia
    "WWW-Authenticate" -> SomeHeaderName SWWWAuthenticate
    "Warning" -> SomeHeaderName SWarning
    "X-Csrf-Token" -> SomeHeaderName SXCsrfToken
    "X-Forwarded-For" -> SomeHeaderName SXForwardedFor
    "X-Forwarded-Host" -> SomeHeaderName SXForwardedHost
    "X-Forwarded-Proto" -> SomeHeaderName SXForwardedProto
    other ->
      case toSing @Symbol (CI.original other) of
        SomeSing s -> SomeHeaderName (SCustomHeader s)


-- | A data type representing names describing headers in an HTTP request
-- or response. Much more importantly, with @DataKinds@ enabled this
-- becomes a kind describing types, one for each such header name.
--
-- It's worth noting that values of this type can be had, but one branch,
-- 'CustomHeader' will not work since it requires 'Symbol' values which
-- cannot be had. For this reason prefer using values of 'SomeHeaderName'
-- instead of values of 'HeaderName' directly.
data HeaderName

  ---- WILDCARD HEADER NAMES

  = CustomHeader Symbol
    -- ^ Inject an arbitatry symbol in as a 'HeaderName'. This can be used to
    -- implement any sort of custom header



  ---- COMMON HEADER NAMES


  | CacheControl
    -- ^ Used to specify directives that must be obeyed by all caching mechanisms
    -- along the request-response chain. Tells all caching mechanisms from server
    -- to client whether they may cache this object. It is measured in seconds
    --
    --     Cache-Control: no-cache

  | Connection
    -- ^ Control options for the current connection and list of hop-by-hop
    -- request fields
    --
    --     Connection: keep-alive
    --     Connection: Upgrade

  | ContentLength
    -- ^ The length of the request body in octets (8-bit bytes)
    --
    --     Content-Length: 348

  | ContentType
    -- ^ The MIME type of the body of the request (used with POST and PUT requests)
    --
    --     Content-Type: application/x-www-form-urlencoded

  | Date
    -- ^ The date and time that the message was sent (in "HTTP-date" format as
    -- defined by RFC 7231 Date/Time Formats)
    --
    --     Date: Tue, 15 Nov 1994 08:12:31 GMT

  | Pragma
    -- ^ Implementation-specific fields that may have various effects anywhere
    -- along the request-response chain.
    --
    --     Pragma: no-cache

  | Upgrade
    -- ^ Ask the server to upgrade to another protocol.
    --
    --     Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11

  | Via
    -- ^ Informs the server of proxies through which the request was sent.
    --
    --     Via: 1.0 fred, 1.1 example.com (Apache/1.1)

  | Warning
    -- ^ A general warning about possible problems with the entity body.
    --
    --     Warning: 199 Miscellaneous warning



    ---- REQUEST-SPECIFIC HEADER NAMES


  | Accept
    -- ^ Content-Types that are acceptable for the response.
    --
    --     Accept: text/plain

  | AcceptCharset
    -- ^ Character sets that are acceptable
    --
    --     Accept-Charset: utf-8

  | AcceptEncoding
    -- ^ List of acceptable encodings.
    --
    --     Accept-Encoding: gzip, deflate

  | AcceptLanguage
    -- ^ List of acceptable human languages for response.
    --
    --     Accept-Language: en-US

  | AccessControlRequestMethod
    -- ^ Used when issuing a preflight request to let the server
    -- know what HTTP method will be used when the actual request is made.
    --
    --     Access-Control-Request-Method: POST

  | AccessControlRequestHeaders
    -- ^ Used when issuing a preflight request to let the server know
    -- what HTTP headers will be used when the actual request is made.
    --
    --     Access-Control-Request-Headers: X-PINGOTHER

  | Authorization
    -- ^ Authentication credentials for HTTP authentication
    --
    --     Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==

  | Cookie
    -- ^ An HTTP cookie previously sent by the server with Set-Cookie (below)
    --
    --     Cookie: $Version=1; Skin=new;

  | Expect
    -- ^ Indicates that particular server behaviors are required by the client
    --
    --     Expect: 100-continue

  | From
    -- ^ The email address of the user making the request
    --
    --     From: user@example.com

  | Host
    -- ^ The domain name of the server (for virtual hosting), and the TCP port
    -- number on which the server is listening. The port number may be omitted
    -- if the port is the standard port for the service requested.
    --
    --     Host: en.wikipedia.org:80
    --     Host: en.wikipedia.org

  | IfMatch
    -- ^ Only perform the action if the client supplied entity matches the same
    -- entity on the server. This is mainly for methods like PUT to only update
    -- a resource if it has not been modified since the user last updated it.
    --
    --     If-Match: "737060cd8c284d8af7ad3082f209582d"

  | IfModifiedSince
    -- ^ Allows a 304 Not Modified to be returned if content is unchanged
    --
    --     If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT

  | IfNoneMatch
    -- ^ Allows a 304 Not Modified to be returned if content is unchanged,
    -- see HTTP ETag
    --
    --     If-None-Match: "737060cd8c284d8af7ad3082f209582d"

  | IfRange
    -- ^ If the entity is unchanged, send me the part(s) that I am missing;
    -- otherwise, send me the entire new entity
    --
    --     If-Range: "737060cd8c284d8af7ad3082f209582d"

  | IfUnmodifiedSince
    -- ^ Only send the response if the entity has not been modified since a
    -- specific time.
    --
    --     If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT

  | MaxForwards
    -- ^ Limit the number of times the message can be forwarded through proxies
    -- or gateways.
    --
    --     Max-Forwards: 10

  | Origin
    -- ^ Initiates a request for cross-origin resource sharing (asks server for
    -- an 'Access-Control-Allow-Origin' response field).
    --
    -- Origin: http://www.example-social-network.com

  | ProxyAuthorization
    -- ^ Authorization credentials for connecting to a proxy.
    --
    --     Proxy-Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==

  | Range
    -- ^ Request only part of an entity. Bytes are numbered from 0.
    --
    --     Range: bytes=500-999

  | Referer
    -- ^ This is the address of the previous web page from which a link to the
    -- currently requested page was followed. (The word “referrer” has been
    -- misspelled in the RFC as well as in most implementations to the point
    -- that it has become standard usage and is considered correct terminology)
    --
    --     Referer: http://en.wikipedia.org/wiki/Main_Page

  | TE
    -- ^ The transfer encodings the user agent is willing to accept: the same
    -- values as for the response header field Transfer-Encoding can be used,
    -- plus the "trailers" value (related to the "chunked" transfer method) to
    -- notify the server it expects to receive additional fields in the trailer
    -- after the last, zero-sized, chunk.
    --
    --     TE: trailers, deflate

  | UserAgent
    -- ^ The user agent string of the user agent
    --
    --     User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/21.0

  | XForwardedFor
    -- ^ A de facto standard for identifying the originating IP address of a
    -- client connecting to a web server through an HTTP proxy or load balancer
    --
    --     X-Forwarded-For: client1, proxy1, proxy2
    --     X-Forwarded-For: 129.78.138.66, 129.78.64.103

  | XForwardedHost
    -- ^ A de facto standard for identifying the original host requested by the client in
    -- the Host HTTP request header, since the host name and/or port of the reverse
    -- proxy (load balancer) may differ from the origin server handling the request.
    --
    --     X-Forwarded-Host: en.wikipedia.org:80
    --     X-Forwarded-Host: en.wikipedia.org

  | XForwardedProto
    -- ^ A de facto standard for identifying the originating protocol of an HTTP request,
    -- since a reverse proxy (or a load balancer) may communicate with a web server using
    -- HTTP even if the request to the reverse proxy is HTTPS.

  | XCsrfToken
    -- ^ Used to prevent cross-site request forgery.
    --
    --     X-Csrf-Token: i8XNjC4b8KVok4uw5RftR38Wgp2BFwql




    ---- RESPONSE-SPECIFIC HEADER NAMES


  | AccessControlAllowOrigin
    -- ^ When responding to a CORS request this lists either
    -- an origin or @*@ describing the server's CORS policy for
    -- the requested resource
    --
    --     Access-Control-Allow-Origin: *
    --     Access-Control-Allow-Origin: http://foo.example
    --     Access-Control-Allow-Origin: https://foo.example

  | AccessControlExposeHeaders
    -- ^ This header lets a server whitelist headers that browsers are allowed to access.
    --
    --     Access-Control-Expose-Headers: X-My-Custom-Header, X-Another-Custom-Header

  | AccessControlMaxAge
    -- ^ This header indicates how long the results of a preflight request can be cached.
    --
    --     Access-Control-Max-Age: <delta-seconds>

  | AccessControlAllowCredentials
    -- ^ Indicates whether or not the response to the request can be exposed when the
    -- credentials flag is true. When used as part of a response to a preflight request,
    -- this indicates whether or not the actual request can be made using credentials. Note
    -- that simple GET requests are not preflighted, and so if a request is made for a
    -- resource with credentials, if this header is not returned with the resource, the
    -- response is ignored by the browser and not returned to web content.
    --
    --     Access-Control-Allow-Credentials: true | false

  | AccessControlAllowMethods
    -- ^ Specifies the method or methods allowed when accessing the resource. This is
    -- used in response to a preflight request.
    --
    --     Access-Control-Allow-Methods: <method>[, <method>]*

  | AccessControlAllowHeaders
    -- ^ Used in response to a preflight request to indicate which HTTP headers can
    -- be used when making the actual request.

  | AcceptPatch
    -- ^ Specifies which patch document formats this server supports
    --
    --     Accept-Patch: text/example;charset=utf-8

  | AcceptRanges
    -- ^ What partial content range types this server supports via byte serving
    --
    --     Accept-Ranges: bytes

  | Age
    -- ^ The age the object has been in a proxy cache in seconds
    --
    --     Age: 12

  | Allow
    -- ^ Valid actions for a specified resource. To be used for a 405 Method not allowed
    --
    --     Allow: GET, HEAD

  | ContentDisposition
    -- ^ An opportunity to raise a "File Download" dialogue box for a known MIME type
    -- with binary format or suggest a filename for dynamic content. Quotes are
    -- necessary with special characters.
    --
    --     Content-Disposition: attachment; filename="fname.ext"

  | ContentEncoding
    -- ^ The type of encoding used on the data.
    --
    --     Content-Encoding: gzip

  | ContentLanguage
    -- ^ The natural language or languages of the intended audience for the enclosed content
    --
    --     Content-Language: da

  | ContentLocation
    -- ^ An alternate location for the returned data
    --
    --     Content-Location: /index.htm

  | ContentRange
    -- ^ Where in a full body message this partial message belongs
    --
    --     Content-Range: bytes 21010-47021/47022

  | ContentSecurityPolicy
    -- ^ Content Security Policy definition.
    --
    --     Content-Security-Policy: default-src 'self'

  | ETag
    -- ^ An identifier for a specific version of a resource, often a message digest
    --
    --     ETag: "737060cd8c284d8af7ad3082f209582d"

  | Expires
    -- ^ Gives the date/time after which the response is considered stale (in
    -- "HTTP-date" format as defined by RFC 7231)
    --
    --     Expires: Thu, 01 Dec 1994 16:00:00 GMT

  | LastModified
    -- ^ The last modified date for the requested object (in "HTTP-date" format as
    -- defined by RFC 7231)
    --
    --     Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT

  | Link
    -- ^ Used to express a typed relationship with another resource, where the
    -- relation type is defined by RFC 5988
    --
    --     Link: </feed>; rel="alternate"

  | Location
    -- ^ Used in redirection, or when a new resource has been created.
    --
    --     Location: http://www.w3.org/pub/WWW/People.html

  | ProxyAuthenticate
    -- ^ Request authentication to access the proxy.
    --
    --     Proxy-Authenticate: Basic

  | PublicKeyPins
    -- ^ HTTP Public Key Pinning, announces hash of website's authentic TLS certificate
    --
    --     Public-Key-Pins: max-age=2592000; pin-sha256="E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=";

  | RetryAfter
    -- ^ If an entity is temporarily unavailable, this instructs the client to
    -- try again later. Value could be a specified period of time (in seconds) or a HTTP-date.
    --
    --     Retry-After: 120
    --     Retry-After: Fri, 07 Nov 2014 23:59:59 GMT

  | SetCookie
    -- ^ An HTTP cookie
    --
    --     Set-Cookie: UserID=JohnDoe; Max-Age=3600; Version=1

  | StrictTransportSecurity
    -- ^ A HSTS Policy informing the HTTP client how long to cache the HTTPS only
    -- policy and whether this applies to subdomains.
    --
    --     Strict-Transport-Security: max-age=16070400; includeSubDomains

  | Trailer
    -- ^ The Trailer general field value indicates that the given set of header fields
    -- is present in the trailer of a message encoded with chunked transfer coding.
    --
    --     Trailer: Max-Forwards

  | TransferEncoding
    -- ^ The form of encoding used to safely transfer the entity to the user. Currently defined
    -- methods are: chunked, compress, deflate, gzip, identity.
    --
    --     Transfer-Encoding: chunked

  | Vary
    -- ^ Tells downstream proxies how to match future request headers to decide whether
    -- the cached response can be used rather than requesting a fresh one from the origin server.
    --
    --     Vary: *
    --     Vary: Accept-Language

  | WWWAuthenticate
    -- ^ Indicates the authentication scheme that should be used to access the requested entity.
    --
    --     WWW-Authenticate: Basic

type CustomHeader s = 'CustomHeader s
type Accept = 'Accept
type AcceptCharset = 'AcceptCharset
type AcceptEncoding = 'AcceptEncoding
type AcceptLanguage = 'AcceptLanguage
type AcceptPatch = 'AcceptPatch
type AcceptRanges = 'AcceptRanges
type AccessControlAllowCredentials = 'AccessControlAllowCredentials
type AccessControlAllowHeaders = 'AccessControlAllowHeaders
type AccessControlAllowMethods = 'AccessControlAllowMethods
type AccessControlAllowOrigin = 'AccessControlAllowOrigin
type AccessControlExposeHeaders = 'AccessControlExposeHeaders
type AccessControlMaxAge = 'AccessControlMaxAge
type AccessControlRequestHeaders = 'AccessControlRequestHeaders
type AccessControlRequestMethod = 'AccessControlRequestMethod
type Age = 'Age
type Allow = 'Allow
type Authorization = 'Authorization
type CacheControl = 'CacheControl
type Connection = 'Connection
type ContentDisposition = 'ContentDisposition
type ContentEncoding = 'ContentEncoding
type ContentLanguage = 'ContentLanguage
type ContentLength = 'ContentLength
type ContentLocation = 'ContentLocation
type ContentRange = 'ContentRange
type ContentSecurityPolicy = 'ContentSecurityPolicy
type ContentType = 'ContentType
type Cookie = 'Cookie
type Date = 'Date
type ETag = 'ETag
type Expect = 'Expect
type Expires = 'Expires
type From = 'From
type Host = 'Host
type IfMatch = 'IfMatch
type IfModifiedSince = 'IfModifiedSince
type IfNoneMatch = 'IfNoneMatch
type IfRange = 'IfRange
type IfUnmodifiedSince = 'IfUnmodifiedSince
type LastModified = 'LastModified
type Link = 'Link
type Location = 'Location
type MaxForwards = 'MaxForwards
type Origin = 'Origin
type Pragma = 'Pragma
type ProxyAuthenticate = 'ProxyAuthenticate
type ProxyAuthorization = 'ProxyAuthorization
type PublicKeyPins = 'PublicKeyPins
type Range = 'Range
type Referer = 'Referer
type RetryAfter = 'RetryAfter
type SetCookie = 'SetCookie
type StrictTransportSecurity = 'StrictTransportSecurity
type TE = 'TE
type Trailer = 'Trailer
type TransferEncoding = 'TransferEncoding
type Upgrade = 'Upgrade
type UserAgent = 'UserAgent
type Vary = 'Vary
type Via = 'Via
type WWWAuthenticate = 'WWWAuthenticate
type Warning = 'Warning
type XCsrfToken = 'XCsrfToken
type XForwardedFor = 'XForwardedFor
type XForwardedHost = 'XForwardedHost
type XForwardedProto = 'XForwardedProto

-- ----------------------------------------------------------------------------
-- These could be generated by TH, but we're inlining them! Not only is
-- this faster but it also properly handles the fact that there is no
-- reasonable 'DemoteRep' available
-- ----------------------------------------------------------------------------

data instance Sing (h :: HeaderName)
  = forall s . h ~ CustomHeader s => SCustomHeader (Sing s)
  | h ~ Accept => SAccept
  | h ~ AcceptCharset => SAcceptCharset
  | h ~ AcceptEncoding => SAcceptEncoding
  | h ~ AcceptLanguage => SAcceptLanguage
  | h ~ AcceptPatch => SAcceptPatch
  | h ~ AcceptRanges => SAcceptRanges
  | h ~ AccessControlAllowCredentials => SAccessControlAllowCredentials
  | h ~ AccessControlAllowHeaders => SAccessControlAllowHeaders
  | h ~ AccessControlAllowMethods => SAccessControlAllowMethods
  | h ~ AccessControlAllowOrigin => SAccessControlAllowOrigin
  | h ~ AccessControlExposeHeaders => SAccessControlExposeHeaders
  | h ~ AccessControlMaxAge => SAccessControlMaxAge
  | h ~ AccessControlRequestHeaders => SAccessControlRequestHeaders
  | h ~ AccessControlRequestMethod => SAccessControlRequestMethod
  | h ~ Age => SAge
  | h ~ Allow => SAllow
  | h ~ Authorization => SAuthorization
  | h ~ CacheControl => SCacheControl
  | h ~ Connection => SConnection
  | h ~ ContentDisposition => SContentDisposition
  | h ~ ContentEncoding => SContentEncoding
  | h ~ ContentLanguage => SContentLanguage
  | h ~ ContentLength => SContentLength
  | h ~ ContentLocation => SContentLocation
  | h ~ ContentRange => SContentRange
  | h ~ ContentSecurityPolicy => SContentSecurityPolicy
  | h ~ ContentType => SContentType
  | h ~ Cookie => SCookie
  | h ~ Date => SDate
  | h ~ ETag => SETag
  | h ~ Expect => SExpect
  | h ~ Expires => SExpires
  | h ~ From => SFrom
  | h ~ Host => SHost
  | h ~ IfMatch => SIfMatch
  | h ~ IfModifiedSince => SIfModifiedSince
  | h ~ IfNoneMatch => SIfNoneMatch
  | h ~ IfRange => SIfRange
  | h ~ IfUnmodifiedSince => SIfUnmodifiedSince
  | h ~ LastModified => SLastModified
  | h ~ Link => SLink
  | h ~ Location => SLocation
  | h ~ MaxForwards => SMaxForwards
  | h ~ Origin => SOrigin
  | h ~ Pragma => SPragma
  | h ~ ProxyAuthenticate => SProxyAuthenticate
  | h ~ ProxyAuthorization => SProxyAuthorization
  | h ~ PublicKeyPins => SPublicKeyPins
  | h ~ Range => SRange
  | h ~ Referer => SReferer
  | h ~ RetryAfter => SRetryAfter
  | h ~ SetCookie => SSetCookie
  | h ~ StrictTransportSecurity => SStrictTransportSecurity
  | h ~ TE => STE
  | h ~ Trailer => STrailer
  | h ~ TransferEncoding => STransferEncoding
  | h ~ Upgrade => SUpgrade
  | h ~ UserAgent => SUserAgent
  | h ~ Vary => SVary
  | h ~ Via => SVia
  | h ~ WWWAuthenticate => SWWWAuthenticate
  | h ~ Warning => SWarning
  | h ~ XCsrfToken => SXCsrfToken
  | h ~ XForwardedFor => SXForwardedFor
  | h ~ XForwardedHost => SXForwardedHost
  | h ~ XForwardedProto => SXForwardedProto

instance SingI s => SingI ('CustomHeader s) where sing = SCustomHeader sing
instance SingI 'Accept where sing = SAccept
instance SingI 'AcceptCharset where sing = SAcceptCharset
instance SingI 'AcceptEncoding where sing = SAcceptEncoding
instance SingI 'AcceptLanguage where sing = SAcceptLanguage
instance SingI 'AcceptPatch where sing = SAcceptPatch
instance SingI 'AcceptRanges where sing = SAcceptRanges
instance SingI 'AccessControlAllowCredentials where sing = SAccessControlAllowCredentials
instance SingI 'AccessControlAllowHeaders where sing = SAccessControlAllowHeaders
instance SingI 'AccessControlAllowMethods where sing = SAccessControlAllowMethods
instance SingI 'AccessControlAllowOrigin where sing = SAccessControlAllowOrigin
instance SingI 'AccessControlExposeHeaders where sing = SAccessControlExposeHeaders
instance SingI 'AccessControlMaxAge where sing = SAccessControlMaxAge
instance SingI 'AccessControlRequestHeaders where sing = SAccessControlRequestHeaders
instance SingI 'AccessControlRequestMethod where sing = SAccessControlRequestMethod
instance SingI 'Age where sing = SAge
instance SingI 'Allow where sing = SAllow
instance SingI 'Authorization where sing = SAuthorization
instance SingI 'CacheControl where sing = SCacheControl
instance SingI 'Connection where sing = SConnection
instance SingI 'ContentDisposition where sing = SContentDisposition
instance SingI 'ContentEncoding where sing = SContentEncoding
instance SingI 'ContentLanguage where sing = SContentLanguage
instance SingI 'ContentLength where sing = SContentLength
instance SingI 'ContentLocation where sing = SContentLocation
instance SingI 'ContentRange where sing = SContentRange
instance SingI 'ContentSecurityPolicy where sing = SContentSecurityPolicy
instance SingI 'ContentType where sing = SContentType
instance SingI 'Cookie where sing = SCookie
instance SingI 'Date where sing = SDate
instance SingI 'ETag where sing = SETag
instance SingI 'Expect where sing = SExpect
instance SingI 'Expires where sing = SExpires
instance SingI 'From where sing = SFrom
instance SingI 'Host where sing = SHost
instance SingI 'IfMatch where sing = SIfMatch
instance SingI 'IfModifiedSince where sing = SIfModifiedSince
instance SingI 'IfNoneMatch where sing = SIfNoneMatch
instance SingI 'IfRange where sing = SIfRange
instance SingI 'IfUnmodifiedSince where sing = SIfUnmodifiedSince
instance SingI 'LastModified where sing = SLastModified
instance SingI 'Link where sing = SLink
instance SingI 'Location where sing = SLocation
instance SingI 'MaxForwards where sing = SMaxForwards
instance SingI 'Origin where sing = SOrigin
instance SingI 'Pragma where sing = SPragma
instance SingI 'ProxyAuthenticate where sing = SProxyAuthenticate
instance SingI 'ProxyAuthorization where sing = SProxyAuthorization
instance SingI 'PublicKeyPins where sing = SPublicKeyPins
instance SingI 'Range where sing = SRange
instance SingI 'Referer where sing = SReferer
instance SingI 'RetryAfter where sing = SRetryAfter
instance SingI 'SetCookie where sing = SSetCookie
instance SingI 'StrictTransportSecurity where sing = SStrictTransportSecurity
instance SingI 'TE where sing = STE
instance SingI 'Trailer where sing = STrailer
instance SingI 'TransferEncoding where sing = STransferEncoding
instance SingI 'Upgrade where sing = SUpgrade
instance SingI 'UserAgent where sing = SUserAgent
instance SingI 'Vary where sing = SVary
instance SingI 'Via where sing = SVia
instance SingI 'WWWAuthenticate where sing = SWWWAuthenticate
instance SingI 'Warning where sing = SWarning
instance SingI 'XCsrfToken where sing = SXCsrfToken
instance SingI 'XForwardedFor where sing = SXForwardedFor
instance SingI 'XForwardedHost where sing = SXForwardedHost
instance SingI 'XForwardedProto where sing = SXForwardedProto
