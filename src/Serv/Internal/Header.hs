{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE ExplicitForAll       #-}
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

module Serv.Internal.Header where

import qualified Data.CaseInsensitive as CI
import           Data.Char
import           Data.Singletons
import           Data.Singletons.TH
import           Data.String
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.TypeLits

-- | The variant (name and meaning) of a HTTP header.
--
-- An incomplete listing of most permanant headers, a selection of provisional
-- ones, and one or two non-standard headers. Generally, any header desired can
-- be specified using the 'Name' constructor.
--
-- Used only as their @DataKinds@ types system.
singletons
  [d|
    data HeaderType symbol

      ---- WILDCARD HEADER NAMES

      = CustomHeader symbol
        -- ^ Inject an arbitatry symbol in as a 'HeaderType'. This can be used to
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
  |]

deriving instance Show a => Show (HeaderType a)
deriving instance Read a => Read (HeaderType a)
deriving instance Eq a => Eq (HeaderType a)
deriving instance Ord a => Ord (HeaderType a)
deriving instance Functor HeaderType

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

headerType :: forall s (h :: HeaderType Symbol) . IsString s => Sing h -> HeaderType s
headerType = fmap fromString . fromSing

standardHeaders :: [HeaderType a]
standardHeaders =
  [ CacheControl
  , Connection
  , ContentLength
  , ContentType
  , Date
  , Pragma
  , Upgrade
  , Via
  , Warning
  , Accept
  , AcceptCharset
  , AcceptEncoding
  , AcceptLanguage
  , AccessControlRequestMethod
  , AccessControlRequestHeaders
  , Authorization
  , Cookie
  , Expect
  , From
  , Host
  , IfMatch
  , IfModifiedSince
  , IfNoneMatch
  , IfRange
  , IfUnmodifiedSince
  , MaxForwards
  , Origin
  , ProxyAuthorization
  , Range
  , Referer
  , TE
  , UserAgent
  , XForwardedFor
  , XForwardedHost
  , XForwardedProto
  , XCsrfToken
  , AccessControlAllowOrigin
  , AccessControlExposeHeaders
  , AccessControlMaxAge
  , AccessControlAllowCredentials
  , AccessControlAllowMethods
  , AccessControlAllowHeaders
  , AcceptPatch
  , AcceptRanges
  , Age
  , Allow
  , ContentDisposition
  , ContentEncoding
  , ContentLanguage
  , ContentLocation
  , ContentRange
  , ContentSecurityPolicy
  , ETag
  , Expires
  , LastModified
  , Link
  , Location
  , ProxyAuthenticate
  , PublicKeyPins
  , RetryAfter
  , SetCookie
  , StrictTransportSecurity
  , Trailer
  , TransferEncoding
  , Vary
  , WWWAuthenticate
  ]

headerName :: IsString s => HeaderType Text -> s
headerName h =
  case h of
    CustomHeader name -> fromString (Text.unpack name)
    Accept -> "Accept"
    AcceptCharset -> "Accept-Charset"
    AcceptEncoding -> "Accept-Encoding"
    AcceptLanguage -> "Accept-Language"
    AcceptPatch -> "Accept-Patch"
    AcceptRanges -> "Accept-Ranges"
    AccessControlAllowCredentials -> "Access-Control-Allow-Credentials"
    AccessControlAllowHeaders -> "Access-Control-Allow-Headers"
    AccessControlAllowMethods -> "Access-Control-Allow-Methods"
    AccessControlAllowOrigin -> "Access-Control-Allow-Origin"
    AccessControlExposeHeaders -> "Access-Control-Expose-Headers"
    AccessControlMaxAge -> "Access-Control-Max-Age"
    AccessControlRequestHeaders -> "Access-Control-Request-Headers"
    AccessControlRequestMethod -> "Access-Control-Request-Method"
    Age -> "Age"
    Allow -> "Allow"
    Authorization -> "Authorization"
    CacheControl -> "Cache-Control"
    Connection -> "Connection"
    ContentDisposition -> "Content-Disposition"
    ContentEncoding -> "Content-Encoding"
    ContentLanguage -> "Content-Language"
    ContentLength -> "Content-Length"
    ContentLocation -> "Content-Location"
    ContentRange -> "Content-Range"
    ContentSecurityPolicy -> "Content-Security-Policy"
    ContentType -> "Content-Type"
    Cookie -> "Cookie"
    Date -> "Date"
    ETag -> "ETag"
    Expect -> "Expect"
    Expires -> "Expires"
    From -> "From"
    Host -> "Host"
    IfMatch -> "If-Match"
    IfModifiedSince -> "If-Modified-Since"
    IfNoneMatch -> "If-None-Match"
    IfRange -> "If-Range"
    IfUnmodifiedSince -> "If-Unmodified-Since"
    LastModified -> "Last-Modified"
    Link -> "Link"
    Location -> "Location"
    MaxForwards -> "Max-Forwards"
    Origin -> "Origin"
    Pragma -> "Pragma"
    ProxyAuthenticate -> "Proxy-Authenticate"
    ProxyAuthorization -> "Proxy-Authorization"
    PublicKeyPins -> "Public-Key-Pins"
    Range -> "Range"
    Referer -> "Referer"
    RetryAfter -> "Retry-After"
    SetCookie -> "Set-Cookie"
    StrictTransportSecurity -> "Strict-Transport-Security"
    TE -> "TE"
    Trailer -> "Trailer"
    TransferEncoding -> "Transfer-Encoding"
    Upgrade -> "Upgrade"
    UserAgent -> "User-Agent"
    Vary -> "Vary"
    Via -> "Via"
    WWWAuthenticate -> "WWW-Authenticate"
    Warning -> "Warning"
    XCsrfToken -> "X-Csrf-Token"
    XForwardedFor -> "X-Forwarded-For"
    XForwardedHost -> "X-Forwarded-Host"
    XForwardedProto -> "X-Forwarded-Proto"

nameHeader :: CI.CI Text -> HeaderType Text
nameHeader n =
  case n of
    "Accept" -> Accept
    "Accept-Charset" -> AcceptCharset
    "Accept-Encoding" -> AcceptEncoding
    "Accept-Language" -> AcceptLanguage
    "Accept-Patch" -> AcceptPatch
    "Accept-Ranges" -> AcceptRanges
    "Access-Control-Allow-Credentials" -> AccessControlAllowCredentials
    "Access-Control-Allow-Headers" -> AccessControlAllowHeaders
    "Access-Control-Allow-Methods" -> AccessControlAllowMethods
    "Access-Control-Allow-Origin" -> AccessControlAllowOrigin
    "Access-Control-Expose-Headers" -> AccessControlExposeHeaders
    "Access-Control-Max-Age" -> AccessControlMaxAge
    "Access-Control-Request-Headers" -> AccessControlRequestHeaders
    "Access-Control-Request-Method" -> AccessControlRequestMethod
    "Age" -> Age
    "Allow" -> Allow
    "Authorization" -> Authorization
    "Cache-Control" -> CacheControl
    "Connection" -> Connection
    "Content-Disposition" -> ContentDisposition
    "Content-Encoding" -> ContentEncoding
    "Content-Language" -> ContentLanguage
    "Content-Length" -> ContentLength
    "Content-Location" -> ContentLocation
    "Content-Range" -> ContentRange
    "Content-Security-Policy" -> ContentSecurityPolicy
    "Content-Type" -> ContentType
    "Cookie" -> Cookie
    "Date" -> Date
    "ETag" -> ETag
    "Expect" -> Expect
    "Expires" -> Expires
    "From" -> From
    "Host" -> Host
    "If-Match" -> IfMatch
    "If-Modified-Since" -> IfModifiedSince
    "If-None-Match" -> IfNoneMatch
    "If-Range" -> IfRange
    "If-Unmodified-Since" -> IfUnmodifiedSince
    "Last-Modified" -> LastModified
    "Link" -> Link
    "Location" -> Location
    "Max-Forwards" -> MaxForwards
    "Origin" -> Origin
    "Pragma" -> Pragma
    "Proxy-Authenticate" -> ProxyAuthenticate
    "Proxy-Authorization" -> ProxyAuthorization
    "Public-Key-Pins" -> PublicKeyPins
    "Range" -> Range
    "Referer" -> Referer
    "Retry-After" -> RetryAfter
    "Set-Cookie" -> SetCookie
    "Strict-Transport-Security" -> StrictTransportSecurity
    "TE" -> TE
    "Trailer" -> Trailer
    "Transfer-Encoding" -> TransferEncoding
    "Upgrade" -> Upgrade
    "User-Agent" -> UserAgent
    "Vary" -> Vary
    "Via" -> Via
    "WWW-Authenticate" -> WWWAuthenticate
    "Warning" -> Warning
    "X-Csrf-Token" -> XCsrfToken
    "X-Forwarded-For" -> XForwardedFor
    "X-Forwarded-Host" -> XForwardedHost
    "X-Forwarded-Proto" -> XForwardedProto
    other -> CustomHeader (CI.original other)
