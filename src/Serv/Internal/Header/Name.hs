{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Serv.Internal.Header.Name where

import           Data.Proxy
import           Data.String
import           GHC.TypeLits
import qualified Network.HTTP.Types as HTTP

-- This is a GHC 8 feature. It won't work today because we
-- cannot promote a GADT the way that we'd like to. Otherwise,
-- we'd let each name specify its "direction".
--
-- For instance, we'd have
--
--     data HeaderName (d :: Direction) where
--       Name :: Symbol -> HeaderName d
--       CacheControl :: HeaderName d
--       Accept :: HeaderName Request
--       Allow :: HeaderName Response
--
-- etc.
--
--     -- | Qualifier of header types.
--     data Direction = Request | Response

-- | The variant (name and meaning) of a HTTP header.
--
-- An incomplete listing of most permanant headers, a selection of provisional
-- ones, and one or two non-standard headers. Generally, any header desired can
-- be specified using the 'Name' constructor.
--
-- Used only as their @DataKinds@ types system.
data HeaderName

  ---- WILDCARD HEADER NAMES

  = HeaderName Symbol
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

-- | Implements reflection of 'Name' types into 'HTTP.HeaderName' values for runtime use.
class ReflectName (n :: HeaderName) where
  reflectName :: proxy n -> HTTP.HeaderName


-- Proxies
-- ----------------------------------------------------------------------------

cacheControl :: Proxy 'CacheControl
cacheControl = Proxy

connection :: Proxy 'Connection
connection = Proxy

contentLength :: Proxy 'ContentLength
contentLength = Proxy

contentType :: Proxy 'ContentType
contentType = Proxy

date :: Proxy 'Date
date = Proxy

pragma :: Proxy 'Pragma
pragma = Proxy

upgrade :: Proxy 'Upgrade
upgrade = Proxy

via :: Proxy 'Via
via = Proxy

warning :: Proxy 'Warning
warning = Proxy

accept :: Proxy 'Accept
accept = Proxy

acceptCharset :: Proxy 'AcceptCharset
acceptCharset = Proxy

acceptEncoding :: Proxy 'AcceptEncoding
acceptEncoding = Proxy

acceptLanguage :: Proxy 'AcceptLanguage
acceptLanguage = Proxy

accessControlRequestMethod :: Proxy 'AccessControlRequestMethod
accessControlRequestMethod = Proxy

accessControlRequestHeaders :: Proxy 'AccessControlRequestHeaders
accessControlRequestHeaders = Proxy

authorization :: Proxy 'Authorization
authorization = Proxy

cookie :: Proxy 'Cookie
cookie = Proxy

expect :: Proxy 'Expect
expect = Proxy

from :: Proxy 'From
from = Proxy

host :: Proxy 'Host
host = Proxy

ifMatch :: Proxy 'IfMatch
ifMatch = Proxy

ifModifiedSince :: Proxy 'IfModifiedSince
ifModifiedSince = Proxy

ifNoneMatch :: Proxy 'IfNoneMatch
ifNoneMatch = Proxy

ifRange :: Proxy 'IfRange
ifRange = Proxy

ifUnmodifiedSince :: Proxy 'IfUnmodifiedSince
ifUnmodifiedSince = Proxy

maxForwards :: Proxy 'MaxForwards
maxForwards = Proxy

origin :: Proxy 'Origin
origin = Proxy

proxyAuthorization :: Proxy 'ProxyAuthorization
proxyAuthorization = Proxy

range :: Proxy 'Range
range = Proxy

referer :: Proxy 'Referer
referer = Proxy

tE :: Proxy 'TE
tE = Proxy

userAgent :: Proxy 'UserAgent
userAgent = Proxy

xForwardedFor :: Proxy 'XForwardedFor
xForwardedFor = Proxy

xForwardedHost :: Proxy 'XForwardedHost
xForwardedHost = Proxy

xForwardedProto :: Proxy 'XForwardedProto
xForwardedProto = Proxy

xCsrfToken :: Proxy 'XCsrfToken
xCsrfToken = Proxy

accessControlAllowOrigin :: Proxy 'AccessControlAllowOrigin
accessControlAllowOrigin = Proxy

accessControlExposeHeaders :: Proxy 'AccessControlExposeHeaders
accessControlExposeHeaders = Proxy

accessControlMaxAge :: Proxy 'AccessControlMaxAge
accessControlMaxAge = Proxy

accessControlAllowCredentials :: Proxy 'AccessControlAllowCredentials
accessControlAllowCredentials = Proxy

accessControlAllowMethods :: Proxy 'AccessControlAllowMethods
accessControlAllowMethods = Proxy

accessControlAllowHeaders :: Proxy 'AccessControlAllowHeaders
accessControlAllowHeaders = Proxy

acceptPatch :: Proxy 'AcceptPatch
acceptPatch = Proxy

acceptRanges :: Proxy 'AcceptRanges
acceptRanges = Proxy

age :: Proxy 'Age
age = Proxy

allow :: Proxy 'Allow
allow = Proxy

contentDisposition :: Proxy 'ContentDisposition
contentDisposition = Proxy

contentEncoding :: Proxy 'ContentEncoding
contentEncoding = Proxy

contentLanguage :: Proxy 'ContentLanguage
contentLanguage = Proxy

contentLocation :: Proxy 'ContentLocation
contentLocation = Proxy

contentRange :: Proxy 'ContentRange
contentRange = Proxy

contentSecurityPolicy :: Proxy 'ContentSecurityPolicy
contentSecurityPolicy = Proxy

eTag :: Proxy 'ETag
eTag = Proxy

expires :: Proxy 'Expires
expires = Proxy

lastModified :: Proxy 'LastModified
lastModified = Proxy

link :: Proxy 'Link
link = Proxy

location :: Proxy 'Location
location = Proxy

proxyAuthenticate :: Proxy 'ProxyAuthenticate
proxyAuthenticate = Proxy

publicKeyPins :: Proxy 'PublicKeyPins
publicKeyPins = Proxy

retryAfter :: Proxy 'RetryAfter
retryAfter = Proxy

setCookie :: Proxy 'SetCookie
setCookie = Proxy

strictTransportSecurity :: Proxy 'StrictTransportSecurity
strictTransportSecurity = Proxy

trailer :: Proxy 'Trailer
trailer = Proxy

transferEncoding :: Proxy 'TransferEncoding
transferEncoding = Proxy

vary :: Proxy 'Vary
vary = Proxy

wWWAuthenticate :: Proxy 'WWWAuthenticate
wWWAuthenticate = Proxy


-- ReflectName Instances
--
-- A very verbose way of matching on lots of simple types. Not really worth
-- your time reading. Heavy potential for bugs.
-- ----------------------------------------------------------------------------


instance KnownSymbol s => ReflectName ('HeaderName s) where
  reflectName _ = fromString (symbolVal (Proxy :: Proxy s))

instance ReflectName 'CacheControl where reflectName _ = "Cache-Control"
instance ReflectName 'Connection where reflectName _ = "Connection"
instance ReflectName 'ContentLength where reflectName _ = "Content-Length"
instance ReflectName 'ContentType where reflectName _ = "Content-Type"
instance ReflectName 'Date where reflectName _ = "Date"
instance ReflectName 'Pragma where reflectName _ = "Pragma"
instance ReflectName 'Upgrade where reflectName _ = "Upgrade"
instance ReflectName 'Via where reflectName _ = "Via"
instance ReflectName 'Warning where reflectName _ = "Warning"
instance ReflectName 'Accept where reflectName _ = "Accept"
instance ReflectName 'AcceptCharset where reflectName _ = "Accept-Charset"
instance ReflectName 'AcceptEncoding where reflectName _ = "Accept-Encoding"
instance ReflectName 'AcceptLanguage where reflectName _ = "Accept-Language"
instance ReflectName 'AccessControlRequestMethod where reflectName _ = "Access-Control-Request-Method"
instance ReflectName 'AccessControlRequestHeaders where reflectName _ = "Access-Control-Request-Headers"
instance ReflectName 'Authorization where reflectName _ = "Authorization"
instance ReflectName 'Cookie where reflectName _ = "Cookie"
instance ReflectName 'Expect where reflectName _ = "Expect"
instance ReflectName 'From where reflectName _ = "From"
instance ReflectName 'Host where reflectName _ = "Host"
instance ReflectName 'IfMatch where reflectName _ = "If-Match"
instance ReflectName 'IfModifiedSince where reflectName _ = "If-Modified-Since"
instance ReflectName 'IfNoneMatch where reflectName _ = "If-None-Match"
instance ReflectName 'IfRange where reflectName _ = "If-Range"
instance ReflectName 'IfUnmodifiedSince where reflectName _ = "If-Unmodified-Since"
instance ReflectName 'MaxForwards where reflectName _ = "Max-Forwards"
instance ReflectName 'Origin where reflectName _ = "Origin"
instance ReflectName 'ProxyAuthorization where reflectName _ = "Proxy-Authorization"
instance ReflectName 'Range where reflectName _ = "Range"
instance ReflectName 'Referer where reflectName _ = "Referer"
instance ReflectName 'TE where reflectName _ = "TE"
instance ReflectName 'UserAgent where reflectName _ = "User-Agent"
instance ReflectName 'XForwardedFor where reflectName _ = "X-Forwarded-For"
instance ReflectName 'XForwardedHost where reflectName _ = "X-Forwarded-Host"
instance ReflectName 'XForwardedProto where reflectName _ = "X-Forwarded-Proto"
instance ReflectName 'XCsrfToken where reflectName _ = "X-Csrf-Token"
instance ReflectName 'AccessControlAllowOrigin where reflectName _ = "Access-Control-Allow-Origin"
instance ReflectName 'AccessControlExposeHeaders where reflectName _ = "Access-Control-Expose-Headers"
instance ReflectName 'AccessControlMaxAge where reflectName _ = "Access-Control-Max-Age"
instance ReflectName 'AccessControlAllowCredentials where reflectName _ = "Access-Control-Allow-Credentials"
instance ReflectName 'AccessControlAllowMethods where reflectName _ = "Access-Control-Allow-Methods"
instance ReflectName 'AccessControlAllowHeaders where reflectName _ = "Access-Control-Allow-Headers"
instance ReflectName 'AcceptPatch where reflectName _ = "Accept-Patch"
instance ReflectName 'AcceptRanges where reflectName _ = "Accept-Ranges"
instance ReflectName 'Age where reflectName _ = "Age"
instance ReflectName 'Allow where reflectName _ = "Allow"
instance ReflectName 'ContentDisposition where reflectName _ = "Content-Disposition"
instance ReflectName 'ContentEncoding where reflectName _ = "Content-Encoding"
instance ReflectName 'ContentLanguage where reflectName _ = "Content-Language"
instance ReflectName 'ContentLocation where reflectName _ = "Content-Location"
instance ReflectName 'ContentRange where reflectName _ = "Content-Range"
instance ReflectName 'ContentSecurityPolicy where reflectName _ = "Content-Security-Policy"
instance ReflectName 'ETag where reflectName _ = "ETag"
instance ReflectName 'Expires where reflectName _ = "Expires"
instance ReflectName 'LastModified where reflectName _ = "Last-Modified"
instance ReflectName 'Link where reflectName _ = "Link"
instance ReflectName 'Location where reflectName _ = "Location"
instance ReflectName 'ProxyAuthenticate where reflectName _ = "Proxy-Authenticate"
instance ReflectName 'PublicKeyPins where reflectName _ = "Public-Key-Pins"
instance ReflectName 'RetryAfter where reflectName _ = "Retry-After"
instance ReflectName 'SetCookie where reflectName _ = "Set-Cookie"
instance ReflectName 'StrictTransportSecurity where reflectName _ = "Strict-Transport-Security"
instance ReflectName 'Trailer where reflectName _ = "Trailer"
instance ReflectName 'TransferEncoding where reflectName _ = "Transfer-Encoding"
instance ReflectName 'Vary where reflectName _ = "Vary"
instance ReflectName 'WWWAuthenticate where reflectName _ = "WWW-Authenticate"
