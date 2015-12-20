{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE UndecidableInstances       #-}

module Serv.Internal.Header.Name where

import           Data.CaseInsensitive     (CI)
import qualified Data.CaseInsensitive     as CI
import           Data.Proxy
import           Data.Singletons
import           Data.Singletons.TypeLits
import           Data.String
import           Data.Text                (Text)
import qualified Data.Text                as Text

-- | Header types are distinguished by name. Any given 'HeaderType' has and
-- is distinguished by a case-insensitive 'HeaderName'.
newtype HeaderName
  = HeaderName { getHeaderName :: CI Text }
  deriving ( Eq, Ord, Show, IsString )

-- | The variant (name and meaning) of a HTTP header.
--
-- An incomplete listing of most permanant headers, a selection of provisional
-- ones, and one or two non-standard headers. Generally, any header desired can
-- be specified using the 'Name' constructor.
--
-- Used only as their @DataKinds@ types system.
data HeaderType

  ---- WILDCARD HEADER NAMES

  = CustomHeader Symbol
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

data instance Sing (n :: HeaderType) where

  ---- WILDCARD HEADER NAMES
  SCustomHeader :: KnownSymbol n => Sing n -> Sing ('CustomHeader n)

  ---- COMMON HEADER NAMES
  SCacheControl :: Sing 'CacheControl
  SConnection :: Sing 'Connection
  SContentLength :: Sing 'ContentLength
  SContentType :: Sing 'ContentType
  SDate :: Sing 'Date
  SPragma :: Sing 'Pragma
  SUpgrade :: Sing 'Upgrade
  SVia :: Sing 'Via
  SWarning :: Sing 'Warning

  ---- REQUEST-SPECIFIC HEADER NAMES
  SAccept :: Sing 'Accept
  SAcceptCharset :: Sing 'AcceptCharset
  SAcceptEncoding :: Sing 'AcceptEncoding
  SAcceptLanguage :: Sing 'AcceptLanguage
  SAccessControlRequestMethod :: Sing 'AccessControlRequestMethod
  SAccessControlRequestHeaders :: Sing 'AccessControlRequestHeaders
  SAuthorization :: Sing 'Authorization
  SCookie :: Sing 'Cookie
  SExpect :: Sing 'Expect
  SFrom :: Sing 'From
  SHost :: Sing 'Host
  SIfMatch :: Sing 'IfMatch
  SIfModifiedSince :: Sing 'IfModifiedSince
  SIfNoneMatch :: Sing 'IfNoneMatch
  SIfRange :: Sing 'IfRange
  SIfUnmodifiedSince :: Sing 'IfUnmodifiedSince
  SMaxForwards :: Sing 'MaxForwards
  SOrigin :: Sing 'Origin
  SProxyAuthorization :: Sing 'ProxyAuthorization
  SRange :: Sing 'Range
  SReferer :: Sing 'Referer
  STE :: Sing 'TE
  SUserAgent :: Sing 'UserAgent
  SXForwardedFor :: Sing 'XForwardedFor
  SXForwardedHost :: Sing 'XForwardedHost
  SXForwardedProto :: Sing 'XForwardedProto
  SXCsrfToken :: Sing 'XCsrfToken

  ---- RESPONSE-SPECIFIC HEADER NAMES
  SAccessControlAllowOrigin :: Sing 'AccessControlAllowOrigin
  SAccessControlExposeHeaders :: Sing 'AccessControlExposeHeaders
  SAccessControlMaxAge :: Sing 'AccessControlMaxAge
  SAccessControlAllowCredentials :: Sing 'AccessControlAllowCredentials
  SAccessControlAllowMethods :: Sing 'AccessControlAllowMethods
  SAccessControlAllowHeaders :: Sing 'AccessControlAllowHeaders
  SAcceptPatch :: Sing 'AcceptPatch
  SAcceptRanges :: Sing 'AcceptRanges
  SAge :: Sing 'Age
  SAllow :: Sing 'Allow
  SContentDisposition :: Sing 'ContentDisposition
  SContentEncoding :: Sing 'ContentEncoding
  SContentLanguage :: Sing 'ContentLanguage
  SContentLocation :: Sing 'ContentLocation
  SContentRange :: Sing 'ContentRange
  SContentSecurityPolicy :: Sing 'ContentSecurityPolicy
  SETag :: Sing 'ETag
  SExpires :: Sing 'Expires
  SLastModified :: Sing 'LastModified
  SLink :: Sing 'Link
  SLocation :: Sing 'Location
  SProxyAuthenticate :: Sing 'ProxyAuthenticate
  SPublicKeyPins :: Sing 'PublicKeyPins
  SRetryAfter :: Sing 'RetryAfter
  SSetCookie :: Sing 'SetCookie
  SStrictTransportSecurity :: Sing 'StrictTransportSecurity
  STrailer :: Sing 'Trailer
  STransferEncoding :: Sing 'TransferEncoding
  SVary :: Sing 'Vary
  SWWWAuthenticate :: Sing 'WWWAuthenticate

instance SingKind ('KProxy :: KProxy HeaderType) where
  type DemoteRep ('KProxy :: KProxy HeaderType) =
    HeaderName

  fromSing s = case s of

    SCustomHeader name -> fromString (symbolVal name)

    SCacheControl -> "Cache-Control"
    SConnection -> "Connection"
    SContentLength -> "Content-Length"
    SContentType -> "Content-Type"
    SDate -> "Date"
    SPragma -> "Pragma"
    SUpgrade -> "Upgrade"
    SVia -> "Via"
    SWarning -> "Warning"
    SAccept -> "Accept"
    SAcceptCharset -> "Accept-Charset"
    SAcceptEncoding -> "Accept-Encoding"
    SAcceptLanguage -> "Accept-Language"
    SAccessControlRequestMethod -> "Access-Control-Request-Method"
    SAccessControlRequestHeaders -> "Access-Control-Request-Headers"
    SAuthorization -> "Authorization"
    SCookie -> "Cookie"
    SExpect -> "Expect"
    SFrom -> "From"
    SHost -> "Host"
    SIfMatch -> "If-Match"
    SIfModifiedSince -> "If-Modified-Since"
    SIfNoneMatch -> "If-None-Match"
    SIfRange -> "If-Range"
    SIfUnmodifiedSince -> "If-Unmodified-Since"
    SMaxForwards -> "Max-Forwards"
    SOrigin -> "Origin"
    SProxyAuthorization -> "Proxy-Authorization"
    SRange -> "Range"
    SReferer -> "Referer"
    STE -> "TE"
    SUserAgent -> "User-Agent"
    SXForwardedFor -> "X-Forwarded-For"
    SXForwardedHost -> "X-Forwarded-Host"
    SXForwardedProto -> "X-Forwarded-Proto"
    SXCsrfToken -> "X-Csrf-Token"
    SAccessControlAllowOrigin -> "Access-Control-Allow-Origin"
    SAccessControlExposeHeaders -> "Access-Control-Expose-Headers"
    SAccessControlMaxAge -> "Access-Control-Max-Age"
    SAccessControlAllowCredentials -> "Access-Control-Allow-Credentials"
    SAccessControlAllowMethods -> "Access-Control-Allow-Methods"
    SAccessControlAllowHeaders -> "Access-Control-Allow-Headers"
    SAcceptPatch -> "Accept-Patch"
    SAcceptRanges -> "Accept-Ranges"
    SAge -> "Age"
    SAllow -> "Allow"
    SContentDisposition -> "Content-Disposition"
    SContentEncoding -> "Content-Encoding"
    SContentLanguage -> "Content-Language"
    SContentLocation -> "Content-Location"
    SContentRange -> "Content-Range"
    SContentSecurityPolicy -> "Content-Security-Policy"
    SETag -> "ETag"
    SExpires -> "Expires"
    SLastModified -> "Last-Modified"
    SLink -> "Link"
    SLocation -> "Location"
    SProxyAuthenticate -> "Proxy-Authenticate"
    SPublicKeyPins -> "Public-Key-Pins"
    SRetryAfter -> "Retry-After"
    SSetCookie -> "Set-Cookie"
    SStrictTransportSecurity -> "Strict-Transport-Security"
    STrailer -> "Trailer"
    STransferEncoding -> "Transfer-Encoding"
    SVary -> "Vary"
    SWWWAuthenticate -> "WWW-Authenticate"

  toSing r =
    case r of
      "Cache-Control" -> SomeSing SCacheControl
      "Connection" -> SomeSing SConnection
      "Content-Length" -> SomeSing SContentLength
      "Content-Type" -> SomeSing SContentType
      "Date" -> SomeSing SDate
      "Pragma" -> SomeSing SPragma
      "Upgrade" -> SomeSing SUpgrade
      "Via" -> SomeSing SVia
      "Warning" -> SomeSing SWarning
      "Accept" -> SomeSing SAccept
      "Accept-Charset" -> SomeSing SAcceptCharset
      "Accept-Encoding" -> SomeSing SAcceptEncoding
      "Accept-Language" -> SomeSing SAcceptLanguage
      "Access-Control-Request-Method" -> SomeSing SAccessControlRequestMethod
      "Access-Control-Request-Headers" -> SomeSing SAccessControlRequestHeaders
      "Authorization" -> SomeSing SAuthorization
      "Cookie" -> SomeSing SCookie
      "Expect" -> SomeSing SExpect
      "From" -> SomeSing SFrom
      "Host" -> SomeSing SHost
      "If-Match" -> SomeSing SIfMatch
      "If-Modified-Since" -> SomeSing SIfModifiedSince
      "If-None-Match" -> SomeSing SIfNoneMatch
      "If-Range" -> SomeSing SIfRange
      "If-Unmodified-Since" -> SomeSing SIfUnmodifiedSince
      "Max-Forwards" -> SomeSing SMaxForwards
      "Origin" -> SomeSing SOrigin
      "Proxy-Authorization" -> SomeSing SProxyAuthorization
      "Range" -> SomeSing SRange
      "Referer" -> SomeSing SReferer
      "TE" -> SomeSing STE
      "User-Agent" -> SomeSing SUserAgent
      "X-Forwarded-For" -> SomeSing SXForwardedFor
      "X-Forwarded-Host" -> SomeSing SXForwardedHost
      "X-Forwarded-Proto" -> SomeSing SXForwardedProto
      "X-Csrf-Token" -> SomeSing SXCsrfToken
      "Access-Control-Allow-Origin" -> SomeSing SAccessControlAllowOrigin
      "Access-Control-Expose-Headers" -> SomeSing SAccessControlExposeHeaders
      "Access-Control-Max-Age" -> SomeSing SAccessControlMaxAge
      "Access-Control-Allow-Credentials" -> SomeSing SAccessControlAllowCredentials
      "Access-Control-Allow-Methods" -> SomeSing SAccessControlAllowMethods
      "Access-Control-Allow-Headers" -> SomeSing SAccessControlAllowHeaders
      "Accept-Patch" -> SomeSing SAcceptPatch
      "Accept-Ranges" -> SomeSing SAcceptRanges
      "Age" -> SomeSing SAge
      "Allow" -> SomeSing SAllow
      "Content-Disposition" -> SomeSing SContentDisposition
      "Content-Encoding" -> SomeSing SContentEncoding
      "Content-Language" -> SomeSing SContentLanguage
      "Content-Location" -> SomeSing SContentLocation
      "Content-Range" -> SomeSing SContentRange
      "Content-Security-Policy" -> SomeSing SContentSecurityPolicy
      "ETag" -> SomeSing SETag
      "Expires" -> SomeSing SExpires
      "Last-Modified" -> SomeSing SLastModified
      "Link" -> SomeSing SLink
      "Location" -> SomeSing SLocation
      "Proxy-Authenticate" -> SomeSing SProxyAuthenticate
      "Public-Key-Pins" -> SomeSing SPublicKeyPins
      "Retry-After" -> SomeSing SRetryAfter
      "Set-Cookie" -> SomeSing SSetCookie
      "Strict-Transport-Security" -> SomeSing SStrictTransportSecurity
      "Trailer" -> SomeSing STrailer
      "Transfer-Encoding" -> SomeSing STransferEncoding
      "Vary" -> SomeSing SVary
      "WWW-Authenticate" -> SomeSing SWWWAuthenticate

      custom ->
        withSomeSing
        (Text.unpack . CI.original . getHeaderName $ custom)
        (\s -> SomeSing (withKnownSymbol s (SCustomHeader s)))

instance KnownSymbol n => SingI ('CustomHeader n) where sing = SCustomHeader (sing :: Sing n)
instance SingI 'CacheControl where sing = SCacheControl
instance SingI 'Connection where sing = SConnection
instance SingI 'ContentLength where sing = SContentLength
instance SingI 'ContentType where sing = SContentType
instance SingI 'Date where sing = SDate
instance SingI 'Pragma where sing = SPragma
instance SingI 'Upgrade where sing = SUpgrade
instance SingI 'Via where sing = SVia
instance SingI 'Warning where sing = SWarning
instance SingI 'Accept where sing = SAccept
instance SingI 'AcceptCharset where sing = SAcceptCharset
instance SingI 'AcceptEncoding where sing = SAcceptEncoding
instance SingI 'AcceptLanguage where sing = SAcceptLanguage
instance SingI 'AccessControlRequestMethod where sing = SAccessControlRequestMethod
instance SingI 'AccessControlRequestHeaders where sing = SAccessControlRequestHeaders
instance SingI 'Authorization where sing = SAuthorization
instance SingI 'Cookie where sing = SCookie
instance SingI 'Expect where sing = SExpect
instance SingI 'From where sing = SFrom
instance SingI 'Host where sing = SHost
instance SingI 'IfMatch where sing = SIfMatch
instance SingI 'IfModifiedSince where sing = SIfModifiedSince
instance SingI 'IfNoneMatch where sing = SIfNoneMatch
instance SingI 'IfRange where sing = SIfRange
instance SingI 'IfUnmodifiedSince where sing = SIfUnmodifiedSince
instance SingI 'MaxForwards where sing = SMaxForwards
instance SingI 'Origin where sing = SOrigin
instance SingI 'ProxyAuthorization where sing = SProxyAuthorization
instance SingI 'Range where sing = SRange
instance SingI 'Referer where sing = SReferer
instance SingI 'TE where sing = STE
instance SingI 'UserAgent where sing = SUserAgent
instance SingI 'XForwardedFor where sing = SXForwardedFor
instance SingI 'XForwardedHost where sing = SXForwardedHost
instance SingI 'XForwardedProto where sing = SXForwardedProto
instance SingI 'XCsrfToken where sing = SXCsrfToken
instance SingI 'AccessControlAllowOrigin where sing = SAccessControlAllowOrigin
instance SingI 'AccessControlExposeHeaders where sing = SAccessControlExposeHeaders
instance SingI 'AccessControlMaxAge where sing = SAccessControlMaxAge
instance SingI 'AccessControlAllowCredentials where sing = SAccessControlAllowCredentials
instance SingI 'AccessControlAllowMethods where sing = SAccessControlAllowMethods
instance SingI 'AccessControlAllowHeaders where sing = SAccessControlAllowHeaders
instance SingI 'AcceptPatch where sing = SAcceptPatch
instance SingI 'AcceptRanges where sing = SAcceptRanges
instance SingI 'Age where sing = SAge
instance SingI 'Allow where sing = SAllow
instance SingI 'ContentDisposition where sing = SContentDisposition
instance SingI 'ContentEncoding where sing = SContentEncoding
instance SingI 'ContentLanguage where sing = SContentLanguage
instance SingI 'ContentLocation where sing = SContentLocation
instance SingI 'ContentRange where sing = SContentRange
instance SingI 'ContentSecurityPolicy where sing = SContentSecurityPolicy
instance SingI 'ETag where sing = SETag
instance SingI 'Expires where sing = SExpires
instance SingI 'LastModified where sing = SLastModified
instance SingI 'Link where sing = SLink
instance SingI 'Location where sing = SLocation
instance SingI 'ProxyAuthenticate where sing = SProxyAuthenticate
instance SingI 'PublicKeyPins where sing = SPublicKeyPins
instance SingI 'RetryAfter where sing = SRetryAfter
instance SingI 'SetCookie where sing = SSetCookie
instance SingI 'StrictTransportSecurity where sing = SStrictTransportSecurity
instance SingI 'Trailer where sing = STrailer
instance SingI 'TransferEncoding where sing = STransferEncoding
instance SingI 'Vary where sing = SVary
instance SingI 'WWWAuthenticate where sing = SWWWAuthenticate
