{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Serv.Internal.Header where

import           Data.Proxy
import           Data.String
import           GHC.TypeLits
import qualified Network.HTTP.Types.Header as Header

-- This is a GHC 8 feature. It won't work today because we
-- cannot promote a GADT the way that we'd like to. Otherwise,
-- we'd let each name specify its "direction".
--
-- For instance, we'd have
--
--     data Name (d :: Direction) where
--       Name :: Symbol -> Name d
--       CacheControl :: Name d
--       Accept :: Name Request
--       Allow :: Name Response
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
data Name where

  ---- WILDCARD HEADER NAMES

  Name :: Symbol -> Name



  ---- COMMON HEADER NAMES


  -- | Used to specify directives that must be obeyed by all caching mechanisms
  -- along the request-response chain. Tells all caching mechanisms from server
  -- to client whether they may cache this object. It is measured in seconds
  --
  --     Cache-Control: no-cache
  CacheControl :: Name

  -- | Control options for the current connection and list of hop-by-hop
  -- request fields
  --
  --     Connection: keep-alive
  --     Connection: Upgrade
  Connection :: Name

  -- | The length of the request body in octets (8-bit bytes)
  --
  --     Content-Length: 348
  ContentLength :: Name

  -- | The MIME type of the body of the request (used with POST and PUT requests)
  --
  --     Content-Type: application/x-www-form-urlencoded
  ContentType :: Name

  -- | The date and time that the message was sent (in "HTTP-date" format as
  -- defined by RFC 7231 Date/Time Formats)
  --
  --     Date: Tue, 15 Nov 1994 08:12:31 GMT
  Date :: Name

  -- | Implementation-specific fields that may have various effects anywhere
  -- along the request-response chain.
  --
  --     Pragma: no-cache
  Pragma :: Name

  -- | Ask the server to upgrade to another protocol.
  --
  --     Upgrade: HTTP/2.0, SHTTP/1.3, IRC/6.9, RTA/x11
  Upgrade :: Name

  -- | Informs the server of proxies through which the request was sent.
  --
  --     Via: 1.0 fred, 1.1 example.com (Apache/1.1)
  Via :: Name

  -- | A general warning about possible problems with the entity body.
  --
  --     Warning: 199 Miscellaneous warning
  Warning :: Name



  ---- REQUEST-SPECIFIC HEADER NAMES


  -- | Content-Types that are acceptable for the response.
  --
  --     Accept: text/plain
  Accept :: Name

  -- | Character sets that are acceptable
  --
  --     Accept-Charset: utf-8
  AcceptCharset :: Name

  -- | List of acceptable encodings.
  --
  --     Accept-Encoding: gzip, deflate
  AcceptEncoding :: Name

  -- | List of acceptable human languages for response.
  --
  --     Accept-Language: en-US
  AcceptLanguage :: Name

  -- | Used when issuing a preflight request to let the server
  -- know what HTTP method will be used when the actual request is made.
  --
  --     Access-Control-Request-Method: POST
  AccessControlRequestMethod :: Name

  -- | Used when issuing a preflight request to let the server know
  -- what HTTP headers will be used when the actual request is made.
  --
  --     Access-Control-Request-Headers: X-PINGOTHER
  AccessControlRequestHeaders :: Name

  -- | Authentication credentials for HTTP authentication
  --
  --     Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
  Authorization :: Name

  -- | An HTTP cookie previously sent by the server with Set-Cookie (below)
  --
  --     Cookie: $Version=1; Skin=new;
  Cookie :: Name

  -- | Indicates that particular server behaviors are required by the client
  --
  --     Expect: 100-continue
  Expect :: Name

  -- | The email address of the user making the request
  --
  --     From: user@example.com
  From :: Name

  -- | The domain name of the server (for virtual hosting), and the TCP port
  -- number on which the server is listening. The port number may be omitted
  -- if the port is the standard port for the service requested.
  --
  --     Host: en.wikipedia.org:80
  --     Host: en.wikipedia.org
  Host :: Name

  -- | Only perform the action if the client supplied entity matches the same
  -- entity on the server. This is mainly for methods like PUT to only update
  -- a resource if it has not been modified since the user last updated it.
  --
  --     If-Match: "737060cd8c284d8af7ad3082f209582d"
  IfMatch :: Name

  -- | Allows a 304 Not Modified to be returned if content is unchanged
  --
  --     If-Modified-Since: Sat, 29 Oct 1994 19:43:31 GMT
  IfModifiedSince :: Name

  -- | Allows a 304 Not Modified to be returned if content is unchanged,
  -- see HTTP ETag
  --
  --     If-None-Match: "737060cd8c284d8af7ad3082f209582d"
  IfNoneMatch :: Name

  -- | If the entity is unchanged, send me the part(s) that I am missing;
  -- otherwise, send me the entire new entity
  --
  --     If-Range: "737060cd8c284d8af7ad3082f209582d"
  IfRange :: Name

  -- | Only send the response if the entity has not been modified since a
  -- specific time.
  --
  --     If-Unmodified-Since: Sat, 29 Oct 1994 19:43:31 GMT
  IfUnmodifiedSince :: Name

  -- | Limit the number of times the message can be forwarded through proxies
  -- or gateways.
  --
  --     Max-Forwards: 10
  MaxForwards :: Name

  -- | Initiates a request for cross-origin resource sharing (asks server for
  -- an 'Access-Control-Allow-Origin' response field).
  --
  -- Origin: http://www.example-social-network.com
  Origin :: Name

  -- | Authorization credentials for connecting to a proxy.
  --
  --     Proxy-Authorization: Basic QWxhZGRpbjpvcGVuIHNlc2FtZQ==
  ProxyAuthorization :: Name

  -- | Request only part of an entity. Bytes are numbered from 0.
  --
  --     Range: bytes=500-999
  Range :: Name

  -- | This is the address of the previous web page from which a link to the
  -- currently requested page was followed. (The word “referrer” has been
  -- misspelled in the RFC as well as in most implementations to the point
  -- that it has become standard usage and is considered correct terminology)
  --
  --     Referer: http://en.wikipedia.org/wiki/Main_Page
  Referer :: Name

  -- | The transfer encodings the user agent is willing to accept: the same
  -- values as for the response header field Transfer-Encoding can be used,
  -- plus the "trailers" value (related to the "chunked" transfer method) to
  -- notify the server it expects to receive additional fields in the trailer
  -- after the last, zero-sized, chunk.
  --
  --     TE: trailers, deflate
  TE :: Name

  -- | The user agent string of the user agent
  --
  --     User-Agent: Mozilla/5.0 (X11; Linux x86_64; rv:12.0) Gecko/20100101 Firefox/21.0
  UserAgent :: Name

  -- | A de facto standard for identifying the originating IP address of a
  -- client connecting to a web server through an HTTP proxy or load balancer
  --
  --     X-Forwarded-For: client1, proxy1, proxy2
  --     X-Forwarded-For: 129.78.138.66, 129.78.64.103
  XForwardedFor :: Name

  -- | A de facto standard for identifying the original host requested by the client in
  -- the Host HTTP request header, since the host name and/or port of the reverse
  -- proxy (load balancer) may differ from the origin server handling the request.
  --
  --     X-Forwarded-Host: en.wikipedia.org:80
  --     X-Forwarded-Host: en.wikipedia.org
  XForwardedHost :: Name

  -- | A de facto standard for identifying the originating protocol of an HTTP request,
  -- since a reverse proxy (or a load balancer) may communicate with a web server using
  -- HTTP even if the request to the reverse proxy is HTTPS.
  XForwardedProto :: Name

  -- | Used to prevent cross-site request forgery.
  --
  --     X-Csrf-Token: i8XNjC4b8KVok4uw5RftR38Wgp2BFwql
  XCsrfToken :: Name




  ---- RESPONSE-SPECIFIC HEADER NAMES


  -- | When responding to a CORS request this lists either
  -- an origin or @*@ describing the server's CORS policy for
  -- the requested resource
  --
  --     Access-Control-Allow-Origin: *
  --     Access-Control-Allow-Origin: http://foo.example
  --     Access-Control-Allow-Origin: https://foo.example
  AccessControlAllowOrigin :: Name

  -- | This header lets a server whitelist headers that browsers are allowed to access.
  --
  --     Access-Control-Expose-Headers: X-My-Custom-Header, X-Another-Custom-Header
  AccessControlExposeHeaders :: Name

  -- | This header indicates how long the results of a preflight request can be cached.
  --
  --     Access-Control-Max-Age: <delta-seconds>
  AccessControlMaxAge :: Name

  -- | Indicates whether or not the response to the request can be exposed when the
  -- credentials flag is true. When used as part of a response to a preflight request,
  -- this indicates whether or not the actual request can be made using credentials. Note
  -- that simple GET requests are not preflighted, and so if a request is made for a
  -- resource with credentials, if this header is not returned with the resource, the
  -- response is ignored by the browser and not returned to web content.
  --
  --     Access-Control-Allow-Credentials: true | false
  AccessControlAllowCredentials :: Name

  -- | Specifies the method or methods allowed when accessing the resource. This is
  -- used in response to a preflight request.
  --
  --     Access-Control-Allow-Methods: <method>[, <method>]*
  AccessControlAllowMethods :: Name

  -- | Used in response to a preflight request to indicate which HTTP headers can
  -- be used when making the actual request.
  AccessControlAllowHeaders :: Name

  -- | Specifies which patch document formats this server supports
  --
  --     Accept-Patch: text/example;charset=utf-8
  AcceptPatch :: Name

  -- | What partial content range types this server supports via byte serving
  --
  --     Accept-Ranges: bytes
  AcceptRanges :: Name

  -- | The age the object has been in a proxy cache in seconds
  --
  --     Age: 12
  Age :: Name

  -- | Valid actions for a specified resource. To be used for a 405 Method not allowed
  --
  --     Allow: GET, HEAD
  Allow :: Name

  -- | An opportunity to raise a "File Download" dialogue box for a known MIME type
  -- with binary format or suggest a filename for dynamic content. Quotes are
  -- necessary with special characters.
  --
  --     Content-Disposition: attachment; filename="fname.ext"
  ContentDisposition :: Name

  -- | The type of encoding used on the data.
  --
  --     Content-Encoding: gzip
  ContentEncoding :: Name

  -- | The natural language or languages of the intended audience for the enclosed content
  --
  --     Content-Language: da
  ContentLanguage :: Name

  -- | An alternate location for the returned data
  --
  --     Content-Location: /index.htm
  ContentLocation :: Name

  -- | Where in a full body message this partial message belongs
  --
  --     Content-Range: bytes 21010-47021/47022
  ContentRange :: Name

  -- | Content Security Policy definition.
  --
  --     Content-Security-Policy: default-src 'self'
  ContentSecurityPolicy :: Name

  -- | An identifier for a specific version of a resource, often a message digest
  --
  --     ETag: "737060cd8c284d8af7ad3082f209582d"
  ETag :: Name

  -- | Gives the date/time after which the response is considered stale (in
  -- "HTTP-date" format as defined by RFC 7231)
  --
  --     Expires: Thu, 01 Dec 1994 16:00:00 GMT
  Expires :: Name

  -- | The last modified date for the requested object (in "HTTP-date" format as
  -- defined by RFC 7231)
  --
  --     Last-Modified: Tue, 15 Nov 1994 12:45:26 GMT
  LastModified :: Name

  -- | Used to express a typed relationship with another resource, where the
  -- relation type is defined by RFC 5988
  --
  --     Link: </feed>; rel="alternate"
  Link :: Name

  -- | Used in redirection, or when a new resource has been created.
  --
  --     Location: http://www.w3.org/pub/WWW/People.html
  Location :: Name

  -- | Request authentication to access the proxy.
  --
  --     Proxy-Authenticate: Basic
  ProxyAuthenticate :: Name

  -- | HTTP Public Key Pinning, announces hash of website's authentic TLS certificate
  --
  --     Public-Key-Pins: max-age=2592000; pin-sha256="E9CZ9INDbd+2eRQozYqqbQ2yXLVKB9+xcprMF+44U1g=";
  PublicKeyPins :: Name

  -- | If an entity is temporarily unavailable, this instructs the client to
  -- try again later. Value could be a specified period of time (in seconds) or a HTTP-date.
  --
  --     Retry-After: 120
  --     Retry-After: Fri, 07 Nov 2014 23:59:59 GMT
  RetryAfter :: Name

  -- | An HTTP cookie
  --
  --     Set-Cookie: UserID=JohnDoe; Max-Age=3600; Version=1
  SetCookie :: Name

  -- | A HSTS Policy informing the HTTP client how long to cache the HTTPS only
  -- policy and whether this applies to subdomains.
  --
  --     Strict-Transport-Security: max-age=16070400; includeSubDomains
  StrictTransportSecurity :: Name

  -- | The Trailer general field value indicates that the given set of header fields
  -- is present in the trailer of a message encoded with chunked transfer coding.
  --
  --     Trailer: Max-Forwards
  Trailer :: Name

  -- | The form of encoding used to safely transfer the entity to the user. Currently defined
  -- methods are: chunked, compress, deflate, gzip, identity.
  --
  --     Transfer-Encoding: chunked
  TransferEncoding :: Name

  -- | Tells downstream proxies how to match future request headers to decide whether
  -- the cached response can be used rather than requesting a fresh one from the origin server.
  --
  --     Vary: *
  --     Vary: Accept-Language
  Vary :: Name

  -- | Indicates the authentication scheme that should be used to access the requested entity.
  --
  --     WWW-Authenticate: Basic
  WWWAuthenticate :: Name

-- | Implements reflection of 'Name' types into 'Header.HeaderName' values for runtime use.
class ReflectName (n :: Name) where
  reflectName :: proxy n -> Header.HeaderName


-- ReflectName Instances
--
-- A very verbose way of matching on lots of simple types. Not really worth
-- your time reading. Heavy potential for bugs.
-- ----------------------------------------------------------------------------


instance KnownSymbol s => ReflectName ('Name s) where
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
