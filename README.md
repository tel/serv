
# Serv, kind-safe framework for type-safe APIs

[![Build Status](https://travis-ci.org/tel/serv.svg)](https://travis-ci.org/tel/serv)

Serv is a collection of libraries for creating HTTP APIs. You construct a
sophisticated, custom *type* which represents the structure of your API and
then use libraries to construct servers, clients, and documentation which are
statically guaranteed to conform to your specification.

## Example API specification

```haskell
type RawBody = HasBody '[TextPlain] Text
type JSONBody = HasBody '[JSON] [Int]

type TheApi
  = Const “user” :> 
      Endpoint ()
      '[
        Method GET
         '[ Ok ::: Respond 
                     '[Lastmodified ::: UTCTime]
                     (HasBody ‘[JSON, TextPlain] User) ]
       , CaptureBody ‘[JSON, TextPlain] User
           (Method PUT
              '[ Ok ::: Respond '[Location ::: URI] Empty) ])
       ]
```

## Libraries

This repo contains the 3 main existing Serv libraries:

- [`serv`](https://github.com/tel/serv/tree/master/serv) describes the API
  specification type language. It’s the basic dependency of all `serv-*`
  libraries.
- [`serv-wai`](https://github.com/tel/serv/tree/master/serv-wai) is a tool for
  constructing ([Wai](https://hackage.haskell.org/package/wai)-style) web
  servers which statically conform to `serv` API types.
- [`http-kinder`](https://github.com/tel/serv/tree/master/http-kinder) provides
  detailed types and kinds representing headers, query parameters, status
  codes, and uri segments in HTTP requests. It also provides for
  encoding/decoding these pieces of the request as Haskell types.

## Prior art

Serv is heavily inspired by Haskell’s
[`servant`](http://github.com/haskell-servant/servant) which also provides
type-safe APIs. Unlike `servant`, Serv endeavors to provide an API
specification language which is itself well-typed (or, really, well-*kinded*)
and benefits in several ways for this choice. Serv is another point in a
similar design space. Or, in bulleted list format:

- Serv's API language is *kind-restricted* enabling us to be more sure that API
  descriptions make sense and enabling us to write closed type functions over
  API descriptions with greater confidence that the function will not become
  stuck. On the other hand, `servant` has all API descriptions at kind `*` and
  therefore is more easily extensible.

- Serv's API language is also more regular than `servant`'s. Again this is
  helpful for writing type functions to analyze API descriptions since they
  can pattern match on each path, api, and method component in the description.

- Serv's API language includes an explicit notion of an `Endpoint` and even
  listing of the `Status` codes which might arise whereas `servant`'s is more
  implicit (though it could be extended to include this idea). Explicit
  `Endpoint`s enable proper OPTIONS (and eventually CORS) handling in a way
  that's transparent to you, the library user.

## Contributing

**Serv is still in early design phases**. Contribution ranging from PRs to
feedback on the API design language is *very welcome*. See the [issues
tracker](https://github.com/tel/serv/issues) to find interesting projects,
submit new issues, or email me at [mailto:me@jspha.com](me@jspha.com).
