
# Serv, kind-safe framework for type-safe APIs

Serv is a collection of libraries for creating HTTP APIs. You construct a
sophisticated, custom *type* which represents the structure of your API and
then use libraries to construct servers, clients, and documentation which are
statically guaranteed to conform to your specification.

Serv is heavily inspired by Haskell’s
[`servant`](http://github.com/haskell-servant/servant) which also provides
type-safe APIs. Unlike `servant`, Serv endeavors to provide an API
specification language which is itself well-typed (or, really, well-*kinded*)
and benefits in several ways for this choice. Serv is another point in a
similar design space.

## Example API specification

(*This to come, recent changes in the API language have invalidated existing
examples. -tel*)

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

## Contributing

**Serv is still in early design phases**. Contribution ranging from PRs to
feedback on the API design language is *very welcome*. See the [issues
tracker](https://github.com/tel/serv/issues) to find interesting projects,
submit new issues, or email me at [mailto:me@jspha.com](me@jspha.com).
