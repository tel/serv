
# Serv

*Dependently typed API servers, clients, and documentation.*

[![Build Status](https://travis-ci.org/tel/serv.svg)](https://travis-ci.org/tel/serv)

Serv separates the description of an API from the implementation of the server,
the client definitions, and the documentation. Unlike other such efforts, the
API descriptions are *types* and type level computation can be used to ensure
that the server, client, and documentation *always match*.

# Status and how you can help!

**Serv is still in the early design phases.** Contribution is *very welcome*
along the lines of the public API (the API types, especially) and functionality
requests. I’m also really quite bad at naming things sometimes and would not
mind *at all* some commentary.

Files to critique:

- [Serv.Internal.Api](https://github.com/tel/serv/blob/master/src/Serv/Internal/Api.hs)
  which is where the types and kinds of the API interface definition language
  are described. This is easily the *most vital* part of Serv.
- [Serv.Internal.Header](https://github.com/tel/serv/blob/master/src/Serv/Internal/Header.hs)
  is where a *big*, *complicated* type encodes all common headers allowing for
  extension. This feels like a bad idea sometimes, but it certainly *seems* to
  fit with the house hasochism style.
- [Serv.Internal.Header.Serialization](https://github.com/tel/serv/blob/master/src/Serv/Internal/Header/Serialization.hs)
  describes how to give semantics to text blobs by claiming that they’re
  supposed to be a representation of some Haskell type under the regime of a
  given header. What kinds of things should be added here? How can Serv best
  handle the orphan instance issue?
- [Serv.Internal.Api.Annotation](https://github.com/tel/serv/blob/master/src/Serv/Internal/Api/Annotation.hs)
  Currently this is basically unused, but it’s goal is to provide a way to have
  a hook into *endpoints* instead of the data types they return (*a la*
  Servant) for documentation purposes. The design here is super speculative.
- [Serv.Internal.Server](https://github.com/tel/serv/tree/master/src/Serv/Internal/Server)
  This whole hunk of junk is the driving use case for the entire library at the
  moment! It’s also sloppy as all hell at times as is resolves itself against
  the changing `Api` API.

I’m also very interested in tips and tricks for working with singleton types as
that’s a key piece of how Serv works!

# Overview

Most APIs have a simple regular structure which is shared by the server, the
clients, and (hopefully) the documentation. *This repetition is bad* leading to
overhead, mismatching, and casualities of API drift in clients and
documentation.

Serv solves this problem by letting you specify your entire API structure once,
canonically, and then either implement or derive servers, clients, and
documentation from it.

This solution is not new, but Serv takes it one step further by having the API
specification be written *in the type system* which allows for static assurance
that the server, client, and documentation cannot possibly drift from one
another.

## How Serv Works

Serv lets you specify your API as a type. 

For instance, if your have an API-description type `A` then `Impl IO A` is the
type of a server implementation (running in the `IO` monad) which necessarily
follows the same structure as `A`. It will require this server to consume data
read from the path, will automatically route things properly, and ensures that
endpoints deliver the proper bodies and headers. 

All of this is statically assured, constructed from data in `A`.

This works by leveraging the weak *dependent type* capabilities in Haskell’s
type system enabling compile-time analysis of the type structure of `A`.
Whatever data can be read from `A` is used to handle the boring, repetitive
parts of your server automatically. For the interesting parts, Serv requires
that you specify the responses your server provides according to the exact
specification listed in `A`.

Similar type functions and analyses are performed to enable Serv to generate
documentation and client libraries (these to come!).

# Tutorial

## Describing HTTP Methods

The most basic component of an API is the `Method`. We begin describing APIs by
listing the `Method`s they respond to. To do this, we define a set of types. For
instance, a simple API might look like

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE TypeOperators     #-}

import Serv.Api
import qualified Serv.StatusCode as Sc

type Method_1 = 'Method 'DELETE '[ 'Sc.NoContent ::: 'Respond '[] 'Empty ]
```

where the single-quotes indicate that we are using a data constructor at the
type-level. For instance, Serv defines the type `data Verb = GET | PUT | DELETE
| ...` but GHC's `DataKinds` extension allows us to treat this also as a set of
types `'GET`, `'PUT`, `'DELETE`, etc. which all have *kind* `Verb`. Serv uses
`DataKinds` extensively to enable kind-safe type-level programming in the same
way that Haskell normally enables type-safe value-level programming.

For convenience, Serv exports a number of type synonyms to cut down on
syntactic noise caused by constantly having to promote types to kinds. Thus,
the following is equivalent to the above and less cumbersome to type:

type Method_1 = Method DELETE '[ Sc.NoContent ::: Respond '[] Empty ]

Above, `Method_1` is a type describing a single method, a `DELETE` method which
produces an empty response with no special headers and a no content status code
(204). We use a type-level list `'[]` to describe the set of headers returned.

Another method might return a particular header:

```haskell
import           Serv.Common (RawText)
import qualified Serv.Header as H

type Method_2 = Method POST 
                '[ Sc.NoContent ::: Respond '[ H.Location ::: RawText ] Empty ]
```

Here, `Method_2` must return the `Location` header specified by some `RawText`
value. We use `(:::)` to link `H.HeaderName`s to Haskell types which *have
semantics for* that header. `RawText` has semantics for every header since it
just lets the server write out to the header directly. Otherwise, a header
specification `h ::: ty` has semantics when we have an instance for
`H.HeaderEncode h ty` and/or `H.HeaderDecode h ty` depending on whether we're
interested in reading or writing such a header.

Finally, we can talk about methods which have interesting response *bodies*. So
far, `Method_1` and `Method_2` returned `'Empty` bodies, but we can fix that.

```haskell
import           Data.Text (Text)
import qualified Serv.ContentType as Ct

type RawBody = HasBody '[ Ct.TextPlain ] Text

type Method_3 = Method GET '[ Sc.Ok ::: Respond '[] RawBody ]
```

To specify a body we use the type `'Body ctypes bodyType` which specifies a type
list of *content types* and a Haskell type for which values of provide the data
used to generate the response body. In this case, we use the `"text/plain"`
content type which has semantics for `Text` as there is an instance for
`MimeEncode TextPlain Text` (namely, UTF-8 encoding).

## Methods become APIs

A collection of methods forms an `Endpoint`

```haskell
type Endpoint_1 = Endpoint () '[ Method_1, Method_2, Method_3 ]
```

and Serv ensures that `'Endpoint`s always arise as lists of `'Method`s. We embed
`'Endpoint`s within a tree of other `'Endpoint`s to form a complete `Api`
description. The basic combinators for this are *choice* and *path augmentation*.

The simplest path augmentation is to add a constant path segment in front of an
endpoint

```haskell
{-# LANGUAGE PolyKinds #-}

type Api_1 = Const "users" :> Endpoint_1
```

other path augmentations include *capture* of a particular segment

```haskell
type Api_2 = Const "users" :> Seg "user_id" Int :> Endpoint_1
```

where we interpret the path segment as an `Int` or fail to match this path. We
know how to interpret segments as `Int`s due to instances of
`Serv.URI.URIDecode` or perhaps matching *all* subsequent path segments at once
with a *wildcard*

```haskell
type Api_3 = Wildcard :> Endpoint_1
```

Given a number of path-augmented `'Endpoint`s we'd like to glue these together
into a set of options which is done using `'OneOf`

```haskell
type Api_All =  Const "api"
             :> OneOf '[ Api_1, Api_2, Api_3 ]
```

and from these pieces we can construct API matching trees of whatever shape we
like.

## Servers are described by `Impl`ementations

To write a server against the given `Api` description we construct a value of
type `Impl api m` for the particular `Api` type we have. We can use GHCi's kind
evaluator to see what `Impl Api_All IO` looks like:

```haskell
> :kind! Impl IO Api_All
Impl IO Api_All :: *
= (IO (SomeResponse '[Ok ::: Respond '[] Empty])
   :<|> (IO
           (SomeResponse '[Ok ::: Respond '[Location ::: RawText] Empty])
         :<|> (IO (SomeResponse '[Ok ::: Respond '[] RawBody])
               :<|> Serv.Internal.Server.Type.MethodNotAllowed)))
  :<|> ((Tagged "user_id" Int
         -> IO (SomeResponse '[Ok ::: Respond '[] Empty])
            :<|> (IO
                    (SomeResponse '[Ok ::: Respond '[Location ::: RawText] Empty])
                  :<|> (IO (SomeResponse '[Ok ::: Respond '[] RawBody])
                        :<|> Serv.Internal.Server.Type.MethodNotAllowed)))
        :<|> (([Text]
               -> IO (SomeResponse '[Ok ::: Respond '[] Empty])
                  :<|> (IO
                          (SomeResponse '[Ok ::: Respond '[Location ::: RawText] Empty])
                        :<|> (IO (SomeResponse '[Ok ::: Respond '[] RawBody])
                              :<|> Serv.Internal.Server.Type.MethodNotAllowed)))
              :<|> Serv.Internal.Server.Type.NotFound))
              :<|> IO NotHere))

```

Ugly! But regular! Taking a closer look we can note what's going on here.

First, we note that `Impl IO Api_All IO :: *`. In other words, it's an actual
Haskell *type*: something we can produce values of.

Second, note that there's a big repeated chunk in the middle. This comes from
the fact that we hit the same Endpoint definition three times. Let's refactor by
finding the kind of `Impl Endpoint_1 IO`

```haskell
> :kind! Impl IO Endpoint_1
Impl IO Endpoint_1 :: *
= IO (SomeResponse '['NoContent ::: 'Respond '[] 'Empty])
  :<|> (IO
            (SomeResponse
                         '[NoContent ::: Respond '[Location ::: RawText]
                         Empty])
                                 :<|> (IO (SomeResponse '[NoContent ::: Respond
                                 '[] RawBody])
                                               :<|>
                                               Serv.Internal.Server.Type.MethodNotAllowed))


```

Here we see that we have one `IO (SomeResponse ...)` for each method at our
endpoint. We also have to provide the final `NotFound` server implementation
which is given to us by `noOp :: Monad m => m NotFound` and simply indicates an
automatic `404 Not Found` error.

With this defined, we can give a fast type to `Impl Api_All IO`

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Serv.Server
import Data.Function ((&))

impl_all :: Impl IO Api_All
impl_all = impl_1 :<|> impl_2 :<|> impl_3 :<|> NotFound
  where
    impl_1 = delete :<|> post :<|> get :<|> MethodNotAllowed
    impl_2 _ = delete :<|> post :<|> get :<|> MethodNotAllowed
    impl_3 _ = delete :<|> post :<|> get :<|> MethodNotAllowed

    delete =
      respond 
      $ emptyResponse Sc.SNoContent 
    post = 
      respond
      $ emptyResponse Sc.SNoContent
      & withHeader H.SLocation "example.com"
    get = 
      respond
      $ emptyResponse Sc.SOk
      & withBody "hello"
```

Which lets us more easily see the pattern. Again, we see distinct server choices
separated by `:<|>` and `IO NotFound` to indicate the final, failing server at
the bottom of the stack. We must also describe the endpoint implementations in
various contexts: one for each in the API specification!

In particular, we must implement `Impl_Endpoint` in a null context, in the
context where we have an `Int` which was read in as the `"user_id"`, and finally
in a context where we have a list of `Text` fragments corresponding to all of
the remaining path segments captured by our wildcard.

With this, we can construct a `Server IO` using the `server` function

```haskell
apiProxy :: Sing Api_All
apiProxy = sing

apiServer :: Server IO
apiServer = server apiProxy implAll
```

and transform this `Server` into a normal Wai `Application` using
`makeApplication`

```haskell
import qualified Network.Wai as Wai

application :: Wai.Application
application = makeApplication defaultConfig apiServer
```

# Prior Art

Serv is heavily inspired by the design of
[`servant`](https://hackage.haskell.org/package/servant) which pioneered the
API-description-as-type implementation style. Serv makes several different
tradeoffs, however, as compared to `servant`

- Serv's API language is *kind-restricted* enabling us to be more sure that API
  descriptions make sense and enabling us to write closed type functions over
  API descriptions with greater confidence that the function will not become
  stuck. On the other hand, `servant` has all API descriptions at kind `*` and
  therefore is more easily extensible.

- Serv's API language is also more regular than `servant`'s. Again this is
  helpful for writing type functions to analyze API descriptions since they
  can pattern match on each path, api, and method component in the description.

- Serv's API language includes an explicit notion of an `Endpoint` whereas
  `servant`'s is more implicit (though it could be extended to include this
  idea). Explicit `Endpoint`s enable proper OPTIONS and CORS handling in a way
  that's transparent to you, the library user.

- Serv's `Server` type is more flexible and less tied to Wai's `Application`
  type or `IO`. Ultimately, as is the same with `servant`, one must interpret a
  `Server` in `IO`, but it can be difficult to handle local effects of interest
  using `servant` due to the explicit need to `enter` different effect contexts.
  On the other hand, Serv lets you use whatever effects you like and convert
  them all at once at the very end just before transforming `Server IO` into an
  `Application`.
