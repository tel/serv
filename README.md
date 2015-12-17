
# Serv

*Dependently typed API servers, clients, and documentation.*

[![Build Status](https://travis-ci.org/tel/serv.svg)](https://travis-ci.org/tel/serv)

Serv separates the description of an API from the implementation of the server,
the client definitions, and the documentation. Unlike other such efforts, the
API descriptions are *types* and type level computation can be used to ensure
that the server, client, and documentation *always match*.

# Tutorial

## Describing HTTP Methods

The most basic component of an API is the `Method`. We begin describing APIs by
listing the `Method`s they respond to. To do this, we define a set of types. For
instance, a simple API might look like

```haskell
{-# LANGUAGE DataKinds, TypeOperators #-}

import Serv.Api

type Method_1 = 'Method 'DELETE '[] 'Empty
```

where the single-quotes indicate that we are using a data constructor at the
type-level. For instance, Serv defines the type `data Verb = GET | PUT | DELETE
| ...` but GHC's `DataKinds` extension allows us to treat this also as a set of
types `'GET`, `'PUT`, `'DELETE`, etc. which all have *kind* `Verb`. Serv uses
`DataKinds` extensively to enable kind-safe type-level programming in the same
way that Haskell normally enables type-safe value-level programming.

Above, `AMethod` is a type describing a single method, a `DELETE` method which
produces an empty response with no special headers. We use a type-level list
`'[]` to describe the set of headers returned.

Another method might return a particular header:

```haskell
import           Serv.Common (RawText)
import qualified Serv.Header as H

type Method_2 = 'Method 'POST '[ 'H.Location '::: RawText ] 'Empty
```

Here, `Method_2` must return the `Location` header specified by some `RawText`
value. We use `(:::)` to link `H.HeaderName`s to Haskell types which *have
semantics for* that header. `RawText` has semantics for every header since it
just lets the server write out to the header directly. Otherwise, a header
specification `h '::: ty` has semantics when we have an instance for
`H.HeaderEncode h ty` and/or `H.HeaderDecode h ty` depending on whether we're
interested in reading or writing such a header.

Finally, we can talk about methods which have interesting response *bodies*. So
far, `Method_1` and `Method_2` returned `'Empty` bodies, but we can fix that.

```haskell
import           Data.Text (Text)
import qualified Serv.ContentType as Ct

type Method_3 = 'Method 'GET '[] ('Body '[ Ct.TextPlain ] Text)
```

To specify a body we use the type `'Body ctypes bodyType` which specifies a type
list of *content types* and a Haskell type for which values of provide the data
used to generate the response body. In this case, we use the `"text/plain"`
content type which has semantics for `Text` as there is an instance for
`MimeEncode TextPlain Text` (namely, UTF-8 encoding).

## Methods become APIs

A collection of methods forms an `Endpoint`

```haskell
type Endpoint_1 = 'Endpoint '[ Method_1, Method_2, Method_3 ]
```

and Serv ensures that `'Endpoint`s always arise as lists of `'Method`s. We embed
`'Endpoint`s within a tree of other `'Endpoint`s to form a complete `Api`
description. The basic combinators for this are *choice* and *path augmentation*.

The simplest path augmentation is to add a constant path segment in front of an
endpoint

```haskell
type Api_1 = 'Const "users" ':> Endpoint_1
```

other path augmentations include *capture* of a particular segment

```haskell
type Api_2 = 'Const "users" ':> 'Seg "user_id" Int ':> Endpoint_1
```

where we interpret the path segment as an `Int` or fail to match this path. We
know how to interpret segments as `Int`s due to instances of
`Serv.URI.URIDecode` or perhaps matching *all* subsequent path segments at once
with a *wildcard*

```haskell
type Api_3 = 'Wildcard :> Endpoint_1
```

Given a number of path-augmented `'Endpoint`s we'd like to glue these together
into a set of options which is done using `'OneOf`

```haskell
type Api_All =   'Const "api"
             ':> 'OneOf '[ Api_1, Api_2, Api_3 ]
```

and from these pieces we can construct API matching trees of whatever shape we
like.

## Servers are described by `Impl`ementations

To write a server against the given `Api` description we construct a value of
type `Impl api m` for the particular `Api` type we have. We can use GHCi's kind
evaluator to see what `Impl Api_All IO` looks like:

```haskell
> :kind Impl Api_All IO
Impl Api_All IO :: *
= (IO (Response '[] 'Empty)
   :<|> (IO (Response '['H.Location '::: RawText] 'Empty)
         :<|> (IO (Response '[] ('Body '[Ct.TextPlain] Text))
               :<|> IO NotHere)))
  :<|> ((Tagged "user_id" Int
         -> IO (Response '[] 'Empty)
            :<|> (IO (Response '['H.Location '::: RawText] 'Empty)
                  :<|> (IO (Response '[] ('Body '[Ct.TextPlain] Text))
                        :<|> IO NotHere)))
        :<|> (([Text]
               -> IO (Response '[] 'Empty)
                  :<|> (IO (Response '['H.Location '::: RawText] 'Empty)
                        :<|> (IO (Response '[] ('Body '[Ct.TextPlain] Text))
                              :<|> IO NotHere)))
              :<|> IO NotHere))
```

Ugly! But regular! Taking a closer look we can note what's going on here.

First, we note that `Impl Api_All IO :: *`. In other words, it's an actual
Haskell *type*: something we can produce values of.

Second, note that there's a big repeated chunk in the middle. This comes from
the fact that we hit the same Endpoint definition three times. Let's refactor by
finding the kind of `Impl Endpoint_1 IO`

```haskell
type Impl_Endpoint =
       IO (Response '[] 'Empty)
  :<|> IO (Response '['H.Location '::: RawText] 'Empty)
  :<|> IO (Response '[] ('Body '[Ct.TextPlain] Text))
  :<|> IO NotHere
```

Here we see that we have one `IO (Response ...)` for each method at our
endpoint. We also have to provide the final `NotHere` server implementation
which is given to us by `noOp :: Monad m => m NotHere` and simply indicates an
automatic `404 Not Found` error.

With this defined, we can give a fast type to `Impl Api_All IO`

```haskell
type Impl_All =
       Impl_Endpoint
  :<|> Tagged "user_id" Int -> Impl_Endpoint
  :<|> [Text] -> Impl_Endpoint
  :<|> IO NotHere
```

Which lets us more easily see the pattern. Again, we see distinct server choices
separated by `:<|>` and `IO NotHere` to indicate the final, failing server at
the bottom of the stack. We must also describe the endpoint implementations in
various contexts: one for each in the API specification!

In particular, we must implement `Impl_Endpoint` in a null context, in the
context where we have an `Int` which was read in as the `"user_id"`, and finally
in a context where we have a list of `Text` fragments corresponding to all of
the remaining path segments captured by our wildcard.

With this, we can construct a `Server IO` using the `handle` function

```haskell
server :: Server IO
server = handle (Proxy :: Proxy Api_All) (myImplementation :: Impl_All)
```

and transform this `Server` into a normal Wai `Application` using
`makeApplication`

```haskell
import qualified Network.Wai as Wai

application :: Wai.Application
application = makeApplication defaultConfig server
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
