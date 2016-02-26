
# `serv-api`, kind-safe types for API descriptions

This package offers kinds and types for constructing “API description types”.
These API description types---technically, types of kind `Api *`---contain much
of the information one needs to understand how to interact with an API. Other
packages in the Serv ecosystem adjoin a little more information to these `Api`
types and then use them to swiftly generate servers, clients, and
documentation.

## A quick overview of `Api`-kinded types

We’ll start from the basic building blocks of `Api` types and move upward.

### `Output`s

The first type of interest is called `Respond hs b`. It corresponds to a
possible response from the server. For example:

```haskell
import Serv.Api.Prelude

type Respond1 = Respond ‘[] Empty
```

indicates a response with no response headers and an empty body. On the other
hand,

```haskell
type Respond2 = 
  Respond 
    ‘[LastModified ::: UTCTime] 
    (Body ‘[JSON, PlainText] User)
```

indicates that the server will respond with a `User` value at either a JSON or
plain text content type (depending on what the client negoatiates for). It also
sets the `Last-Modified` response header to a `UTCTime` value. 

Servers which produce this type must select a `UTCTime` to set as
`Last-Modified` and must ensure that `User`s have encodings in both plain text
and JSON. Clients offer this data back to people and must know how to decode
`User` of at least one of these content types.

### `Handler`s

If you hit a HTTP server at a given endpoint with a given verb it might respond
with any number of possible `Respond` types corresponding to different response
statuses. The `Method` type allows us to describe this situation.

For instance, let’s say that if we `GET` this resource we’ll either get our
`User` value (with a `200` response) or nothing at all (with a `404`) response.
We model this situation like so:

```haskell
type Handler1 =
  Method GET
  ‘[ Ok ::: Respond2
   , NotFound ::: Respond1
   ]
```

In particular, we’ve constructed a `Handler`-kinded type which consists of a
`Verb` specification (in this case `GET`) and `Respond` types corresponding to
every error code which may be returned by the server.

At the handler level we might also suggest that we need more data from the
request. For instance, we have the following types

```haskell
CaptureBody contentTypes ty nextHandler
CaptureHeaders headerSpec nextHandler
CaptureQuery querySpec nextHandler
```

which each modify a handler to capture some further information. For instance,
if our request must specify a `User` in the query then we can represent that
here

```haskell
type Handler2 = CaptureQuery ‘[“id” ::: UserId] Handler1
```

> As a side note we might go ahead and explain this `(:::)` type operator that
> keeps showing up: it’s just syntax sugar for a type-level tuple so that `a :::
> b` is the same as `’(a, b)`. Infix syntax can be convenient for these `Api`
> types, but it’s completely optional.

### `Api`s

We’re finally ready to specify a full `Api`. To do this, we just need to bundle
a few `Handler`s under a specific `Endpoint`. For instance, let’s say we have
some more `Handler`s numbered 3 and 4, here’s our first `Endpoint`

```haskell
type Api1 = Endpoint () ‘[Handler2, Handler3, Handler4]
```

That’s all there is to it! (Ignore the `()` argument. It’s useful later for
annotation and documentation.)

Well, no, because we often want to describe whole `Api`s where there are
choices of `Endpoint`s and they exist at various paths.

```haskell
type Api2 =
  OneOf
  ‘[ Const “user” :> Api1
   , Const “book” :> Api2
   , Const “car”  :> OneOf ‘[ Const “sedan” :> Api3, Const “truck” :> Api4 ]
   [
```

We might also want to capture data from the path as we go down it. For this we
just replace `Const` path segments with `Seg` capture types

```haskell
type Api3 =
  Seg “factory” FactoryId :> Seg “employee” EmployeeId :> ApiFactoryEmployee
```

### Moving on

That’s basically all you need to know to start describing `Api`s! There are
more types available for this purpose, but these are the basics. Feel free to
examine the documentation for `Serv.Api` to learn more.
