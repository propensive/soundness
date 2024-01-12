Here is an example of a simple HTTP request:

```scala
import telekinesis.*

val response = url"http://example.com/test".query(flag = t"yes", param = t"7").get().as[Text]
```

### Sending an HTTP request

An HTTP request may be sent by calling one of the HTTP methods—`get`, `post`, `put`, `options`, `head`, `trace`,
`delete`, `connect` or `patch`—on the `Http` object. As a minimum, these methods all take a URL as their first
parameter. This may be provided as a `Url` (see below) or a `Text`, or any type which has a contextual
`ToLocation` instance which can convert it into URL string. This may be useful for integration with alternitave
URL representations.

If the request is successful, a response will be returned synchronously as an `HttpResponse` instance.
`HttpResponse` provides the methods `status` (the HTTP status code), `headers` (a map of HTTP response headers),
and `body` which will be a representation of the response body, in bytes.

The easiest way to access the body is by converting it to another type, using a contextual reader. That can be
achieved by calling `as` with an appropriate type, for example,
```scala
url"https://example.com/service".get().as[Text]
```
or with a suitable JSON library such as [Jacinta](https://propensive.com/opensource/jacinta/),
```scala
import jacinta.*
url"http://example.com/file".post(content).as[Json]
```

### Request and response bodies

The type of `body` is `Body`, defined as an alias for, `Unit | IArray[Byte] | LazyList[IArray[Byte]]`, a union
type corresponding to the cases of an empty response, a response of known length, and a streamed response,
respectively.

This type is commonly used for both requests and responses.

### Error handling

HTTP requests may fail for a variety of reasons. These will be thrown as `HttpError`s only when the `as` method
is invoked (an `HttpResponse` is always returned from `get` or `post`, even in the event of a failure status).
An `HttpError` contains a `status` field of the HTTP status code.

Some HTTP requests will fail, but will still send a useful response body which can be read and interpreted like
any other, albeit from the `HttpError` instance.

Here is an example of an HTTP error being handled:

```scala
try uri.get().as[Text]
catch
  case error@HttpError(HttpStatus.NotFound, _) =>
    t"The page was not found. The server responded with: ${error.as[Text]}"
  case HttpError(_, _) =>
    t"The request failed"
```




