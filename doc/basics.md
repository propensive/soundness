### Launching an HTTP server

An HTTP server can be launched anywhere by calling,
```scala
HttpServer.listen:
  // handler
```

This is a non-blocking call whose body will be executed every time a request is received. The `listen` method
returns an `HttpService` instance whose only method is `HttpService#stop()`, which will cause the server to stop
listening for new requests.

The `listen` block must return a `Response` instance. A `Response` may be instantiated with a single parameter
of the content to be returned. In this case, its `Content-Type` and `Content-Length` would be determined from
the type of the parameter, as well as the HTTP status (which is usually `200`, except in failure cases) and
how its body is sent: all at once, or streamed.

The simplest sever implementation would look something like this,
```scala
HttpServer.listen:
  Response("Hello world!")
```
and would respond with a `200` response with the MIME type `text/plain`, and the string `Hello world!` for every
request, regardless of its HTTP method, parameters, body or headers.

Within the body of `listen`, a `Request` instance is contextually available, and may be accessed with the
`request` method. For convenience, the methods `param` and `header` may also be used directly within a `listen`
block to access a parameter or HTTP header, for example:
```scala
HttpServer.listen:
  val name = param("name")
  val age = param("age")

  Response(s"The name is $name and age is $age")
```

### Pattern matching on Requests

Another way to work with `Request`s is by pattern matching against them. Several pattern extractors are provided
for this purpose.

A very simple pattern match on a request object might look like this,
```scala
HttpServer.listen:
  request match
    case Path("/")                   => homePage
    case Path("/contact")            => contactUsPage
    case Path(s"/products/$product") => productPage(product)
```
where the `Path` extractor is used to match on the part of the URL after the hostname, and before the query, if
there is one.

But other extractors for matching on the HTTP method, headers and parameters also exist, and can be combined in
the same pattern using the `&` combinator, like so:
```scala
HttpServer.listen:
  request match
    case Path("/") & RequestHeader.UserAgent(s"Mozilla/$_")               => mozillaHome
    case Path("/") & Post() & AcceptEncoding(lang) if lang.contains("en") => englishHome
```

In these examples, `&` is an extractor which always matches a `Request`, and "extracts" it into two copies. As
an infix extractor, both sides may then be matched with their own patterns. Of course, this can be repeated
any number of times in the same case clause.

### URLs

Scintillate uses the `Url` type to represent a URL. This will always use either the `http` or `https` URL
scheme, and will not represent URLs containing unescaped characters. The `hostname`, `path` and `queryString`
are also available or `Url` instances.

Additionally, the HTTP methods `get`, `post`, and others are available to call directly on `Url` instances, as
an alternative to their equivalent methods in the `Http` object. There is a one-to-one correspondence between
the methods on a `Url` instance an the `Http` object, except that the first parameter of each of the `Http`
methods—the URL itself—is the subject of the method invocation.

#### Queries

A `Url` instance may include a query string, which would be written following a `?` character after the path.
The `Url#query` method may be used to append parameters to an existing `Url`. Usually these are key/value pairs
in the form `key=value`, but plain strings can also be used.

There are two ways to call the `query` method. Firstly, it may be invoked with variadic dynamically-named
arguments, and `String` parameters, like so:
```scala
url.query(param = "one", value = "two", flag = "three", option = "four")
```
the names of these parameters may be any valid Scala identifier, and do not need to be quoted.

Another variant of the `query` method exists which takes a single parameter of a type that can be interpreted
as a set of parameters, based on a contextual `ToQuery` instance.

Default given `ToQuery` instances are provided for the primitive types, `String` and `Int`, and instances will
be derived for case classes composed of other types for which `ToQuery` instances exist. That includes nested
case classes.

A case class will generate one query parameter for each field, named after that field, except for fields of
nested case class instances: In these cases, each field in the nested type will be prefixed with the outer field
name, separated by a `.`.

For example,
```scala
case class Address(number: Int, street: String, city: String)
case class Person(name: String, address: Address)

val person = Person("Jack", Address(17, "East Street", "Birmingham"))
url"http://example.com/person/add".query(person).get()
```

The process of serializing this case class instance to query parameters would send the parameters, `name`,
`address.number`, `address.street` and `address.city`.

### Redirection and Missing Pages

The `Redirect` and `NotFound` case classes provide representations of an HTTP `301` redirect repsonse and a
`404` "not found" page respectively.

`Redirect` takes a single parameter, a representation of a location typically as a `Url` or a `String`, but
other representations can be used provided a `ToLocation` for that type is in contextual scope.


