                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package telekinesis

import language.dynamics

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import legerdemain.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*
import zephyrine.*

object Http:
  object Version:
    given showable: Version is Showable =
      case 0.9 => t"HTTP/0.9"
      case 1.0 => t"HTTP/1.0"
      case 1.1 => t"HTTP/1.1"
      case 2.0 => t"HTTP/2"
      case 3.0 => t"HTTP/3"

    def parse(text: Text): Version = text match
      case t"HTTP/0.9"             => 0.9
      case t"HTTP/1.1"             => 1.1
      case t"HTTP/2" | t"HTTP/2.0" => 2.0
      case t"HTTP/3" | t"HTTP/3.0" => 3.0
      case _                       => 1.0

  type Version = 0.9 | 1.0 | 1.1 | 2.0 | 3.0

  object Header:
    given encodable: Http.Header is Encodable in Http.Header = identity(_)

  case class Header(key: Text, value: Text)

  object Method:
    given formmethod: ("formmethod" is GenericHtmlAttribute[Method]):
      def name: Text = t"formmethod"
      def serialize(method: Method): Text = method.show

    given method: ("method" is GenericHtmlAttribute[Method]):
      def name: Text = t"method"
      def serialize(method: Method): Text = method.show

    given showable: Method is Showable = _.toString.tt.upper

    given decodable: Method is Decodable in Text = _.upper match
      case t"HEAD"    => Http.Head
      case t"POST"    => Http.Post
      case t"PUT"     => Http.Put
      case t"DELETE"  => Http.Delete
      case t"CONNECT" => Http.Connect
      case t"OPTIONS" => Http.Options
      case t"TRACE"   => Http.Trace
      case t"PATCH"   => Http.Patch
      case t"GET"     => Http.Get
      case _          => Http.Get

  sealed trait Method:
    // `Payload` carries, at the type level, whether this method has a request
    // body (e.g. `Get.Payload =:= false`, `Post.Payload =:= true`). It has no
    // runtime accessor, so — unlike the former `tracked val payload` — it can
    // never throw `AbstractMethodError` when read through the erased `Method`
    // base type (issue #1307). `payload` is the safe runtime counterpart.
    type Payload <: Boolean

    def payload: Boolean = this match
      case Get     => valueOf[Get.Payload]
      case Head    => valueOf[Head.Payload]
      case Post    => valueOf[Post.Payload]
      case Put     => valueOf[Put.Payload]
      case Delete  => valueOf[Delete.Payload]
      case Connect => valueOf[Connect.Payload]
      case Options => valueOf[Options.Payload]
      case Trace   => valueOf[Trace.Payload]
      case Patch   => valueOf[Patch.Payload]

    def unapply(request: Request): Boolean = request.method == this

  case object Get extends Method:
    type Payload = false

  case object Head extends Method:
    type Payload = false

  case object Post extends Method:
    type Payload = true

  case object Put extends Method:
    type Payload = true

  case object Delete extends Method:
    type Payload = false

  case object Connect extends Method:
    type Payload = false

  case object Options extends Method:
    type Payload = false

  case object Trace extends Method:
    type Payload = false

  case object Patch extends Method:
    type Payload = false

  object Status:
    private lazy val all: Map[Int, Status] =
      values.immutable(using Unsafe).bi.map(_.code -> _).to(Map)

    def unapply(code: Int): Option[Status] = all.get(code)

    given communicable: Status is Communicable = status => m"${status.code} (${status.description})"

    enum Category:
      case Informational, Successful, Redirection, ClientError, ServerError

  enum Status(val code: Int, val description: Text):
    case Continue                      extends Status(100, t"Continue")
    case SwitchingProtocols            extends Status(101, t"Switching Protocols")
    case EarlyHints                    extends Status(103, t"Early Hints")
    case Ok                            extends Status(200, t"OK")
    case Created                       extends Status(201, t"Created")
    case Accepted                      extends Status(202, t"Accepted")
    case NonAuthoritativeInformation   extends Status(203, t"Non-Authoritative Information")
    case NoContent                     extends Status(204, t"No Content")
    case ResetContent                  extends Status(205, t"Reset Content")
    case PartialContent                extends Status(206, t"Partial Content")
    case MultipleChoices               extends Status(300, t"Multiple Choices")
    case MovedPermanently              extends Status(301, t"Moved Permanently")
    case Found                         extends Status(302, t"Found")
    case SeeOther                      extends Status(303, t"See Other")
    case NotModified                   extends Status(304, t"Not Modified")
    case TemporaryRedirect             extends Status(307, t"Temporary Redirect")
    case PermanentRedirect             extends Status(308, t"Permanent Redirect")
    case BadRequest                    extends Status(400, t"Bad Request")
    case Unauthorized                  extends Status(401, t"Unauthorized")
    case PaymentRequired               extends Status(402, t"Payment Required")
    case Forbidden                     extends Status(403, t"Forbidden")
    case NotFound                      extends Status(404, t"Not Found")
    case MethodNotAllowed              extends Status(405, t"Method Not Allowed")
    case NotAcceptable                 extends Status(406, t"Not Acceptable")
    case ProxyAuthenticationRequired   extends Status(407, t"Proxy Authentication Required")
    case RequestTimeout                extends Status(408, t"Request Timeout")
    case Conflict                      extends Status(409, t"Conflict")
    case Gone                          extends Status(410, t"Gone")
    case LengthRequired                extends Status(411, t"Length Required")
    case PreconditionFailed            extends Status(412, t"Precondition Failed")
    case PayloadTooLarge               extends Status(413, t"Payload Too Large")
    case UriTooLong                    extends Status(414, t"URI Too Long")
    case UnsupportedMediaType          extends Status(415, t"Unsupported Media Type")
    case RangeNotSatisfiable           extends Status(416, t"Range Not Satisfiable")
    case ExpectationFailed             extends Status(417, t"Expectation Failed")
    case UnprocessableEntity           extends Status(422, t"Unprocessable Entity")
    case TooEarly                      extends Status(425, t"Too Early")
    case UpgradeRequired               extends Status(426, t"Upgrade Required")
    case PreconditionRequired          extends Status(428, t"Precondition Required")
    case TooManyRequests               extends Status(429, t"Too Many Requests")
    case RequestHeaderFieldsTooLarge   extends Status(431, t"Request Header Fields Too Large")
    case UnavailableForLegalReasons    extends Status(451, t"Unavailable For Legal Reasons")
    case InternalServerError           extends Status(500, t"Internal Server Error")
    case NotImplemented                extends Status(501, t"Not Implemented")
    case BadGateway                    extends Status(502, t"Bad Gateway")
    case ServiceUnavailable            extends Status(503, t"Service Unavailable")
    case GatewayTimeout                extends Status(504, t"Gateway Timeout")
    case HttpVersionNotSupported       extends Status(505, t"HTTP Version Not Supported")
    case VariantAlsoNegotiates         extends Status(506, t"Variant Also Negotiates")
    case InsufficientStorage           extends Status(507, t"Insufficient Storage")
    case LoopDetected                  extends Status(508, t"Loop Detected")
    case NotExtended                   extends Status(510, t"Not Extended")
    case NetworkAuthenticationRequired extends Status(511, t"Network Authentication Required")

    def category: Status.Category = (code/100).absolve match
      case 1 => Http.Category.Informational
      case 2 => Http.Category.Successful
      case 3 => Http.Category.Redirection
      case 4 => Http.Category.ClientError
      case 5 => Http.Category.ServerError

    def apply(headers: List[Header], body: Body): Response = Response.response(this, headers, body)

  export Status.*

  object Request:
    given showable: Request is Showable = request =>
      val bodySample: Text =
        try request.body().memoize.utf8 catch case error: StreamError  => t"[-/-]"

      val headers: Text =
        request.textHeaders.map: header =>
          t"${header.key}: ${header.value}"

        . join(t"\n          ")

      val params: Text =
        request.query.values.map: (key, value) =>
          t"$key = \"$value\""

        . join(t"\n          ")

      ListMap[Text, Text](
        t"content" ->
          ( safely(request.headers.contentType.prim.or(media"application/octet-stream").show)
            . or(t"?") ),
        t"method"   -> request.method.show,
        t"query"    -> request.query.show,
        t"hostname" -> request.host.show,
        t"path"     -> request.location,
        t"body"     -> bodySample,
        t"headers"  -> headers,
        t"params"   -> params
      ).map { case (key, value) => t"$key = $value" }.join(t", ")

    // Serialize the request to its HTTP/1.1 wire form: the request line, `Host`
    // and framing headers, then the header block and body. Used by transports
    // that write directly to a socket (e.g. the `coaxial` domain-socket client
    // in `telekinesis.jvm`, which wraps this in a `Transmissible`).
    def serialize(request: Request): LazyList[Data] =
      import charEncoders.asciiEncoder

      val text: Text = Text.build:
        def newline(): Unit = append(t"\r\n")
        append(request.method.show)
        append(t" ")
        append(request.target)
        append(t" ")
        append(Http.Version.showable.text(request.version))
        newline()
        append(t"Host: ")
        append(request.host.show)
        newline()

        request.body().lazyList match
          case LazyList()     => append(t"Content-Length: 0")
          case LazyList(data) => append(t"Content-Length: ${data.length}")
          case _              => append(t"Transfer-Encoding: chunked")

        request.textHeaders.map: parameter =>
          newline()
          append(parameter.key)
          append(t": ")
          append(parameter.value)

        newline()
        newline()

      text.data #:: request.body().lazyList

    case class Head
      ( method: Method, version: Version, host: Host, target: Text, headers: List[Header] )

    // Parse a request-line (`METHOD SP target SP HTTP-version CRLF`) and the
    // header block off `cursor`, leaving it positioned at the first byte of
    // the message body. The symmetric twin of `Http.Response.parse`'s
    // status-line + header parsing; factored out so a server driving a single
    // cursor across a keep-alive connection can frame the body itself. Scans
    // with `peek`/`next` rather than `seek` so it works on a bare `Cursor[Data, ?]`
    // parameter (which loses the `tracked` `Operand = Byte` refinement that
    // `seek`'s signature relies on).
    // `maxRequestLine` and `maxHeaders` bound how many bytes the request line and
    // the header block may occupy, yielding `414`/`431` (rather than reading an
    // unbounded amount) — the scan aborts mid-token once the cap is crossed.
    def parseHead
      ( cursor: Cursor[Data, {}]^, maxRequestLine: Int = 8192, maxHeaders: Int = 65536 )
    :   Head raises HttpRequestError =

      import HttpRequestError.Reason

      inline def expected(char: Char): Diagnostics ?=> HttpRequestError =
        HttpRequestError(Reason.Expectation(char, cursor.peek.asInt.toChar))

      def upTo(stop: Char, limit: Int, reason: Reason): Text = cursor.hold:
        val start = cursor.mark

        while !cursor.finished && !(cursor.peek == stop) do
          if cursor.position.n0 > limit then abort(HttpRequestError(reason))
          cursor.next()

        Ascii(cursor.grab(start, cursor.mark)).show

      val lineLimit = cursor.position.n0 + maxRequestLine

      val method: Http.Method = upTo(' ', lineLimit, Reason.UriTooLong).decode[Http.Method]
      cursor.next()

      val target: Text = upTo(' ', lineLimit, Reason.UriTooLong)
      cursor.next()

      val version: Http.Version = Http.Version.parse(upTo('\r', lineLimit, Reason.UriTooLong))
      cursor.next()
      cursor.expect('\n')(expected('\n'))

      val headerLimit = cursor.position.n0 + maxHeaders

      def readHeaders(headers: List[Http.Header]): List[Http.Header] =
        if cursor.position.n0 > headerLimit then abort(HttpRequestError(Reason.HeadersTooLarge))

        if cursor.peek == '\r' then
          // Consume the final CRLF with `advance` rather than `next`/`expect`:
          // `next` calls `more`, which forces a blocking refill of the underlying
          // stream. That is fatal when a request has no body (e.g. a `GET`) and
          // the client is already waiting for our response — there are no more
          // bytes to read. `advance` steps past the terminator without reading
          // ahead, leaving the cursor at the first byte of the body (if any).
          cursor.advance()
          if !(cursor.peek == '\n') then raise(expected('\n'))
          cursor.advance()
          headers

        else
          val key: Text = upTo(':', headerLimit, Reason.HeadersTooLarge)
          cursor.next()

          while cursor.peek == ' ' || cursor.peek == '\t' do cursor.next()

          val value: Text = upTo('\r', headerLimit, Reason.HeadersTooLarge)
          cursor.next()
          cursor.expect('\n')(expected('\n'))
          readHeaders(Http.Header(key, value) :: headers)

      val headers = readHeaders(Nil).reverse

      val hostText: Optional[Text] = headers.filter(_.key.lower == t"host").prim.let(_.value)

      val host: Host = hostText.lay(abort(HttpRequestError(HttpRequestError.Reason.Host(t"")))):
        text =>
          safely(text.decode[Host]).or:
            safely(text.cut(t":").prim.or(text).decode[Host]).or:
              abort(HttpRequestError(HttpRequestError.Reason.Host(text)))

      Head(method, version, host, target, headers)

    def parse(stream: LazyList[Data])(using Tactic[HttpRequestError]): Request^ =
      val cursor = Cursor[Data](stream.filter(_.nonEmpty).iterator)
      val head = parseHead(cursor)

      Request
        ( head.method,
          head.version,
          head.host,
          head.target,
          head.headers,
          () => Stream(cursor.remainder.iterator) )

    // LazyList exactly `length` bytes of body off `cursor`, in buffer-sized
    // pieces, leaving it at the first byte after the body (the start of the next
    // pipelined request on a kept-alive connection). Uses `advance` rather than
    // `next`, so — like `parseHead` — it never reads past the body and so never
    // blocks waiting for bytes that will not arrive until the client has our
    // response.
    def fixedBody(cursor: Cursor[Data, {}]^, length: Int): LazyList[Data] =
      def recur(remaining: Int): LazyList[Data] =
        if remaining <= 0 || cursor.finished then LazyList() else
          val take = remaining.min(cursor.available)

          val chunk = cursor.hold:
            val start = cursor.mark
            var index = 0

            while index < take do
              cursor.advance()
              index += 1

            cursor.grab(start, cursor.mark)

          chunk #:: recur(remaining - take)

      LazyList.defer(recur(length))

    // Decode a `Transfer-Encoding: chunked` request body off `cursor`, yielding
    // each chunk's data and leaving the cursor after the terminating `0`-chunk
    // and trailers — i.e. at the next request. Lenient: a malformed length or a
    // truncated stream simply ends the body. Consumes CRLFs with `advance` (not
    // `next`), so it never reads past the body's final `\r\n` (see `parseHead`).
    def chunkedBody(cursor: Cursor[Data, {}]^): LazyList[Data] =
      def hex(digit: Int): Int =
        if digit >= '0' && digit <= '9' then digit - '0'
        else if digit >= 'a' && digit <= 'f' then digit - 'a' + 10
        else if digit >= 'A' && digit <= 'F' then digit - 'A' + 10
        else -1

      def consumeCrlf(): Unit =
        if cursor.peek == '\r' then cursor.advance()
        if cursor.peek == '\n' then cursor.advance()

      def skipToCrlf(): Unit = while !(cursor.peek == '\r') && !cursor.finished do cursor.advance()

      def readSize(): Int =
        var size = 0
        var continue = true

        while continue do
          val value = hex(cursor.peek.asInt)

          if value < 0 then continue = false else
            size = size*16 + value
            cursor.advance()

        skipToCrlf() // discard any chunk extension
        consumeCrlf()
        size

      def recur(): LazyList[Data] = LazyList.defer:
        if cursor.finished then LazyList() else
          val size = readSize()

          if size <= 0 then
            while !(cursor.peek == '\r') && !cursor.finished do // trailer header lines
              skipToCrlf()
              consumeCrlf()

            consumeCrlf() // final blank line
            LazyList()

          else
            val data = cursor.hold:
              val start = cursor.mark
              var index = 0

              while index < size && !cursor.finished do
                cursor.advance()
                index += 1

              cursor.grab(start, cursor.mark)

            consumeCrlf()
            data #:: recur()

      recur()

  enum Body:
    case Streaming(data: LazyList[Data])
    case Flowing(source: Spring[Data]^)
    case Fixed(data: Data)
    case Empty

    def stream: LazyList[Data] = this match
      case Body.Fixed(data)       => LazyList(data)
      case Body.Empty             => LazyList()
      case Body.Streaming(stream) => stream
      case Body.Flowing(source)   =>
        val flowing = source()
        flowing.lazyList


  // A request body with no bytes; each call constructs a fresh, already-empty
  // pull endpoint, matching the re-materializable contract of `body` thunks.
  def emptyBody(): (Stream[Data] over Credit)^ = Stream(Iterator.empty[Data])

  class Request
    ( val method:      Http.Method,
      val version:     Http.Version,
      val host:        Host,
      val target:      Text,
      val textHeaders: List[Http.Header],
      val body:        Spring[Data]^ ):

    inline def request: this.type = this

    // `Www`'s `Radical` always succeeds, so decoding the path cannot fail.
    lazy val path: Path on Www under %.type =
      unsafely(location.decode[Path on Www under %.type])

    def on[scheme <: "http" | "https"](origin: Origin[scheme]): HttpUrl =
      Url[scheme](origin, target)

    private lazy val queryText: Text =
      target.offsetOf(t"?").lay(t""): ordinal => target.skip(ordinal.n0 + 1)

    lazy val query: Query =
      contentType.let(_.base.show) match
        case t"application/x-www-form-urlencoded" =>
          queryText.decode[Query] ++ body().memoize.utf8.decode[Query]

        case _ =>
          queryText.decode[Query]

    lazy val location: Text =
      target.offsetOf(t"?").lay(target): ordinal => target.keep(ordinal.n0)

    object headers extends Dynamic:
      def selectDynamic(name: Label)
        ( using directive: name.type is Directive,
                decoder:   (directive.Topic is Decodable in Text)^ )
      :   List[directive.Topic] =

        val name2 = name.tt.uncamel.kebab.lower
        textHeaders.filter(_.key.lower == name2).map(_.value.decode)

    lazy val contentType: Optional[MediaType] = safely(headers.contentType.prim)

    lazy val textCookies: Map[Text, Text] =
      headers.cookie.flatMap: cookie =>
        cookie.bi.map(_.name -> _.value)

      . to(Map)


  // The swappable transport that physically sends a single request and returns
  // its response. The URL is fully resolved (passed as `Text`) so non-JVM
  // backends can parse it themselves; redirects are handled by `HttpClient`, not
  // the backend. Backends are platform-specific, so each is summoned by an
  // explicit import: `httpBackends.virtualMachine` (`java.net.http`, in
  // `telekinesis.jvm`) on the JVM; other platforms or implementations (e.g.
  // HTTP/2) supply their own given.
  trait Backend:
    def request
      ( url:     Text,
        method:  Http.Method,
        headers: List[Http.Header],
        body:    Spring[Data]^ )
      ( using Tactic[ConnectError] )
    :   Http.Response

  object Response extends Dynamic:
    transparent inline def applyDynamicNamed(id: "apply")(inline headers: (Label, Any)*)
    :   Protoresponse | Response =

      ${telekinesis.internal.response('headers)}


    given conversion: [servable: Servable] => Conversion[servable, Response] = servable.serve(_)

    transparent inline def applyDynamic(id: "apply")(inline headers: Any*)
    :   Protoresponse | Response =

      ${telekinesis.internal.response('headers)}

    case class Protoresponse(status0: Optional[Status], headers: Seq[Header]):
      def apply(body: Body = Body.Empty): Response =
        Response(1.1, status0.or(Ok), headers.to(List), body)

      def apply[servable: Servable](body: servable): Response =
        val response = servable.serve(body)

        Response
          ( 1.1,
            status0.or(response.status),
            headers.to(List) ++ response.textHeaders,
            response.body )

    given streamable: (tactic: Tactic[HttpError])
    =>  ((Response is Streamable by Data)^{tactic}) = response =>
      response.status.category match
        case Http.Status.Category.Successful => response.body.stream

        case _ =>
          abort(HttpError(response.status, response.textHeaders))

    private[Http] def response(status: Status, headers: List[Header], body: Body): Response =
      new Response(1.1, status, headers, body)

    // Serialise a response to HTTP/1.1 wire bytes: status line, headers (with an
    // automatic `Content-Length` for fixed/empty bodies, or `Transfer-Encoding:
    // chunked` framing for streaming bodies), the blank line, then the framed
    // body. `includeBody` is `false` for responses to `HEAD` requests and for
    // `101` upgrades (where the caller pipes the post-handshake stream raw). The
    // inverse of `parse`.
    def serialize(response: Response, includeBody: Boolean = true, version: Version = 1.1)
    :   LazyList[Data] =

      import charEncoders.asciiEncoder

      // After `101 Switching Protocols` the bytes are no longer HTTP: the body is
      // the upgraded protocol's raw stream (e.g. WebSocket frames), so suppress
      // Content-Length / chunked framing and the headers that announce them.
      val upgrade: Boolean = response.status == Http.SwitchingProtocols

      // Chunked transfer-encoding is an HTTP/1.1 feature; a streaming body to an
      // HTTP/1.0 client must instead be delimited by closing the connection (the
      // server adds `Connection: close`), so its body is written raw.
      val chunkable: Boolean = version != 1.0 && version != 0.9

      val explicitChunked: Boolean = response.textHeaders.exists: header =>
        header.key.lower == t"transfer-encoding" && header.value.lower == t"chunked"

      val hasContentLength: Boolean = response.textHeaders.exists(_.key.lower == t"content-length")

      val (extraHeaders, chunked): (List[Header], Boolean) =
        if upgrade then (Nil, false) else response.body match
          case Body.Empty =>
            (if hasContentLength then Nil else List(Header(t"content-length", t"0")), false)

          case Body.Fixed(data) =>
            val length = data.length.toString.tt
            (if hasContentLength then Nil else List(Header(t"content-length", length)), false)

          case Body.Streaming(_) | Body.Flowing(_) =>
            if explicitChunked && chunkable then (Nil, true)
            else if hasContentLength then (Nil, false)
            else if chunkable then (List(Header(t"transfer-encoding", t"chunked")), true)
            else (Nil, false)

      val headers: List[Header] =
        if !upgrade then response.textHeaders ++ extraHeaders else
          response.textHeaders.filter: header =>
            val key = header.key.lower
            key != t"transfer-encoding" && key != t"content-length"

      val head: Text = Text.build:
        append(t"HTTP/1.1 ")
        append(response.status.code.toString.tt)
        append(t" ")
        append(response.status.description)
        append(t"\r\n")

        headers.each: header =>
          append(header.key)
          append(t": ")
          append(header.value)
          append(t"\r\n")

        append(t"\r\n")

      def chunkedFraming(stream: LazyList[Data]): LazyList[Data] = stream match
        case block #:: tail =>
          if block.length == 0 then chunkedFraming(tail) else
            val size: Text = Integer.toHexString(block.length).nn.tt
            t"$size\r\n".data #:: block #:: t"\r\n".data #:: chunkedFraming(tail)

        case _ =>
          LazyList(t"0\r\n\r\n".data)

      def bodyBytes: LazyList[Data] =
        if !includeBody then LazyList() else if upgrade then response.body.stream
        else response.body match
          case Body.Empty             => LazyList()
          case Body.Fixed(data)       => LazyList(data)
          case Body.Streaming(stream) => if chunked then chunkedFraming(stream) else stream

          case Body.Flowing(source) =>
            val stream = source().lazyList
            if chunked then chunkedFraming(stream) else stream

      head.data #:: bodyBytes

    def parse(stream: LazyList[Data]): Response raises HttpResponseError =
      val cursor = Cursor[Data](stream.filter(_.nonEmpty).iterator)

      inline def expected(char: Char): Diagnostics ?=> HttpResponseError =
        HttpResponseError(HttpResponseError.Reason.Expectation(char, cursor.peek.asInt.toChar))

      val version: Http.Version = cursor.hold:
        val start = cursor.mark
        cursor.expect('H')(expected('H'))
        cursor.expect('T')(expected('T'))
        cursor.expect('T')(expected('T'))
        cursor.expect('P')(expected('P'))
        cursor.expect('/')(expected('/'))
        cursor.seek(' '.toByte.asInstanceOf[cursor.addressable.Operand])
        Http.Version.parse(Ascii(cursor.grab(start, cursor.mark)).show)

      cursor.next()

      val code: Int = cursor.hold:
        val start = cursor.mark
        val d1 = cursor.peek

        if d1.asInt < '1' || d1.asInt > '5' then
          cursor.next()
          cursor.next()

          abort:
            HttpResponseError:
              HttpResponseError.Reason.Status(Ascii(cursor.grab(start, cursor.mark)).show)

        var code = d1.asInt - '0'
        cursor.next()
        val d2 = cursor.peek

        if d2.asInt < '0' || d2.asInt > '9' then
          cursor.next()

          abort:
            HttpResponseError
              ( HttpResponseError.Reason.Status(Ascii(cursor.grab(start, cursor.mark)).show) )

        code = code*10 + (d2.asInt - '0')
        cursor.next()
        val d3 = cursor.peek

        if d3.asInt < '0' || d3.asInt > '9' then
          abort:
            HttpResponseError
              ( HttpResponseError.Reason.Status(Ascii(cursor.grab(start, cursor.mark)).show) )

        code*10 + (d3.asInt - '0')

      cursor.next()
      cursor.expect(' ')(expected(' '))

      val status = Http.Status.unapply(code).optional.or:
        abort(HttpResponseError(HttpResponseError.Reason.Status(code.toString.tt)))

      cursor.seek('\r'.toByte.asInstanceOf[cursor.addressable.Operand])
      cursor.next()
      cursor.expect('\n')(expected('\n'))

      def readHeaders(headers: List[Http.Header]): List[Http.Header] =
        if cursor.peek == '\r' then
          cursor.next()
          cursor.expect('\n')(expected('\n'))
          headers

        else
          val header: Text = cursor.hold:
            val start = cursor.mark
            cursor.seek(':'.toByte.asInstanceOf[cursor.addressable.Operand])
            Ascii(cursor.grab(start, cursor.mark)).show

          cursor.next()

          while cursor.peek == ' ' || cursor.peek == '\t'
          do cursor.next()

          val value: Text = cursor.hold:
            val start = cursor.mark
            cursor.seek('\r'.toByte.asInstanceOf[cursor.addressable.Operand])
            Ascii(cursor.grab(start, cursor.mark)).show

          cursor.next()
          cursor.expect('\n')(expected('\n'))
          readHeaders(Http.Header(header, value) :: headers)

      val headers = readHeaders(Nil)

      val body = Http.Body.Streaming(cursor.remainder)

      Response(version, status, headers.reverse, body)

  into case class Response private
    ( version: Version, status: Status, textHeaders: List[Header], body: Body )
  extends Dynamic:

    def updateDynamic[label <: Label: Directive of topic, topic](name: label)(value: topic)
    :   Response =

      val key2 = name.tt.uncamel.kebab.lower
      copy(textHeaders = Header(key2, label.encode(value)) :: textHeaders.filter(_.key != key2))


    def successBody: Optional[LazyList[Data]] =
      if status.category != Http.Status.Category.Successful then Unset else body.stream


    def receive[body](using receivable: (body is Receivable)^): body =
      receivable.read(this)

    object headers extends Dynamic:
      def selectDynamic(name: Label)
        ( using directive: name.type is Directive,
                decoder:   (directive.Topic is Decodable in Text)^ )
      :   List[directive.Topic] =

        val name2 = name.tt.uncamel.kebab.lower
        textHeaders.filter(_.key.lower == name2).map(_.value.decode)


    @targetName("add")
    infix def + [value: Encodable in Http.Header](value: value): Response =
      val header: Http.Header = value.encode
      copy(textHeaders = header :: textHeaders)

  case class Submit[target](originForm: Text, target: target, host: Host)
  extends Dynamic:
    inline def applyDynamicNamed[payload](id: "apply")(inline headers: (Label, Any)*)
      ( payload: payload )
      ( using online:   Online,
              loggable: HttpEvent is Loggable,
              postable: payload is Postable,
              client:   HttpClient onto target )
    :   Http.Response =

      $ {
          ( telekinesis.internal.submit[target, payload]
              ( 'this, 'headers, 'online, 'loggable, 'payload, 'postable, 'client ) )
        }


    inline def applyDynamic[payload: Postable as postable](id: "apply")(inline headers: Any*)
      ( payload: payload )
      ( using online:   Online,
              loggable: HttpEvent is Loggable,
              client:   HttpClient onto target )
    :   Http.Response =

      $ {
          ( telekinesis.internal.submit[target, payload]
              ( 'this, 'headers, 'online, 'loggable, 'payload, 'postable, 'client ) )
        }


  case class Fetch[target](originForm: Text, target: target, host: Host)
    extends Dynamic:

    inline def applyDynamicNamed(id: "apply")(inline headers: (Label, Any)*)
      ( using online:   Online,
              loggable: HttpEvent is Loggable,
              postable: Unit is Postable,
              client:   HttpClient onto target )
    :   Http.Response =

      ${telekinesis.internal.fetch('this, 'headers, 'online, 'loggable, 'client)}


    inline def applyDynamic[payload](id: "apply")(inline headers: Any*)
      ( using online:   Online,
              loggable: HttpEvent is Loggable,
              client:   HttpClient onto target )
    :   Http.Response =

      ${telekinesis.internal.fetch('this, 'headers, 'online, 'loggable, 'client)}

sealed trait Http
