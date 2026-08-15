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
┃    Soundness, version 0.64.0.                                                                    ┃
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

import scala.caps

import beneficence.*
import proscenium.compat.*

import scala.language.dynamics

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
import java.net as jn
import scala.util.NotGiven

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

    // The fields that may legitimately appear more than once in a message. Most may not, and a
    // recipient may treat a repeated singleton field as malformed (RFC 9110 §5.3). Two kinds may:
    // list-based fields, where repeating a field means the same as sending one comma-joined value,
    // and `Set-Cookie`, which *must* be repeated rather than joined, because a cookie's value can
    // itself contain a comma (RFC 6265 §3).
    private val repeatableFields: Set[Text] =
      Set
       ( t"accept", t"accept-charset", t"accept-encoding", t"accept-language", t"accept-patch",
         t"accept-ranges", t"access-control-allow-headers", t"access-control-allow-methods",
         t"access-control-expose-headers", t"access-control-request-headers", t"allow", t"alt-svc",
         t"cache-control", t"clear-site-data", t"connection", t"content-encoding",
         t"content-language", t"expect", t"forwarded", t"if-match", t"if-none-match", t"link",
         t"pragma", t"prefer", t"preference-applied", t"proxy-authenticate", t"set-cookie", t"te",
         t"trailer", t"transfer-encoding", t"upgrade", t"vary", t"via", t"warning",
         t"www-authenticate" )

    // Whether a field may appear more than once. Field names are case-insensitive, so the key is
    // lowered before lookup. An unknown field is treated as a singleton: that is the safe default,
    // since duplicating a singleton is a protocol error while collapsing a repeat of an unlisted
    // list-based field only loses a value the caller has explicitly replaced.
    def repeatable(key: Text): Boolean = repeatableFields.stdlib.contains(key.lower)

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
    // `values` is the enum's cached array, so this is an alias of shared state rather than
    // owned material -- but it is consumed here and now, into a `Map`, and nothing writes to
    // an enum's `values`.
    private lazy val all: Map[Int, Status] =
      Map.from(Array.unsafeFrozen(values).readable.bi.map(_.code -> _))

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

    def apply(headers: List[Header], body: Body^): Response^{body} =
      Response.response(this, headers, body)

  export Status.*

  object Request:
    given showable: Request is Showable = request =>
      val bodySample: Text =
        try request.body().memoize.utf8 catch case error: StreamError  => t"[-/-]"

      val headers: Text =
        request.textHeaders.map: (header: Header) =>
          t"${header.key}: ${header.value}"

        . join(t"\n          ")

      val params: Text =
        request.query.values.map: (key, value) => t"$key = \"$value\""
        . join(t"\n          ")

      Ledger[Text, Text](
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
      ).to[List].map { (key, value) => t"$key = $value" }.join(t", ")

    // Serialize the request to its HTTP/1.1 wire form: the request line, `Host`
    // and framing headers, then the header block and body, as a fresh pull
    // endpoint. Used by transports that write directly to a socket (e.g. the
    // `coaxial` domain-socket client in `telekinesis.jvm`, which wraps this in
    // a `Transmissible`). The body is probed one block at a time: a body that
    // ends within the first block is framed with `Content-Length`; anything
    // longer streams with chunked transfer encoding.
    def serialize(request: Request)(using buffering: Buffering)
    :   (Stream[Data] over Credit)^ =

      import charEncoders.asciiEncoder

      val endpoint = request.body()
      val block = buffering.capacity(Substrate.Bytes)

      // A zero count (an empty upstream chunk) is retried, never yielded: a
      // zero-length frame would terminate chunked transfer encoding early. A
      // while-loop rather than self-recursion: a def capturing the exclusive
      // endpoint may not call itself under the statement rule.
      def pull(): Optional[Data] =
        var result: Optional[Data] = Unset
        var continue = true

        while continue do
          endpoint.refill(Credit(block)) match
            case 0 => ()

            case count: Int =>
              result =
                endpoint.lend { region => range => region.materialize(range.capped(count)) }

              endpoint.skip(count)
              continue = false

            case _ =>
              result = Unset
              continue = false

        result

      val first: Optional[Data] = pull()
      val second: Optional[Data] = if first.absent then Unset else pull()

      def head(framing: Text): Text = Text.build:
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
        append(framing)

        request.textHeaders.map: parameter =>
          newline()
          append(parameter.key)
          append(t": ")
          append(parameter.value)

        newline()
        newline()

      def frame(data: Data): Iterator[Data] =
        Iterator(t"${Integer.toHexString(data.length).nn.tt}\r\n".in[Data], data, t"\r\n".in[Data])

      (first, second) match
        case (first: Data, second: Data) =>
          val text = head(t"Transfer-Encoding: chunked")

          Stream
            ( Iterator(text.in[Data])
              ++ frame(first)
              ++ frame(second)
              ++ Iterator.continually(pull()).takeWhile(_.present).flatMap(_.lay(Iterator())(frame))
              ++ Iterator(t"0\r\n\r\n".in[Data]) )

        case _ =>
          val data = first.or(Array.empty[Byte])
          val text = head(t"Content-Length: ${data.length}")

          Stream:
            if data.length == 0 then Iterator(text.in[Data]) else Iterator(text.in[Data], data)

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
    // The last successfully-parsed `Host` header, memoized across requests; see the
    // comment at its use in `parseHead`. Untracked: an immutable pair behind a benign
    // read-mostly race, not a capability-bearing state.
    @scala.caps.unsafe.untrackedCaptures @volatile
    private var hostMemo: (Text, Host) | Null = null

    // `maxRequestLine` and `maxHeaders` bound how many bytes the request line and
    // the header block may occupy, yielding `414`/`431` (rather than reading an
    // unbounded amount) — the scan aborts mid-token once the cap is crossed.
    def parseHead
      ( cursor: Cursor[Data, {}]^, maxRequestLine: Int = 8192, maxHeaders: Int = 65536 )
      ( using Tactic[Http.Request.Error] )
    :   Head =

      import Http.Request.Error.Reason

      inline def expected(char: Char): Diagnostics ?=> Http.Request.Error =
        Http.Request.Error(Reason.Expectation(char, cursor.peek.asInt.toChar))

      def upTo(stop: Char, limit: Int, reason: Reason): Text = cursor.hold:
        val start = cursor.mark

        while !cursor.finished && !(cursor.peek == stop) do
          if cursor.position.n0 > limit then abort(Http.Request.Error(reason))
          cursor.next()

        Ascii(cursor.grab(start, cursor.mark)).show

      val lineLimit = cursor.position.n0 + maxRequestLine

      val method: Http.Method = upTo(' ', lineLimit, Reason.UriTooLong).as[Http.Method]
      cursor.next()

      val target: Text = upTo(' ', lineLimit, Reason.UriTooLong)
      cursor.next()

      val version: Http.Version = Http.Version.parse(upTo('\r', lineLimit, Reason.UriTooLong))
      cursor.next()
      cursor.expect('\n')(expected('\n'))

      val headerLimit = cursor.position.n0 + maxHeaders

      def readHeaders(headers: List[Http.Header]): List[Http.Header] =
        if cursor.position.n0 > headerLimit then abort(Http.Request.Error(Reason.HeadersTooLarge))

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

      val hostText: Optional[Text] =
        headers.filter(_.key.s.equalsIgnoreCase("host")).prim.let(_.value)

      val host: Host = hostText.lay(abort(Http.Request.Error(Http.Request.Error.Reason.Host(t"")))):
        text =>
          // The typed parse of the union (`Hostname | Ipv4 | Ipv6`, trying each form) is
          // the costliest step of parsing a request, yet every request on a keep-alive
          // connection repeats the same bytes, and host cardinality per server is tiny.
          // Only a successful parse is memoized, so a missing or invalid host still
          // aborts here; a stale memo under concurrency merely reparses.
          hostMemo match
            case (cached, host0) if cached == text => host0
            case _ =>
              val parsed =
                safely(text.as[Host]).or:
                  safely(text.cut(t":").prim.or(text).as[Host]).or:
                    abort(Http.Request.Error(Http.Request.Error.Reason.Host(text)))

              hostMemo = (text, parsed)
              parsed

      Head(method, version, host, target, headers)

    def parse(stream: Chain[Data])(using Tactic[Http.Request.Error]): Request^ =
      val cursor = Cursor[Data](stream.filter(_.nonEmpty).iterator)
      val head = parseHead(cursor)

      Request
        ( head.method,
          head.version,
          head.host,
          head.target,
          head.headers,
          () => cursor.remainder.iterator.stream )

    // The endpoint form: the request parses straight off the connection's pull
    // endpoint. The body spring lends the cursor's remainder as a SINGLE-OWNER
    // stream: each mint resumes from wherever the previous reader stopped, and
    // explicit `memoize` replaces the Chain form's implicit caching.
    def parse(consume input: (Stream[Data] over Credit)^)(using Tactic[Http.Request.Error])
    :   Request^ =

      val cursor = Cursor[Data](input)
      val head = parseHead(cursor)

      // Sealed like `Response.parse`: the cursor is single-owner and reachable
      // only through the spring; a capturing `Spring` would cascade `^` through
      // every `Request` value. The neutral carrier keeps the spring's result
      // from naming the non-local cursor (the `-scalajs` row enforces the
      // hiding rule where the JVM row does not).
      val cursorRef: AnyRef = cursor.asInstanceOf[AnyRef]

      val spring: Spring[Data]^ =
        () => streamOf(cursorRef.asInstanceOf[Cursor[Data, {}]^])

      Request
        ( head.method,
          head.version,
          head.host,
          head.target,
          head.headers,
          caps.unsafe.unsafeAssumePure(spring) )

    // Exactly `length` bytes of body, lent zero-copy off `cursor` (which stays
    // open, positioned at the first byte after the body — the start of the next
    // pipelined request on a kept-alive connection). It never reads past the
    // body, so it never blocks waiting for bytes that will not arrive until the
    // client has our response.
    def fixedBody(cursor: Cursor[Data, {}]^, length: Int)
    :   (Stream[Data] over Credit)^{cursor, caps.any} =

      streamOf(cursor, length)

    // Decode a `Transfer-Encoding: chunked` request body off `cursor`, lending
    // each chunk's data zero-copy and leaving the cursor after the terminating
    // `0`-chunk and trailers — i.e. at the next request. Lenient: a malformed
    // length or a truncated stream simply ends the body. Consumes CRLFs with
    // `advance` (not `next`), so it never reads past the body's final `\r\n`
    // (see `parseHead`).
    def chunkedBody(cursor: Cursor[Data, {}]^)
    :   (Stream[Data] over Credit)^{cursor, caps.any} =

      new Stream[Data]:
        type Transport = Credit

        // Bytes left in the current chunk's data; -1 before its size is read.
        private var remaining: Int = -1
        private var ended: Boolean = false

        // Snapshot of the cursor's buffer, taken by `refill` (the `streamOf`
        // discipline: only update methods access the exclusive cursor).
        private var storage: AnyRef = ""
        private var start0: Int = 0
        private var limit0: Int = 0

        protected def storage0: AnyRef = storage
        def start: Int = start0
        def limit: Int = limit0

        update def skip(count: Int): Unit =
          remaining -= count
          start0 += count
          cursor.unsafeAdvanceBy(count)(using Unsafe)

        private def hex(digit: Int): Int =
          if digit >= '0' && digit <= '9' then digit - '0'
          else if digit >= 'a' && digit <= 'f' then digit - 'a' + 10
          else if digit >= 'A' && digit <= 'F' then digit - 'A' + 10
          else -1

        private update def consumeCrlf(): Unit =
          if cursor.peek == '\r' then cursor.advance()
          if cursor.peek == '\n' then cursor.advance()

        private update def skipToCrlf(): Unit =
          while !(cursor.peek == '\r') && !cursor.finished do cursor.advance()

        private update def readSize(): Int =
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

        update def refill(demand: Credit): Optional[Int] =
          if !ended then
            // The previous chunk's data is fully consumed: its trailing CRLF,
            // then the next chunk's size line.
            if remaining == 0 then
              consumeCrlf()
              remaining = -1

            if remaining < 0 then
              if cursor.finished then ended = true else
                val size = readSize()

                if size <= 0 then
                  while !(cursor.peek == '\r') && !cursor.finished do // trailers
                    skipToCrlf()
                    consumeCrlf()

                  consumeCrlf() // final blank line
                  ended = true
                else remaining = size

          if ended then Unset
          else if cursor.more then
            storage = cursor.unsafeBuffer(using Unsafe).asInstanceOf[AnyRef]
            start0 = cursor.unsafePos(using Unsafe)
            val available = cursor.unsafeWriteEnd(using Unsafe) - start0
            val readable = remaining.min(available)
            limit0 = start0 + readable
            readable
          else
            ended = true
            Unset

    // HttpRequestError → Http.Request.Error
    object Error:
      enum Reason(val number: Int) extends Clarification:
        case Expectation(expected: Char, found: Char) extends Reason(1)
        case Version(value: Text)                     extends Reason(2)
        case Host(value: Text)                        extends Reason(3)
        case UriTooLong                               extends Reason(4)
        case HeadersTooLarge                          extends Reason(5)

      given communicable: Reason is Communicable =
        case Reason.Expectation(expected, found) => m"$found was found when $expected was expected"
        case Reason.Version(value)               => m"the HTTP version $value was invalid"
        case Reason.Host(value)                  => m"the host $value was missing or invalid"
        case Reason.UriTooLong                   => m"the request line exceeded the maximum length"
        case Reason.HeadersTooLarge              => m"the request headers exceeded the maximum size"

    case class Error(reason: Http.Request.Error.Reason)(using Diagnostics)
    extends fulminate.Error(367, reason.number)(m"could not parse HTTP request because $reason")

  enum Body:
    case Flowing(source: Spring[Data]^)
    case Fixed(data: Data)
    case Empty

    // A fresh pull endpoint over this body's content: mintable repeatedly for
    // `Fixed`/`Empty`, and per the `Spring` contract for `Flowing`.
    def stream: (Stream[Data] over Credit)^ = this match
      case Body.Fixed(data)     => data.stream
      case Body.Empty           => Iterator.empty[Data].stream
      case Body.Flowing(source) => source()

  // A request body with no bytes; each call constructs a fresh, already-empty
  // pull endpoint, matching the re-materializable contract of `body` thunks.
  def emptyBody(): (Stream[Data] over Credit)^ = Iterator.empty[Data].stream

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
      unsafely(location.as[Path on Www under %.type])

    def on[scheme <: "http" | "https"](origin: Origin[scheme]): HttpUrl =
      Url[scheme](origin, target)

    private lazy val queryText: Text =
      target.offsetOf(t"?").lay(t""): ordinal => target.skip(ordinal.n0 + 1)

    lazy val query: Query =
      contentType.let(_.base.show) match
        case t"application/x-www-form-urlencoded" =>
          queryText.as[Query] ++ body().memoize.utf8.as[Query]

        case _ =>
          queryText.as[Query]

    lazy val location: Text =
      target.offsetOf(t"?").lay(target): ordinal => target.keep(ordinal.n0)

    object headers extends Dynamic:
      def selectDynamic(name: Label)
        ( using directive: name.type is Directive,
                decoder:   (directive.Topic is Decodable in Text)^ )
      :   List[directive.Topic] =

        val name2 = name.tt.uncamel.kebab.lower
        textHeaders.filter(_.key.lower == name2).map(_.value.as)

    lazy val contentType: Optional[MediaType] = safely(headers.contentType.prim)

    lazy val textCookies: Map[Text, Text] = Map.from:
      headers.cookie.stdlib.flatMap: (cookie: List[Cookie.Value]) =>
        cookie.stdlib.map { value => value.name -> value.value }

  // The swappable transport that physically sends a single request and returns
  // its response. The URL is fully resolved (passed as `Text`) so non-JVM
  // backends can parse it themselves; redirects are handled by `Http.Client`, not
  // the backend. Backends are platform-specific, so each is summoned by an
  // explicit import: `httpBackends.virtualMachineHttp` (`java.net.http`, in
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

    case class Protoresponse(status0: Optional[Status], headers: List[Header]):
      def apply(body: Body^ = Body.Empty): Response^{body} =
        Response(1.1, status0.or(Ok), headers, body)

      def apply[servable: Servable](body: servable): Response =
        val response = servable.serve(body)

        // A header named at the call site *overrides* the one the `Servable` derives for the
        // same field, rather than joining it: `Response(Ok, contentType = …)(text)` would
        // otherwise put `content-type` on the wire twice, once from the call site and once
        // from `Text`'s `Servable`, and a recipient may treat a repeated singleton field as
        // malformed. Only singleton fields are overridden, though — a repeatable field such as
        // `Set-Cookie` keeps both, since repeating it is how more than one value is expressed.
        // Comparison is case-insensitive, as field names are.
        val derived = response.textHeaders.stdlib.filterNot: header =>
          !Header.repeatable(header.key)
          && headers.stdlib.exists(_.key.lower == header.key.lower)

        Response
          ( 1.1,
            status0.or(response.status),
            List.of(headers.stdlib ++ derived),
            // `serve` returns a pure `Response`, so its body is pure; the seal only
            // discharges the field's capture-polymorphic declared type.
            caps.unsafe.unsafeAssumePure(response.body) )

    given streamable: (tactic: Tactic[Http.Error])
    =>  ((Response is Streamable by Data over Credit)^{tactic}) = response =>
      response.status.category match
        case Http.Status.Category.Successful => response.body.stream

        case _ =>
          abort(Http.Error(response.status, response.textHeaders))

    private[Http] def response(status: Status, headers: List[Header], body: Body^)
    :   Response^{body} =
      new Response(1.1, status, headers, body)

    // Serialise a response to HTTP/1.1 wire bytes: status line, headers (with an
    // automatic `Content-Length` for fixed/empty bodies, or `Transfer-Encoding:
    // chunked` framing for streaming bodies), the blank line, then the framed
    // body. `includeBody` is `false` for responses to `HEAD` requests and for
    // `101` upgrades (where the caller pipes the post-handshake stream raw). The
    // inverse of `parse`.
    def serialize(response: Response^, includeBody: Boolean = true, version: Version = 1.1)
      ( using buffering: Buffering )
    :   (Stream[Data] over Credit)^ =

      import charEncoders.asciiEncoder

      // After `101 Switching Protocols` the bytes are no longer HTTP: the body is
      // the upgraded protocol's raw stream (e.g. WebSocket frames), so suppress
      // Content-Length / chunked framing and the headers that announce them.
      val upgrade: Boolean = response.status == Http.SwitchingProtocols

      // Chunked transfer-encoding is an HTTP/1.1 feature; a streaming body to an
      // HTTP/1.0 client must instead be delimited by closing the connection (the
      // server adds `Connection: close`), so its body is written raw.
      val chunkable: Boolean = version != 1.0 && version != 0.9

      // Case-insensitive on the raw strings: `.lower` would allocate a fresh `Text` per
      // header per response on this hot path.
      val explicitChunked: Boolean = response.textHeaders.exists: header =>
        header.key.s.equalsIgnoreCase("transfer-encoding")
          && header.value.s.equalsIgnoreCase("chunked")

      val hasContentLength: Boolean =
        response.textHeaders.exists(_.key.s.equalsIgnoreCase("content-length"))

      val (extraHeaders, chunked): (List[Header], Boolean) =
        if upgrade then (Nil, false) else response.body match
          case Body.Empty =>
            (if hasContentLength then Nil else List(Header(t"content-length", t"0")), false)

          case Body.Fixed(data) =>
            val length = data.length.toString.tt
            (if hasContentLength then Nil else List(Header(t"content-length", length)), false)

          case Body.Flowing(_) =>
            if explicitChunked && chunkable then (Nil, true)
            else if hasContentLength then (Nil, false)
            else if chunkable then (List(Header(t"transfer-encoding", t"chunked")), true)
            else (Nil, false)

      val headers: List[Header] =
        if !upgrade then List.of(response.textHeaders.stdlib ++ extraHeaders.stdlib) else
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

      // Materialize successive blocks off a pull endpoint as a chunk iterator.
      def pulls(endpoint: (Stream[Data] over Credit)^): Iterator[Data]^{endpoint} =
        val block = buffering.capacity(Substrate.Bytes)

        Iterator.continually:
          // See `Request.serialize`: empty chunks are retried, never framed, and
          // the retry is a loop, not self-recursion.
          var result: Optional[Data] = Unset
          var continue = true

          while continue do
            endpoint.refill(Credit(block)) match
              case 0 => ()

              case count: Int =>
                result =
                  endpoint.lend { region => range => region.materialize(range.capped(count)) }

                endpoint.skip(count)
                continue = false

              case _ =>
                result = Unset
                continue = false

          result

        . takeWhile(_.present).flatMap(_.lay(Iterator())(Iterator(_)))
      def frame(data: Data): Iterator[Data] =
        Iterator(t"${Integer.toHexString(data.length).nn.tt}\r\n".in[Data], data, t"\r\n".in[Data])

      def bodyBytes: Iterator[Data]^ =
        if !includeBody then Iterator.empty
        else if upgrade then pulls(response.body.stream)
        else response.body match
          case Body.Empty       => Iterator.empty
          case Body.Fixed(data) => Iterator(data)

          case Body.Flowing(source) =>
            if chunked then pulls(source()).flatMap(frame) ++ Iterator(t"0\r\n\r\n".in[Data])
            else pulls(source())

      // Hoisted: a by-name `++` operand may not mint a fresh capability.
      val chunks = bodyBytes
      Stream(Iterator(head.in[Data]) ++ chunks)

    def parse(stream: Chain[Data], bodiless: Boolean = false)
    :   Response raises Http.Response.Error =

      parseCursor(Cursor[Data](stream.filter(_.nonEmpty).iterator), bodiless)

    // The endpoint form: the response is parsed straight off the connection's pull
    // endpoint, with no lazy-list view. The tactic is a plain using-parameter: a
    // context-function result may not hide the consumed endpoint.
    def parse(consume input: (Stream[Data] over Credit)^)(using Tactic[Http.Response.Error])
    :   Response =

      parseCursor(Cursor[Data](input), false)

    // As above, with `bodiless` marking a response to a `HEAD` request, which
    // repeats the `GET` framing headers but carries no body (RFC 7230 §3.3.3).
    // (A separate overload: only one variant may have default arguments.)
    def parse(consume input: (Stream[Data] over Credit)^, bodiless: Boolean)
      ( using Tactic[Http.Response.Error] )
    :   Response =

      parseCursor(Cursor[Data](input), bodiless)

    case class Head(version: Version, status: Status, headers: List[Header])

    // Parse a status line (`HTTP-version SP status-code SP reason CRLF`) and the
    // header block off `cursor`, leaving it positioned at the first byte of the
    // message body. The symmetric twin of `Request.parseHead`; factored out so a
    // client driving a single cursor across a kept-alive connection can frame
    // the body itself (e.g. an `Http.Session` lending streaming bodies).
    def parseHead(cursor: Cursor[Data, {}]^)(using Tactic[Http.Response.Error]): Head =
      inline def expected(char: Char): Diagnostics ?=> Http.Response.Error =
        Http.Response.Error(Http.Response.Error.Reason.Expectation(char, cursor.peek.asInt.toChar))

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
            Http.Response.Error:
              Http.Response.Error.Reason.Status(Ascii(cursor.grab(start, cursor.mark)).show)

        var code = d1.asInt - '0'
        cursor.next()
        val d2 = cursor.peek

        if d2.asInt < '0' || d2.asInt > '9' then
          cursor.next()

          abort:
            Http.Response.Error
              ( Http.Response.Error.Reason.Status(Ascii(cursor.grab(start, cursor.mark)).show) )

        code = code*10 + (d2.asInt - '0')
        cursor.next()
        val d3 = cursor.peek

        if d3.asInt < '0' || d3.asInt > '9' then
          abort:
            Http.Response.Error
              ( Http.Response.Error.Reason.Status(Ascii(cursor.grab(start, cursor.mark)).show) )

        code*10 + (d3.asInt - '0')

      cursor.next()
      cursor.expect(' ')(expected(' '))

      val status = Http.Status.unapply(code).optional.or:
        abort(Http.Response.Error(Http.Response.Error.Reason.Status(code.toString.tt)))

      cursor.seek('\r'.toByte.asInstanceOf[cursor.addressable.Operand])
      cursor.next()
      cursor.expect('\n')(expected('\n'))

      // A while-loop rather than a recursive def: a def capturing the locally
      // bound exclusive cursor may not call itself under the statement rule.
      var headers: List[Http.Header] = Nil
      var reading = true

      while reading do
        if cursor.peek == '\r' then
          // Consume the head's final CRLF with `advance` rather than `next`:
          // `next` calls `more`, which forces a blocking refill of the
          // underlying stream. That is fatal for a bodiless response (204,
          // 304, 1xx, or an answer to `HEAD`) on a kept-alive connection: the
          // server sends nothing after the head until our next request, so
          // the refill would deadlock. The symmetric hazard to the server
          // side's `Http.Request.parseHead`; see issue #1301.
          cursor.advance()
          cursor.expect('\n')(expected('\n'))
          reading = false

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

          cursor.advance()
          cursor.expect('\n')(expected('\n'))
          headers = Http.Header(header, value) :: headers

      Head(version, status, headers.reverse)

    private def parseCursor(consume cursor: Cursor[Data, {}]^, bodiless: Boolean)
      ( using Tactic[Http.Response.Error] )
    :   Response =

      val head: Head = parseHead(cursor)
      val version: Http.Version = head.version
      val status: Http.Status = head.status
      val code: Int = status.code
      val headerList: List[Http.Header] = head.headers

      // The body is framed by its headers, per RFC 7230 §3.3.3, leaving the
      // cursor at the first byte after it — the start of the next response on a
      // kept-alive connection. `Transfer-Encoding: chunked` decodes chunks
      // (taking precedence over any `Content-Length`); a `Content-Length` bounds
      // a fixed run; a response that can never carry a body — one answering a
      // `HEAD` request (signalled by `bodiless`), or with a 1xx/204/304 status —
      // ends at the header block. `101 Switching Protocols` is the exception:
      // its remainder is the upgraded protocol's raw stream, lent whole. With no
      // framing header at all, the body is delimited by connection close, as
      // before.
      val chunked: Boolean = headerList.exists: header =>
        header.key.lower == t"transfer-encoding" && header.value.lower.contains(t"chunked")

      val length: Optional[Int] =
        headerList.filter(_.key.lower == t"content-length").prim.let(_.value)
        . lay(Unset: Optional[Int]): text =>
            safely(Integer.parseInt(text.s.trim.nn))

      // The body spring lends the framed view as a SINGLE-OWNER stream: each
      // mint resumes where the previous reader stopped. A chunked or fixed
      // framing is stateful (mid-chunk position, remaining length), so it is
      // minted once and the same endpoint re-lent; the unframed remainder is
      // position-resuming by construction. Sealed: the cursor is reachable only
      // through the spring, and the client API keeps `Response` pure (the honest
      // capturing form is reserved for server-side handler results). Neutral
      // carrier: see `Request.parse`.
      val cursorRef: AnyRef = cursor.asInstanceOf[AnyRef]

      def remainder(): Http.Body =
        val spring: Spring[Data]^ =
          () => streamOf(cursorRef.asInstanceOf[Cursor[Data, {}]^])

        Http.Body.Flowing(caps.unsafe.unsafeAssumePure(spring))

      def framed(fixed: Optional[Int]): Http.Body =
        var stream0: Optional[AnyRef] = Unset

        val spring: Spring[Data]^ = () =>
          if stream0 == Unset then
            stream0 =
              fixed.lay(Request.chunkedBody(cursorRef.asInstanceOf[Cursor[Data, {}]^])):
                length => Request.fixedBody(cursorRef.asInstanceOf[Cursor[Data, {}]^], length)

              . asInstanceOf[AnyRef]

          stream0.asInstanceOf[(Stream[Data] over Credit)^]

        Http.Body.Flowing(caps.unsafe.unsafeAssumePure(spring))

      val cannotHaveBody: Boolean =
        bodiless || code == 204 || code == 304 || (code >= 100 && code < 200 && code != 101)

      val body: Http.Body =
        if code == 101 then remainder()
        else if cannotHaveBody then Http.Body.Empty
        else if chunked then framed(Unset)
        else
          length.lay(remainder()): length =>
            if length <= 0 then Http.Body.Empty else framed(length)

      Response(version, status, headerList, body)

    // HttpResponseError → Http.Response.Error
    object Error:
      enum Reason(val number: Int) extends Clarification:
        case Expectation(expected: Char, found: Char) extends Reason(1)
        case Status(value: Text)                      extends Reason(2)

      given communicable: Reason is Communicable =
        case Reason.Expectation(expected, found) => m"$found was found when $expected was expected"
        case Reason.Status(value)                => m"the HTTP status code $value was invalid"

    case class Error(reason: Http.Response.Error.Reason)(using Diagnostics)
    extends fulminate.Error(366, reason.number)(m"could not parse HTTP response because $reason")

  // The body is capture-polymorphic (`Body^`): a server-side streamed response
  // legitimately retains the live connection it answers — the handler shape
  // `(connection: Http.Connection) ?=> Http.Response^{connection}` — while every
  // client-facing `Response` remains pure.
  into case class Response private
    ( version: Version, status: Status, textHeaders: List[Header], body: Body^ )
  extends Dynamic:

    def updateDynamic[label <: Label: Directive of topic, topic](name: label)(value: topic)
    :   Response^{this} =

      val key2 = name.tt.uncamel.kebab.lower

      // Explicit construction rather than `copy`: the synthesized `copy`'s
      // capture-polymorphic `body` formal is fresh, so its `this.body` default
      // would hide a capability the prefix also captures (separation).
      new Response
        ( version,
          status,
          Header(key2, label.encode(value)) :: textHeaders.filter(_.key != key2),
          body )


    // The successful response's body as a single-owner pull endpoint (explicit
    // `memoize` replaces the former implicit whole-body caching).
    def successBody: Optional[(Stream[Data] over Credit)^] =
      if status.category != Http.Status.Category.Successful then Unset
      else body.stream

    def receive[body](using receivable: (body is Receivable)^): body =
      receivable.read(this)

    object headers extends Dynamic:
      def selectDynamic(name: Label)
        ( using directive: name.type is Directive,
                decoder:   (directive.Topic is Decodable in Text)^ )
      :   List[directive.Topic] =

        val name2 = name.tt.uncamel.kebab.lower
        textHeaders.filter(_.key.lower == name2).map(_.value.as)


    @targetName("add")
    infix def + [value: Encodable in Http.Header](value: value): Response^{this} =
      val header: Http.Header = value.encode

      // Explicit construction rather than `copy`: see `updateDynamic`.
      new Response(version, status, header :: textHeaders, body)

  case class Submit[target](originForm: Text, target: target, host: Host)
  extends Dynamic:
    inline def applyDynamicNamed[payload](id: "apply")(inline headers: (Label, Any)*)
      ( payload: payload )
      ( using online:   Online,
              loggable: (Http.Event is Loggable)^,
              postable: (payload is Postable)^,
              client:   Http.Client onto target )
    :   Http.Response =

      $ {
          ( telekinesis.internal.submit[target, payload]
              ( 'this, 'headers, 'online, 'loggable, 'payload, 'postable, 'client ) )
        }


    inline def applyDynamic[payload](id: "apply")(inline headers: Any*)
      ( payload: payload )
      ( using online:   Online,
              loggable: (Http.Event is Loggable)^,
              postable: (payload is Postable)^,
              client:   Http.Client onto target )
    :   Http.Response =

      $ {
          ( telekinesis.internal.submit[target, payload]
              ( 'this, 'headers, 'online, 'loggable, 'payload, 'postable, 'client ) )
        }


  case class Fetch[target](originForm: Text, target: target, host: Host)
    extends Dynamic:

    inline def applyDynamicNamed(id: "apply")(inline headers: (Label, Any)*)
      ( using online:   Online,
              loggable: (Http.Event is Loggable)^,
              postable: (Unit is Postable)^,
              client:   Http.Client onto target )
    :   Http.Response =

      ${telekinesis.internal.fetch('this, 'headers, 'online, 'loggable, 'client)}


    inline def applyDynamic[payload](id: "apply")(inline headers: Any*)
      ( using online:   Online,
              loggable: (Http.Event is Loggable)^,
              client:   Http.Client onto target )
    :   Http.Response =

      ${telekinesis.internal.fetch('this, 'headers, 'online, 'loggable, 'client)}

  // HttpClient → Http.Client
  object Client:
    // Log a received response at a level reflecting its status: a server error is a `Fail`, a client
    // error a `Warn`, and anything else (informational, success, redirect) routine `Fine` detail.
    private def logResponse(response: Http.Response): Http.Response =
      response.status.category match
        case Http.Status.Category.ServerError => Log.fail(Http.Event.Response(response.status))
        case Http.Status.Category.ClientError => Log.warn(Http.Event.Response(response.status))
        case _                                => Log.fine(Http.Event.Response(response.status))

      response

    // 301/302/303 historically downgrade the method to GET and drop the body;
    // 307/308 preserve both. Matches Java's `Redirect.NORMAL` and the WHATWG
    // fetch spec.
    private def redirectMethod(code: Int, original: Http.Method): Http.Method = code match
      case 301 | 302 | 303 => Http.Get
      case _               => original

    private def isRedirect(code: Int): Boolean = code match
      case 301 | 302 | 303 | 307 | 308 => true
      case _                           => false

    given httpStrict: (tactic: Tactic[ConnectError])
    =>  Online
    =>  Redirects.Disabled
    =>  ( backend: Http.Backend )
    =>  ( (Http.Client { type Target = Origin["http" | "https"] })^{tactic, caps.any} ) =
      new Http.Client:
        type Target = Origin["http" | "https"]

        def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
          ( using (Http.Event is Loggable)^ )
        :   Http.Response =

          val url = httpRequest.on(origin)
          Log.info(Http.Event.Send(httpRequest.method, url, httpRequest.textHeaders))

          logResponse:
            backend.request(url.show, httpRequest.method, httpRequest.textHeaders, httpRequest.body)

    given http: (tactic: Tactic[ConnectError])
    =>  Online
    =>  NotGiven[Redirects.Disabled]
    =>  ( redirection: Http.Redirection )
    =>  ( backend: Http.Backend )
    =>  ( (Http.Client { type Target = Origin["http" | "https"] })^{tactic, caps.any} ) =
      new Http.Client:
        type Target = Origin["http" | "https"]

        def request(httpRequest: Http.Request, origin: Origin["http" | "https"])
          ( using (Http.Event is Loggable)^ )
        :   Http.Response =

          val url = httpRequest.on(origin)
          Log.info(Http.Event.Send(httpRequest.method, url, httpRequest.textHeaders))

          // The spring crosses the recursion as a neutral reference: a
          // capability-typed binding of a parameter would hide it from the
          // recursive call under the statement rule.
          def loop(uri: jn.URI, method: Http.Method, bodyRef: AnyRef, remaining: Int)
          :   Http.Response =

            val bodyFn = bodyRef.asInstanceOf[Spring[Data]^]
            val response = backend.request(uri.toString.tt, method, httpRequest.textHeaders, bodyFn)
            val code = response.status.code

            if !isRedirect(code) || remaining <= 0 then response else
              response.textHeaders.stdlib.find(_.key.lower == t"location") match
                case None =>
                  response

                case Some(header) =>
                  // Drain the discarded intermediate body to free its connection.
                  safely(response.body.stream.sweep { _ => _ => () })

                  val nextUri = uri.resolve(jn.URI.create(header.value.s).nn).nn
                  Log.fine(Http.Event.Redirect(uri.toString.tt, nextUri.toString.tt))
                  val nextMethod = redirectMethod(code, method)

                  val nextBody: AnyRef =
                    if nextMethod == method then bodyRef else
                      val empty: Spring[Data] = () => Http.emptyBody()
                      empty.asInstanceOf[AnyRef]

                  loop(nextUri, nextMethod, nextBody, remaining - 1)

          logResponse:
            loop(jn.URI.create(url.show.s).nn, httpRequest.method,
                httpRequest.body.asInstanceOf[AnyRef], redirection.value)

  // An `Http.Client` is a capability: its instances are constructed from other capabilities (a
  // `Tactic`, an `Online` token, a backend) which they retain — a given that takes capabilities
  // as parameters produces a capability (Jon, 2026-07-06; see rep/DECISIONS.md).
  trait Client extends Targetable, caps.ExclusiveCapability:
    def request(request: Http.Request, target: Target)(using (Http.Event is Loggable)^): Http.Response

  // HttpError → Http.Error
  case class Error(status: Http.Status, headers: List[Http.Header])(using Diagnostics)
  extends fulminate.Error(438, 0)(m"the HTTP request failed with status $status")

  // HttpEvent → Http.Event
  object Event:
    given communicable: Http.Event is Communicable =
      case Response(status)           => m"received a response with status $status"
      case Request(preview)           => m"request [$preview]"
      case Send(method, url, headers) => m"sending a $method request to $url"
      case Redirect(from, to)         => m"following a redirect from $from to $to"

  enum Event:
    case Response(status: Http.Status) extends Http.Event, Log.Network
    case Request(preview: Text) extends Http.Event, Log.Network

    case Send(method: Http.Method, url: into[HttpUrl], headers: List[Http.Header])
    extends Http.Event, Log.Network, Log.Protocol

    case Redirect(from: Text, to: Text) extends Http.Event, Log.Network, Log.Protocol

  // HttpRedirection → Http.Redirection
  // Caps the number of 3xx redirects the HTTP client given will follow.
  // Default = 10 (matches curl). Override with a local `given Http.Redirection(n)`.
  object Redirection:
    given default: Http.Redirection = Http.Redirection(10)

  class Redirection(val value: Int)

  // HttpSession → Http.Session
  // An HTTP session: a single client connection to one origin, over which several
  // requests are exchanged. The protocol is fixed for the session's lifetime — for
  // `https`, by ALPN during the TLS handshake (a multiplexed HTTP/2 session or a
  // sequential HTTP/1.1 one); for plaintext `http`, always sequential HTTP/1.1 with
  // keep-alive. `fetch` requires exclusive access (`update`) and its response borrows
  // the session, so an unconsumed streaming body blocks the next fetch at compile time.
  //
  // Only the interface lives here: the implementations need a wire, and live in
  // `telekinesis.http2`. It was `sealed` while class and implementations shared a file;
  // they no longer can, so the implementations are `private[telekinesis]` instead.
  abstract class Session extends caps.ExclusiveCapability, caps.Stateful:
    update def fetch(request: Request)(using Tactic[ConnectError])
    :   Response^{this, caps.any}

  // HttpConnection → Http.Connection
  object Connection:
    // The sink that writes the response to (and closes) the client connection. `out` may
    // capture the live connection (a streamed body reading the request stream), and a
    // function type cannot take a `^` parameter (the `Spring` precedent). The tactic is a
    // using-parameter, not a curried context-function result — a value of curried dependent
    // context-function type is not yet supported — so nothing escapes `apply`.
    trait Respond:
      def apply(response: Response^)(using Tactic[StreamError]): Unit

  // One live request/response exchange, as the handler sees it: the request, plus the
  // means of answering it. `Exclusive` because an exchange has a single owner and may be
  // responded to only once. Constructing one from a particular server's transport is the
  // server's business; the type itself needs nothing but the request and the sink.
  class Connection
    (     request: Request^,
      val tls:     Boolean,
      val port:    Int,
      val respond: Connection.Respond^ )
  extends Request
    ( request.method,
      request.version,
      request.host,
      request.target,
      request.textHeaders,
      request.body ),
    Findable,
    caps.ExclusiveCapability

sealed trait Http
