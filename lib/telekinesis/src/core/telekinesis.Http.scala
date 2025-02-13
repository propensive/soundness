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
┃    Soundness, version 0.27.0.                                                                    ┃
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

import anticipation.*
import coaxial.*
import contingency.*
import distillate.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import nettlesome.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import zephyrine.*

import language.dynamics

erased trait Http

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

  case class Header(key: Text, value: Text)

  object Method:
    given formmethod: ("formmethod" is GenericHtmlAttribute[Method]):
      def name: Text = t"formmethod"
      def serialize(method: Method): Text = method.show

    given method: ("method" is GenericHtmlAttribute[Method]):
      def name: Text = t"method"
      def serialize(method: Method): Text = method.show

    given communicable: Method is Communicable = method => Message(method.show.upper)

    given Method is Showable =
      case method    => method.toString.tt.upper

    given Method is Decodable in Text = _.upper match
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

  sealed trait Method(tracked val payload: Boolean):
    def unapply(request: Request): Boolean = request.method == this

  case object Get extends Method(false)
  case object Head extends Method(false)
  case object Post extends Method(true)
  case object Put extends Method(true)
  case object Delete extends Method(false)
  case object Connect extends Method(false)
  case object Options extends Method(false)
  case object Trace extends Method(false)
  case object Patch extends Method(false)

  object Status:
    private lazy val all: Map[Int, Status] =
      values.immutable(using Unsafe).bi.map(_.code -> _).to(Map)

    def unapply(code: Int): Option[Status] = all.get(code)

    given Status is Communicable = status => m"${status.code} (${status.description})"

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

    def category: Category = (code/100).absolve match
      case 1 => Http.Category.Informational
      case 2 => Http.Category.Successful
      case 3 => Http.Category.Redirection
      case 4 => Http.Category.ClientError
      case 5 => Http.Category.ServerError

  export Status.*

  object Request:
    given Request is Showable = request =>
      val bodySample: Text =
        try request.body.stream.read[Bytes].utf8 catch
          case err: StreamError  => t"[-/-]"

      val headers: Text =
        request.textHeaders.map: header =>
          t"${header.key}: ${header.value}"

        . join(t"\n          ")

      val params: Text = request.params.map:
        case (k, v) => t"$k=\"$v\""

      . join(t"\n          ")

      ListMap[Text, Text](
        t"content"
        -> safely(request.headers.contentType.prim.or(media"application/octet-stream").show)
           . or(t"?"),
        t"method"   -> request.method.show,
        t"query"    -> request.query.show,
        t"hostname" -> request.host.show,
        t"path"     -> request.pathText,
        t"body"     -> bodySample,
        t"headers"  -> headers,
        t"params"   -> params
      ).map { case (key, value) => t"$key = $value" }.join(t", ")

    given Request is Transmissible = request =>
      import charEncoders.ascii

      val text: Text = Text.construct:
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

        request.body match
          case Stream()     => append(t"Content-Length: 0")
          case Stream(data) => append(t"Content-Length: ${data.length}")
          case _            => append(t"Transfer-Encoding: chunked")

        request.textHeaders.map: parameter =>
          newline()
          append(parameter.key)
          append(t": ")
          append(parameter.value)

        newline()
        newline()

      text.bytes #:: request.body

  case class Request
     (method:      Http.Method,
      version:     Http.Version,
      host:        Hostname,
      target:      Text,
      textHeaders: List[Http.Header],
      body:        Stream[Bytes]):

    def on[SchemeType <: "http" | "https"](origin: Origin[SchemeType]): HttpUrl =
      Url[SchemeType](origin, target)

    lazy val query: Text = target.s.indexOf('?') match
      case -1    => t""
      case index => target.skip(index + 1)

    lazy val pathText: Text = target.s.indexOf('?') match
      case -1    => target
      case index => target.keep(index)

    lazy val queryParams: Map[Text, List[Text]] =
      val items = query.nn.show.cut(t"&")

      items.foldLeft(Map[Text, List[Text]]()): (map, elem) =>
        elem.cut(t"=", 2) match
          case List(key, value) => map.updated(key, safely(value).or(t"") :: map.getOrElse(key, Nil))
          case _                => map

    lazy val params: Map[Text, Text] =
      queryParams.map:
        case (k, vs) => k.urlDecode -> vs.prim.or(t"").urlDecode

      . to(Map) ++ method.match
        case Http.Post | Http.Put =>
          contentType.or(media"application/x-www-form-urlencoded").base.show match
            case t"multipart/form-data" =>
              mend:
                case MultipartError(_) => Map()

              . within:
                  Multipart.parse(body.read[Bytes]).parts.filter(_.filename.absent).map: part =>
                    import charDecoders.utf8
                    import textSanitizers.strict
                    part.name.or(t"") -> safely(part.body.read[Text]).or(t"")
                  . to(Map)

            case t"application/x-www-form-urlencoded" =>
              body.stream.read[Bytes].utf8.cut(t"&").map(_.cut(t"=", 2).to(Seq) match
                case Seq(key: Text)              => key.urlDecode.show -> t""
                case Seq(key: Text, value: Text) => key.urlDecode.show -> value.urlDecode.show
                case _                           => panic(m"key/value pair does not match")
              ).to(Map)

            case _ =>
              Map()

        case _ => Map()

    object headers extends Dynamic:
      def selectDynamic(name: Label)
         (using Prefixable: name.type is Prefixable, decoder: Prefixable.Subject is Decodable in Text)
      :     List[Prefixable.Subject] =
        val name2 = name.tt.uncamel.kebab.lower
        textHeaders.filter(_.key.lower == name2).map(_.value.decode)

    lazy val contentType: Optional[MediaType] = safely(headers.contentType.prim)

    lazy val textCookies: Map[Text, Text] =
      headers.cookie.flatMap: cookie =>
        cookie.bi.map(_.name -> _.value)
      . to(Map)

  object Response extends Dynamic:
    inline def applyDynamicNamed(id: "apply")(inline headers: (Label, Any)*): Prototype =
      ${Telekinesis.response('headers)}

    inline def applyDynamic(id: "apply")(inline headers: Any*): Prototype =
      ${Telekinesis.response('headers)}

    case class Prototype(status0: Optional[Status], headers: Seq[Header]):

      def apply(body: Stream[Bytes]): Response =
        Response(1.1, status0.or(Ok), headers.to(List), body)

      def apply[BodyType: Servable](body: BodyType): Response =
        val response = BodyType.serve(body)
        Response
         (1.1,
          status0.or(response.status),
          headers.to(List) ++ response.textHeaders,
          response.body)

    given readable: Tactic[HttpError] => Response is Readable by Bytes = response =>
      val body = response.status.category match
        case Http.Status.Category.Successful => response.body

        case _ =>
          raise(HttpError(response.status, response.textHeaders), response.body)

      body.stream

    def make(status: Status, headers: List[Header], body: Stream[Bytes]): Response =
      new Response(1.1, status, headers, body)

    def parse(stream: Stream[Bytes]): Response raises HttpResponseError =
      val conduit = Conduit(stream)

      inline def expect(char: Char) = if conduit.datum != char then raise:
        HttpResponseError(HttpResponseError.Reason.Expectation(char, conduit.datum.toChar))

      conduit.mark()
      expect('H')
      conduit.next()
      expect('T')
      conduit.next()
      expect('T')
      conduit.next()
      expect('P')
      conduit.next()
      expect('/')
      conduit.next()
      conduit.seek(' ')
      val version: Http.Version = Http.Version.parse(Ascii(conduit.save()).show)
      conduit.next()
      conduit.mark()
      if conduit.datum < '1' || conduit.datum > '5' then
        conduit.next()
        conduit.next()
        abort(HttpResponseError(HttpResponseError.Reason.Status(Ascii(conduit.save()).show)))

      var code: Int = conduit.datum - '0'
      conduit.next()

      if conduit.datum < '0' || conduit.datum > '9' then
        conduit.next()
        abort(HttpResponseError(HttpResponseError.Reason.Status(Ascii(conduit.save()).show)))

      code = code*10 + (conduit.datum - '0')
      conduit.next()

      if conduit.datum < '0' || conduit.datum > '9' then
        abort(HttpResponseError(HttpResponseError.Reason.Status(Ascii(conduit.save()).show)))

      code = code*10 + (conduit.datum - '0')
      conduit.next()
      expect(' ')

      val status = Http.Status.unapply(code).optional.or:
        abort(HttpResponseError(HttpResponseError.Reason.Status(code.toString.tt)))

      conduit.seek('\r')
      conduit.next()
      expect('\n')

      def readHeaders(headers: List[Http.Header]): List[Http.Header] =
        conduit.next()
        conduit.mark()
        if conduit.datum == '\r' then
          conduit.next()
          expect('\n')
          headers

        else
          conduit.next()
          conduit.seek(':')
          val header = Ascii(conduit.save()).show
          conduit.next()
          while conduit.datum == ' ' || conduit.datum == '\t' do conduit.next()
          conduit.mark()
          conduit.seek('\r')
          val value = Ascii(conduit.save()).show
          conduit.next()
          expect('\n')
          readHeaders(Http.Header(header, value) :: headers)

      val headers = readHeaders(Nil)

      conduit.break()
      val body = conduit.remainder

      Response(version, status, headers.reverse, body)

  case class Response private
     (version:     Http.Version,
      status:      Http.Status,
      textHeaders: List[Http.Header],
      body:        Stream[Bytes]):

    def successBody: Optional[Stream[Bytes]] =
      body.provided(status.category == Http.Status.Category.Successful)

    def receive[BodyType: Receivable as receivable]: BodyType = receivable.read(this)

    object headers extends Dynamic:
      def selectDynamic(name: Label)
         (using Prefixable: name.type is Prefixable, decoder: Prefixable.Subject is Decodable in Text)
      :     List[Prefixable.Subject] =
        val name2 = name.tt.uncamel.kebab.lower
        textHeaders.filter(_.key.lower == name2).map(_.value.decode)

    @targetName("add")
    infix def + [ValueType: Encodable in Http.Header](value: ValueType): Response =
      val header: Http.Header = ValueType.encode(value)
      copy(textHeaders = header :: textHeaders)
