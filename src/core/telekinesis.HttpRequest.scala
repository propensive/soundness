/*
    Telekinesis, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package telekinesis

import anticipation.*
import coaxial.*
import contingency.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import nettlesome.*
import proscenium.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

import language.dynamics

object HttpRequest:
  given HttpRequest is Showable = request =>
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
      -> safely(request.headers.contentType.prim.or(media"application/octet-stream").show).or(t"?"),
      t"method"   -> request.method.show,
      t"query"    -> request.query.show,
      t"hostname" -> request.host.show,
      t"path"     -> request.pathText,
      t"body"     -> bodySample,
      t"headers"  -> headers,
      t"params"   -> params
    ).map { case (key, value) => t"$key = $value" }.join(t", ")

  given HttpRequest is Transmissible = request =>
    import charEncoders.ascii

    val text: Text = Text.construct:
      def newline(): Unit = append(t"\r\n")
      append(request.method.show)
      append(t" ")
      append(request.target)
      append(t" ")
      append(HttpVersion.showable.text(request.version))
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

case class HttpRequest
   (method:      Http.Method,
    version:     HttpVersion,
    host:        Hostname,
    target:      Text,
    textHeaders: List[HttpHeader],
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
       (using capitate: name.type is Capitate, decoder: Decoder[capitate.Subject])
    :     List[capitate.Subject] =
      val name2 = name.tt.uncamel.kebab.lower
      textHeaders.filter(_.key.lower == name2).map(_.value.decode)

  lazy val contentType: Optional[MediaType] = safely(headers.contentType.prim)

  lazy val textCookies: Map[Text, Text] =
    headers.cookie.flatMap: cookie =>
      cookie.bi.map(_.name -> _.value)
    . to(Map)
