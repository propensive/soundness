/*
    Telekinesis, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import contingency.*
import fulminate.*
import gesticulate.*
import gossamer.*
import hieroglyph.*
import nettlesome.*
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
      request.headers.map: header =>
        t"${header.header}: ${header.value}"

      . join(t"\n          ")

    val params: Text = request.params.map:
      case (k, v) => t"$k=\"$v\""

    . join(t"\n          ")

    ListMap[Text, Text](
      t"content"  -> request.contentType.lay(t"application/octet-stream")(_.show),
      t"method"   -> request.method.show,
      t"query"    -> request.query.show,
      t"hostname" -> request.host.show,
      t"path"     -> request.pathText,
      t"body"     -> bodySample,
      t"headers"  -> headers,
      t"params"   -> params
    ).map { case (key, value) => t"$key = $value" }.join(t", ")

case class HttpRequest
   (method:  HttpMethod,
    version: HttpVersion,
    host:    Hostname,
    target:  Text,
    headers: List[RequestHeader.Value],
    body:    LazyList[Bytes]):

  def serialize: LazyList[Bytes] =
    import charEncoders.ascii
    val text: Text = Text.construct:
      def newline(): Unit = append(t"\r\n")
      append(method.show.upper)
      append(t" ")
      append(target)
      append(t" ")
      append(HttpVersion.showable.text(version))
      newline()
      append(t"Host: ")
      append(host.show)
      newline()

      body match
        case LazyList()     => append(t"Content-Length: 0")
        case LazyList(data) => append(t"Content-Length: ${data.length}")
        case _              => append(t"Transfer-Encoding: chunked")

      headers.map: parameter =>
        newline()
        append(parameter.header.header)
        append(t": ")
        append(parameter.value)

      newline()
      newline()

    text.bytes #:: body

  lazy val query: Text = target.s.indexOf('?') match
    case -1    => t""
    case index => target.skip(index + 1)

  lazy val pathText: Text = target.s.indexOf('?') match
    case -1    => target
    case index => target.keep(index)

  lazy val queryParams: Map[Text, List[Text]] =
    val items = query.nn.show.cut(t"&")
    items.foldLeft(Map[Text, List[Text]]()): (map, elem) =>
      val kv: List[Text] = elem.cut(t"=", 2)
      map.updated(kv(0), safely(kv(1)).or(t"") :: map.getOrElse(kv(0), Nil))

  lazy val params: Map[Text, Text] =
    queryParams.map:
      case (k, vs) => k.urlDecode -> vs.prim.or(t"").urlDecode

    . to(Map) ++ method.match
      case HttpMethod.Post | HttpMethod.Put =>
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

  def header(header: RequestHeader[?]): List[RequestHeader.Value] =
    headers.filter(_.header == header)

  lazy val length: Int raises StreamError =
    try throwErrors:
      header(RequestHeader.ContentLength).headOption.map(_.value.decode[Int]).getOrElse:
        body.stream.map(_.length).sum
    catch case err: NumberError => abort(StreamError(0.b))

  lazy val contentType: Optional[MediaType] =
    headers.find(_.header == RequestHeader.ContentType).map(_.value).flatMap(MediaType.unapply(_)).optional

  lazy val cookies: Map[Text, Text] =
    header(RequestHeader.Cookie).map(_.value).flatMap(_.cut(t"; ")).flatMap: cookie =>
      cookie.cut(t"=", 2) match
      case List(key, value) => List((key.urlDecode, value.urlDecode))
      case _                => Nil

    . to(Map)
