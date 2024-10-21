/*
    Telekinesis, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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

import gossamer.{slice as _, *}
import rudiments.*
import gesticulate.*
import spectacular.*
import anticipation.*
import nettlesome.*

import java.net.*
import java.io.*
import java.util as ju

import language.dynamics

object Http:
  def post[PostType: Postable, UrlType: Hyperlinkable]
      (url: UrlType, content: PostType = (), headers: RequestHeader.Value*)
      (using Online)
          : HttpResponse logs HttpEvent =

    request[PostType](UrlType.hyperlink(url), content, HttpMethod.Post, headers)

  def put[PostType: Postable, UrlType: Hyperlinkable]
      (url: UrlType, content: PostType = (), headers: RequestHeader.Value*)
      (using Online)
          : HttpResponse logs HttpEvent =

    request[PostType](UrlType.hyperlink(url), content, HttpMethod.Put, headers)

  def get[UrlType: Hyperlinkable](url: UrlType, headers: Seq[RequestHeader.Value] = Nil)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Get, headers)

  def options[UrlType: Hyperlinkable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Options, headers)

  def head[UrlType: Hyperlinkable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Head, headers)

  def delete[UrlType: Hyperlinkable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Delete, headers)

  def connect[UrlType: Hyperlinkable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Connect, headers)

  def trace[UrlType: Hyperlinkable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Trace, headers)

  def patch[UrlType: Hyperlinkable]
      (url: UrlType, headers: RequestHeader.Value*)(using Online)
          : HttpResponse logs HttpEvent =

    request(UrlType.hyperlink(url), (), HttpMethod.Patch, headers)

  private def request[PostType: Postable]
      (url: HttpUrl, content: PostType, method: HttpMethod, headers: Seq[RequestHeader.Value])
      (using Online)
          : HttpResponse logs HttpEvent =

    Log.info(HttpEvent.Send(method, url, headers))
    Log.fine(HttpEvent.Request(PostType.preview(content)))

    (URI(url.show.s).toURL.nn.openConnection.nn: @unchecked) match
      case conn: HttpURLConnection =>
        conn.setRequestMethod(method.toString.show.upper.s)
        conn.setRequestProperty(RequestHeader.ContentType.header.s, PostType.contentType.show.s)
        conn.setRequestProperty("User-Agent", "Telekinesis/1.0.0")

        headers.each:
          case RequestHeader.Value(key, value) => conn.setRequestProperty(key.header.s, value.s)

        if method == HttpMethod.Post || method == HttpMethod.Put then
          conn.setDoOutput(true)
          val out = conn.getOutputStream().nn
          PostType.content(content).map(_.to(Array)).each(out.write(_))
          out.close()

        val buf = new Array[Byte](65536)

        def read(in: InputStream): LazyList[Bytes] =
          val len = in.read(buf, 0, buf.length)

          if len < 0 then LazyList() else IArray(buf.slice(0, len)*) #:: read(in)


        def body: LazyList[Bytes] =
          try read(conn.getInputStream.nn) catch case _: Exception =>
            try read(conn.getErrorStream.nn) catch case _: Exception => LazyList()

        val HttpStatus(status) = conn.getResponseCode: @unchecked
        Log.fine(HttpEvent.Response(status))

        val responseHeaders: Map[ResponseHeader[?], List[Text]] =
          val scalaMap: Map[String | Null, ju.List[String]] = conn.getHeaderFields.nn.asScala.toMap

          scalaMap.flatMap: value =>
            (value: @unchecked) match
              case (null, v)              => Nil
              case (ResponseHeader(k), v) => List((k, v.asScala.to(List).map(_.tt)))
          .to(Map)

        HttpResponse(url, method, status, responseHeaders, body)
