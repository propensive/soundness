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
import gesticulate.*
import gossamer.*
import nettlesome.*
import proscenium.*
import rudiments.*
import spectacular.*

import java.net.*
import java.io.*

import language.dynamics

object Http:
  def fetch[PostType: Postable]
     (url: HttpUrl, content: PostType, method: HttpMethod, headers: Seq[RequestHeader.Value])
     (using Online)
  :     HttpResponse logs HttpEvent =

    Log.info(HttpEvent.Send(method, url, headers))
    Log.fine(HttpEvent.Request(PostType.preview(content)))

    URI(url.show.s).toURL.nn.openConnection.nn.absolve match
      case connection: HttpURLConnection =>
        connection.setRequestMethod(method.toString.show.upper.s)

        connection.setRequestProperty
         (RequestHeader.ContentType.header.s, PostType.contentType.show.s)

        connection.setRequestProperty("User-Agent", "Telekinesis/1.0.0")

        headers.each:
          case RequestHeader.Value(key, value) =>
            connection.setRequestProperty(key.header.s, value.s)

        if method == Post || method == Put then
          connection.setDoOutput(true)
          connection.connect()
          val out = connection.getOutputStream().nn
          PostType.content(content).map(_.to(Array)).each(out.write(_))
          out.close()

        val buf = new Array[Byte](65536)

        def read(in: InputStream): Stream[Bytes] =
          val len = in.read(buf, 0, buf.length)

          if len < 0 then Stream() else IArray(buf.slice(0, len)*) #:: read(in)

        def body: Stream[Bytes] =
          try read(connection.getInputStream.nn) catch case _: Exception =>
            try read(connection.getErrorStream.nn) catch case _: Exception => Stream()

        val HttpStatus(status) = connection.getResponseCode: @unchecked
        Log.fine(HttpEvent.Response(status))

        val responseHeaders: List[(Text, Text)] =
          connection.getHeaderFields.nn.asScala.to(List).flatMap:
            case (key: String, values) => values.asScala.to(List).map(key.nn.tt -> _.tt)
            case _                     => Nil

        HttpResponse(1.1, status, responseHeaders, body)
