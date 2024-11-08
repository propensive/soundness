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

import gossamer.*
import rudiments.*
import hieroglyph.*
import spectacular.*
import anticipation.*
import nettlesome.*

import language.dynamics

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
