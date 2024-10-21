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
import hieroglyph.*
import spectacular.*
import anticipation.*
import nettlesome.*

import language.dynamics

case class HttpRequest
    (method:        HttpMethod,
     host:          Hostname,
     requestTarget: Text,
     headers:       List[RequestHeader.Value],
     body:          LazyList[Bytes])

  // def serialize: LazyList[Bytes] =
  //   import charEncoders.ascii
  //   val buffer = StringBuffer()
  //   buffer.append(method.show.upper)
  //   buffer.append(t" ")
  //   buffer.append(requestTarget)
  //   buffer.append(t" HTTP/1.0\nhost: ")
  //   buffer.append(host.show)

  //   body match
  //     case HttpBody.Chunked(_) => ()
  //     case HttpBody.Empty      => buffer.append(t"\ncontent-length: 0")

  //     case HttpBody.Data(data) =>
  //       buffer.append(t"\ncontent-length: ")
  //       buffer.append(data.length.show)

  //   headers.map: parameter =>
  //     buffer.append(t"\n")
  //     buffer.append(parameter.header.header)
  //     buffer.append(t": ")
  //     buffer.append(parameter.value)

  //   buffer.append(t"\n\n")

  //   buffer.toString.tt.bytes #:: data
