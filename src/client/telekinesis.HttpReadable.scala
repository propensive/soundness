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
import turbulence.*
import anticipation.*

object HttpReadable:
  given Text is HttpReadable as text = (status, body) => body match
    case HttpBody.Empty         => t""
    case HttpBody.Data(body)    => body.utf8
    case HttpBody.Chunked(body) => body.read[Bytes].utf8

  given Bytes is HttpReadable as bytes = (status, body) => body match
    case HttpBody.Empty         => IArray()
    case HttpBody.Data(body)    => body
    case HttpBody.Chunked(body) => body.read[Bytes]

  given LazyList[Bytes] is HttpReadable as byteStream = (status, body) => body match
    case HttpBody.Empty         => LazyList()
    case HttpBody.Data(body)    => LazyList(body)
    case HttpBody.Chunked(body) => body

  given [ContentType: GenericHttpReader as readable]
      => ContentType is HttpReadable as genericHttpReader =
    (status, body) => body match
      case HttpBody.Empty         => readable.read(t"")
      case HttpBody.Data(data)    => readable.read(data.utf8)
      case HttpBody.Chunked(data) => readable.read(data.read[Bytes].utf8)

  given HttpStatus is HttpReadable as httpStatus:
    def read(status: HttpStatus, body: HttpBody) = status

trait HttpReadable:
  type Self
  def read(status: HttpStatus, body: HttpBody): Self
