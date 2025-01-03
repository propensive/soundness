/*
    Telekinesis, version 0.24.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import prepositional.*
import rudiments.*
import turbulence.*
import vacuous.*

object HttpResponse:
  given (using Tactic[HttpError]) => HttpResponse is Readable by Bytes as readable = response =>
    val body = response.status match
      case status: (HttpStatus & FailureCase) =>
        raise(HttpError(status, response.headers), response.body)

      case status =>
        response.body

    body.stream

  def apply[ServableType: Servable](servable: ServableType, headers: ResponseHeader.Value*)
          : HttpResponse =

    val headers2: List[(Text, Text)] = headers.to(List).map: header =>
      header.header.header -> header.value

    val response = ServableType.serve(servable)
    response.copy(headers = headers2 ++ response.headers)

case class HttpResponse
   (version: HttpVersion, status:  HttpStatus, headers: List[(Text, Text)], body: LazyList[Bytes]):

  lazy val headersMap: Map[ResponseHeader[?], List[Text]] = headers.foldLeft(Map()):
    case (acc, (ResponseHeader(key), value)) => acc.updated(key, value :: acc.getOrElse(key, Nil))

  def as[BodyType: Receivable as receivable]: BodyType raises HttpError = (status: @unchecked) match
    case status: FailureCase => abort(HttpError(status, headers: List[(Text, Text)]))
    case status              => readable.read(status, body)

  @targetName("add")
  infix def + [ValueType: Encodable in ResponseHeader.Value](value: ValueType): HttpResponse =
    val header = ValueType.encode(value)
    copy(headers = (header.header.header, header.value) :: headers)

  def apply[ValueType](header: ResponseHeader[ValueType])
     (using decoder: HttpHeaderDecoder[ValueType])
          : List[ValueType] =

    headersMap.at(header).or(Nil).map(decoder.decode(_))
