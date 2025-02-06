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

import language.dynamics

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import turbulence.*
import vacuous.*

object HttpResponse:
  given readable: Tactic[HttpError] => HttpResponse is Readable by Bytes = response =>
    val body = response.status.category match
      case HttpStatus.Category.Successful => response.body

      case _ =>
        raise(HttpError(response.status, response.textHeaders), response.body)

    body.stream

  def apply[ServableType: Servable](servable: ServableType, headers: HttpHeader*)
  :     HttpResponse =

    val response = ServableType.serve(servable)
    response.copy(textHeaders = headers.to(List) ++ response.textHeaders)

case class HttpResponse
   (version: HttpVersion, status: HttpStatus, textHeaders: List[HttpHeader], body: Stream[Bytes]):

  def successBody: Optional[Stream[Bytes]] = body.provided(status == HttpStatus.Category.Successful)

  def receive[BodyType: Receivable as receivable]: BodyType = receivable.read(this)

  object headers extends Dynamic:
    def selectDynamic(name: Label)
       (using capitate: name.type is Capitate, decoder: Decoder[capitate.Subject])
    :     List[capitate.Subject] =
      val name2 = name.tt.uncamel.kebab.lower
      textHeaders.filter(_.key.lower == name2).map(_.value.decode)

  @targetName("add")
  infix def + [ValueType: Encodable in HttpHeader](value: ValueType): HttpResponse =
    val header: HttpHeader = ValueType.encode(value)
    copy(textHeaders = header :: textHeaders)
