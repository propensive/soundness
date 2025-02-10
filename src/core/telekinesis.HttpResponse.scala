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
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import spectacular.*
import turbulence.*
import vacuous.*

object HttpResponseError:
  enum Reason:
    case Expectation(expected: Char, found: Char)
    case Status(value: Text)

  given Reason is Communicable =
    case Reason.Expectation(expected, found) => m"$found was found when $expected was expected"
    case Reason.Status(value)                => m"the HTTP status code $value was invalid"

case class HttpResponseError(reason: HttpResponseError.Reason)(using Diagnostics)
extends Error(m"could not parse HTTP response because $reason")

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

  def parse(stream: Stream[Bytes]): HttpResponse raises HttpResponseError =
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
    val version: HttpVersion = HttpVersion.parse(Ascii(conduit.save()).show)
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

    val status = HttpStatus.unapply(code).optional.or:
      abort(HttpResponseError(HttpResponseError.Reason.Status(code.toString.tt)))

    conduit.seek('\r')
    conduit.next()
    expect('\n')

    def readHeaders(headers: List[HttpHeader]): List[HttpHeader] =
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
        readHeaders(HttpHeader(header, value) :: headers)

    val headers = readHeaders(Nil)

    conduit.break()
    val body = conduit.remainder

    HttpResponse(version, status, headers.reverse, body)

case class HttpResponse
   (version: HttpVersion, status: HttpStatus, textHeaders: List[HttpHeader], body: Stream[Bytes]):

  def successBody: Optional[Stream[Bytes]] =
    body.provided(status.category == HttpStatus.Category.Successful)

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
