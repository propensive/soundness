/*
    Scintillate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package scintillate

import rudiments.*
import fulminate.*
import prepositional.*
import vacuous.*
import parasite.*
import contingency.*
import gossamer.*
import nettlesome.*
import monotonous.*, alphabets.base64.standard
import telekinesis.*
import anticipation.*
import serpentine.*
import spectacular.*

def cookie(using request: HttpRequest)(key: Text): Optional[Text] = request.cookies.at(key)

def header(using request: HttpRequest)(header: RequestHeader[?]): Optional[List[Text]] =
  request.header(header).map(_.value)

def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => HttpResponse)
   (using connection: HttpConnection)
        : HttpResponse =
  connection.request.header(RequestHeader.Authorization).let(_.map(_.value)).or(Nil) match
    case List(s"Basic $credentials") =>
      safely(credentials.tt.deserialize[Base64].utf8.cut(t":").to(List)) match
        case List(username: Text, password: Text) if validate(username, password) =>
          response

        case _ =>
          HttpResponse(1.1, HttpStatus.Forbidden, Nil, LazyList())

    case _ =>
      val auth = t"""Basic realm="$realm", charset="UTF-8""""

      HttpResponse
       (1.1, HttpStatus.Unauthorized, List(ResponseHeader.WwwAuthenticate.show -> auth), LazyList())

inline def param(key: Text): Optional[Text] = request.params.get(key).getOrElse(Unset)

inline def request: HttpRequest = scala.compiletime.summonFrom:
   case request: HttpRequest       => request
   case connection: HttpConnection => connection.request

given realm: Realm = realm"scintillate"

extension (value: Http.type)
  def listen(handle: (connection: HttpConnection) ?=> HttpResponse)
     (using RequestServable, Monitor, Codicil)
          : HttpService logs HttpServerEvent =
    summon[RequestServable].listen(handle)

extension (request: HttpRequest)
  def as[BodyType: Acceptable]: BodyType = BodyType.accept(request)

  def path(using connection: HttpConnection)
          : HttpUrl raises PathError raises UrlError raises HostnameError =
    val scheme = if connection.secure then t"https" else t"http"
    Url.parse(t"$scheme://${request.host}${request.pathText}")
