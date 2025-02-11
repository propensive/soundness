/*
    Scintillate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import nettlesome.*
import parasite.*
import prepositional.*
import proscenium.*
import rudiments.*
import serpentine.*
import spectacular.*
import telekinesis.*
import vacuous.*

def cookie(using request: HttpRequest)(key: Text): Optional[Text] = request.textCookies.at(key)

def basicAuth(validate: (Text, Text) => Boolean, realm: Text)(response: => HttpResponse)
   (using connection: HttpConnection)
:     HttpResponse raises AuthError =
  connection.headers.authorization match
    case List(Auth.Basic(username, password)) =>
      if validate(username, password) then response
      else HttpResponse(1.1, HttpStatus.Forbidden, Nil, Stream())

    case _ =>
      val auth = t"""Basic realm="$realm", charset="UTF-8""""

      HttpResponse
       (1.1,
        HttpStatus.Unauthorized,
        List(HttpHeader(ResponseHeader.WwwAuthenticate.show, auth)),
        Stream())

inline def param(key: Text): Optional[Text] = request.params.get(key).getOrElse(Unset)
inline def request: HttpRequest = compiletime.summonInline[HttpRequest]

given realm: Realm = realm"scintillate"

extension (value: Http.type)
  def listen(handle: (connection: HttpConnection) ?=> HttpResponse)
     (using RequestServable, Monitor, Codicil)
  :     HttpService logs HttpServerEvent =
    summon[RequestServable].listen(handle)

extension (request: HttpRequest)
  def as[BodyType: Acceptable]: BodyType = BodyType.accept(request)

  def path(using connection: HttpConnection)
  :     HttpUrl raises PathError raises UrlError raises HostnameError =
    val scheme = if connection.tls then t"https" else t"http"
    Url.parse(t"$scheme://${request.host}${request.pathText}")
