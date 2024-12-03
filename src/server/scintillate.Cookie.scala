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

import anticipation.*
import fulminate.*
import gossamer.*
import nettlesome.*
import prepositional.*
import rudiments.*
import spectacular.*
import symbolism.*
import telekinesis.*
import vacuous.*

import scala.compiletime.*

import java.text as jt

object Cookie:
  given ("set-cookie" is GenericHttpRequestParam[Cookie.Value]) as setCookie = _.show
  given ("cookie" is GenericHttpRequestParam[Cookie.Value]) as cookie = _.show

  val dateFormat: jt.SimpleDateFormat = jt.SimpleDateFormat("dd MMM yyyy HH:mm:ss")

  def apply[ValueType: {Encodable in Text, Decoder}](using DummyImplicit)
     [DurationType: GenericDuration]
     (name:     Text,
      domain:   Optional[Hostname]     = Unset,
      expiry:   Optional[DurationType] = Unset,
      secure:   Boolean                = false,
      httpOnly: Boolean                = false,
      path:     Optional[Text]         = Unset) =
  new Cookie[ValueType](name, domain, expiry.let(_.milliseconds), secure, httpOnly, path)

  object Value:
    given Value is Showable = cookie =>
      List
       (t"${cookie.name}=${cookie.value}",
        cookie.expiry.let { expiry => t"Max-Age=$expiry" },
        cookie.domain.let { domain => t"Domain=$domain" },
        cookie.path.let { path => t"Path=$path" },
        if cookie.secure then t"Secure" else Unset,
        if cookie.httpOnly then t"HttpOnly" else Unset)
      .compact.join(t"; ")

    given Cookie.Value is Encodable in ResponseHeader.Value = cookie =>
      ResponseHeader.SetCookie(cookie.show)

    given HttpResponse is Addable by Cookie.Value into HttpResponse = (response, cookie) =>
      response.copy(headers = (t"set-cookie", cookie.show) :: response.headers)

  case class Value
     (name:     Text,
      value:    Text,
      domain:   Optional[Text] = Unset,
      path:     Optional[Text] = Unset,
      expiry:   Optional[Long] = Unset,
      secure:   Boolean        = false,
      httpOnly: Boolean        = false)

case class Cookie[ValueType: {Encodable in Text, Decoder}]
   (name:     Text,
    domain:   Optional[Hostname],
    expiry:   Optional[Long],
    secure:   Boolean,
    httpOnly: Boolean,
    path:     Optional[Text]):

  def apply(value: ValueType): Cookie.Value =
    Cookie.Value
     (name,
      value.encode,
      domain.let(_.show),
      path,
      expiry.let(_/1000),
      secure,
      httpOnly)

  inline def apply(): Optional[ValueType] = request.cookies.at(name).let(_.decode)
