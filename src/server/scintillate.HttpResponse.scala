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
import prepositional.*
import gossamer.*
import telekinesis.*
import anticipation.*
import spectacular.*


object HttpResponse:
  def apply[FormatType: Servable]
     (content: FormatType,
      status: HttpStatus = HttpStatus.Found,
      headers: Map[ResponseHeader[?], Text] = Map(),
      cookies: List[Cookie.Value] = Nil)
          : HttpResponse in FormatType =
    inline def content0: FormatType = content
    inline def status0: HttpStatus = status
    inline def headers0: Map[ResponseHeader[?], Text] = headers
    inline def cookies0: List[Cookie.Value] = cookies

    new HttpResponse:
      type Format = FormatType
      def servable: Format is Servable = summon[FormatType is Servable]
      def content: Format = content0
      def status: HttpStatus = status0
      def headers: Map[ResponseHeader[?], Text] = headers0
      def cookies: List[Cookie.Value] = cookies0

trait HttpResponse:
  type Format
  def servable: Format is Servable
  def content: Format
  def status: HttpStatus
  def headers: Map[ResponseHeader[?], Text]
  def cookies: List[Cookie.Value]

  def allHeaders: List[(ResponseHeader[?], Text)] =
    headers.to(List) ++ cookies.map(ResponseHeader.SetCookie -> _.show)

  def respond(responder: Responder): Unit =
    servable.process
     (content, status.code, allHeaders.map { case (k, v) => k.header -> v }.to(Map), responder)

  def serialize: Text = Text.construct:
    for (key, value) <- allHeaders do append(t"${key.header}: $value\r\n")
    append(t"\r\n")
