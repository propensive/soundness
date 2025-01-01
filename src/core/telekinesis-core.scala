/*
    Telekinesis, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

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
import fulminate.*
import nettlesome.*

import language.dynamics

extension (url: HttpUrl)(using Online)
  def post[BodyType: Postable](headers: RequestHeader.Value*)(body: BodyType)
          : HttpResponse logs HttpEvent =
    Http.post(url, body, headers*)

  def put[BodyType: Postable](headers: RequestHeader.Value*)(body: BodyType)
          : HttpResponse logs HttpEvent =
    Http.put(url, body, headers*)

  def post[BodyType: Postable](body: BodyType): HttpResponse logs HttpEvent = Http.post(url, body)
  def put[BodyType: Postable](body: BodyType): HttpResponse logs HttpEvent = Http.put(url, body)
  def get(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.get(url, headers)

  def options(headers: RequestHeader.Value*): HttpResponse logs HttpEvent =
    Http.options(url, headers*)

  def trace(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.trace(url, headers*)
  def patch(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.patch(url, headers*)
  def head(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.head(url, headers*)
  def delete(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.delete(url, headers*)
  def connect(headers: RequestHeader.Value*): HttpResponse logs HttpEvent = Http.connect(url, headers*)

given Realm = realm"telekinesis"
