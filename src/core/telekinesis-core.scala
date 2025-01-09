/*
    Telekinesis, version 0.25.0. Copyright 2025 Jon Pretty, Propensive OÜ.

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

given Realm = realm"telekinesis"

extension [UrlType: Fetchable](url: UrlType)(using Online)
  def fetch(method: HttpMethod)(headers: RequestHeader.Value*): HttpResponse logs HttpEvent =
    Http.fetch(UrlType.url(url), (), method, headers)

  def fetch(headers: RequestHeader.Value*): HttpResponse logs HttpEvent =
    Http.fetch(UrlType.url(url), (), Get, headers)

  def submit[BodyType: Postable]
     (method: (HttpMethod { val payload: true }) = Post, headers: RequestHeader.Value*)
     (body: BodyType)
          : HttpResponse logs HttpEvent =
    Http.fetch[BodyType](UrlType.url(url), body, method, headers)
