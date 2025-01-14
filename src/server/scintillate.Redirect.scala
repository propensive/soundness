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
import spectacular.*
import telekinesis.*

object Redirect:
  def apply[LinkType: Fetchable](location: LinkType, permanent: Boolean): Redirect =
    new Redirect(LinkType.url(location).show, permanent)

  given Redirect is Servable as redirect = redirect =>
    val headers = List(ResponseHeader.Location.header -> redirect.location)
    val status = if redirect.permanent then HttpStatus.MovedPermanently else HttpStatus.Found
    HttpResponse(1.1, status, headers, LazyList())

case class Redirect(location: Text, permanent: Boolean = false)
