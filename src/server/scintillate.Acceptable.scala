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
import vacuous.*
import turbulence.*
import contingency.*
import gossamer.*
import gesticulate.*
import telekinesis.*
import anticipation.*

import errorDiagnostics.stackTraces


object Acceptable:
  given (using Tactic[MultipartError]) => Multipart is Acceptable = request =>
    tend:
      case _: MediaTypeError => MultipartError(MultipartError.Reason.MediaType)
    .within:
      given HttpRequest = request
      val contentType = header(RequestHeader.ContentType).let(_.prim).let(Media.parse(_)).or:
        abort(MultipartError(MultipartError.Reason.MediaType))

      if contentType.base == media"multipart/form-data" then
        val boundary = contentType.at(t"boundary").or:
          abort(MultipartError(MultipartError.Reason.MediaType))

        Multipart.parse(request.body, boundary)
      else abort(MultipartError(MultipartError.Reason.MediaType))

trait Acceptable:
  type Self
  def accept(request: HttpRequest): Self
