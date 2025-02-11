/*
    Gesticulate, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gesticulate

import anticipation.*
import fulminate.*

import scala.reflect.*

object MultipartError:
  enum Reason:
    case Expected(char: Char)
    case StreamContinues, BadBoundaryEnding, MediaType, BadDisposition

  given Reason is Communicable =
    case Reason.Expected(char)    => m"the character '$char' was expected"
    case Reason.StreamContinues   => m"the stream continues beyond the last part"
    case Reason.BadBoundaryEnding => m"unexpected content followed the boundary"
    case Reason.MediaType         => m"the media type is invalid"
    case Reason.BadDisposition    => m"the `Content-Disposition` header has the wrong format"

import MultipartError.Reason

case class MultipartError(reason: MultipartError.Reason)(using Diagnostics)
extends Error(m"The multipart data could not be read because $reason")
