/*
    Gesticulate, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

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
import contextual.*
import fulminate.*
import gossamer.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.dynamics
//import language.experimental.captureChecking

object MediaTypeError:
  enum Reason:
    case NotOneSlash, MissingParam, InvalidGroup
    case InvalidChar(char: Char)
    case InvalidSuffix(suffix: Text)

    def message: Text = this match
      case NotOneSlash      => txt"a media type should always contain exactly one '/' character"
      case MissingParam     => txt"a terminal ';' suggests that a parameter is missing"
      case InvalidGroup     => val list = Media.Group.values.immutable(using Unsafe).map(_.name)
                               txt"the type must be one of: ${list.join(t", ", t" or ")}"
      case InvalidChar(c)   => txt"the character '$c' is not allowed"
      case InvalidSuffix(s) => txt"the suffix '$s' is not recognized"

case class MediaTypeError(value: Text, reason: MediaTypeError.Reason)(using Diagnostics)
extends Error(m"the value $value is not a valid media type; ${reason.message}")
