/*
    Galilei, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contextual.*
import contingency.*
import fulminate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

import scala.compiletime.*

import language.experimental.pureFunctions

object Link:
  given creator: PathCreator[Link, GeneralForbidden, Int] with
    def path(ascent: Int, descent: List[Name[GeneralForbidden]]): SafeLink = SafeLink(ascent, descent)

  inline given (using Tactic[PathError]) => Decoder[Link] as decoder:
    def decode(text: Text): Link =
      if text.contains(t"\\") then text.decodeAs[Windows.Link] else text.decodeAs[Unix.Link]

  given Link is Showable as showable =
    case link: Unix.Link    => link.render
    case link: Windows.Link => link.render
    case link: SafeLink     => link.render

  given encoder: Encoder[Link] = showable.text(_)
  given Link is Inspectable = showable.text(_)

trait Link
