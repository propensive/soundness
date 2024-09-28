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

object Relative:
  given creator: PathCreator[Relative, GeneralForbidden, Int] with
    def path(ascent: Int, descent: List[Name[GeneralForbidden]]): SafeRelative =
      SafeRelative(ascent, descent)

  inline given (using Tactic[PathError]) => Decoder[Relative] as decoder:
    def decode(text: Text): Relative =
      if text.contains(t"\\") then text.decodeAs[Windows.Relative] else text.decodeAs[Unix.Relative]

  given Relative is Showable as showable =
    case relative: Unix.Relative    => relative.render
    case relative: Windows.Relative => relative.render
    case relative: SafeRelative     => relative.render

  given encoder: Encoder[Relative] = showable.text(_)
  given Relative is Inspectable = showable.text(_)

trait Relative
