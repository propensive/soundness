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
import gossamer.*
import rudiments.*
import serpentine.*
import spectacular.*

import scala.jdk.StreamConverters.*

import language.experimental.pureFunctions

case class SafeLink(ascent: Int, descent: List[Name[GeneralForbidden]]) extends Link

object SafeLink:
  given creator: PathCreator[SafeLink, GeneralForbidden, Int] = SafeLink(_, _)
  given SafeLink is Showable as show = _.render
  given encoder: Encoder[SafeLink] = _.render
  given SafeLink is Inspectable = _.render

  given (using PathCreator[SafeLink, GeneralForbidden, Int], ValueOf["."]) => SafeLink is Followable[GeneralForbidden, "..", "."] =
    new Followable[GeneralForbidden, "..", "."]:
      type Self = SafeLink
      val separators: Set[Char] = Set('/', '\\')
      def separator(link: SafeLink): Text = t"/"
      def ascent(link: SafeLink): Int = link.ascent
      def descent(link: SafeLink): List[Name[GeneralForbidden]] = link.descent

  inline given decoder(using Tactic[PathError]): Decoder[SafeLink] = Followable.decoder[SafeLink]
