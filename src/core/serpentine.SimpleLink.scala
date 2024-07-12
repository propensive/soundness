/*
    Serpentine, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package serpentine

import rudiments.*
import anticipation.*
import spectacular.*
import contingency.*
import gossamer.*

object SimpleLink:
  inline given decoder(using Tactic[PathError]): Decoder[SimpleLink] =
    Followable.decoder[SimpleLink]

  given SimpleLink is Showable = _.render

  inline def parse(text: Text)(using path: Tactic[PathError]): SimpleLink/*^{path}*/ =
    text.decodeAs[SimpleLink]

  given pathCreator: PathCreator[SimpleLink, ".*\\/.*", Int] = SimpleLink(_, _)

  given followable: Followable[".*\\/.*", "..", "."] with
    type Self = SimpleLink
    def separator(link: SimpleLink): Text = t"/"
    val separators: Set[Char] = Set('/')
    def ascent(path: SimpleLink): Int = path.ascent
    def descent(path: SimpleLink): List[Name[".*\\/.*"]] = path.descent

case class SimpleLink(ascent: Int, descent: List[Name[".*\\/.*"]])
