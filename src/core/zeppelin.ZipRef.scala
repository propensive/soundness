/*
    Zeppelin, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package zeppelin

import rudiments.*
import gossamer.*
import vacuous.*
import contingency.*
import serpentine.*
import denominative.*
import anticipation.*
import spectacular.*
import prepositional.*

object ZipRef:
  def apply(text: Text)
      (using navigable:  ZipRef is Navigable[InvalidZipNames, Unset.type],
             rootParser: RootParser[ZipRef, Unset.type],
             creator:    PathCreator[ZipRef, InvalidZipNames, Unset.type])
          : ZipRef raises PathError =

    Navigable.decode[ZipRef](text)

  @targetName("child")
  infix def / (name: Name[InvalidZipNames]): ZipRef = ZipRef(List(name))

  given ZipRef is Navigable[InvalidZipNames, Unset.type]:
    def root(path: ZipRef): Unset.type = Unset
    def descent(path: ZipRef): List[Name[InvalidZipNames]] = path.descent
    def prefix(ref: Unset.type): Text = t""
    def separator(path: ZipRef): Text = t"/"

  given rootParser: RootParser[ZipRef, Unset.type] with
    def parse(text: Text): (Unset.type, Text) =
      (Unset, if text.length > 0 && text.at(Prim) == '/' then text.skip(1) else text)

  given creator: PathCreator[ZipRef, InvalidZipNames, Unset.type] = (root, descent) => ZipRef(descent)
  given ZipRef is Showable = _.descent.reverse.map(_.render).join(t"/", t"/", t"")

case class ZipRef(descent: List[Name[InvalidZipNames]]):
  def parent: Optional[ZipRef] = descent match
    case Nil       => Unset
    case _ :: tail => ZipRef(tail)
