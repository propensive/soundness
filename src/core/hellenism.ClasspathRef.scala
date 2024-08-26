/*
    Hellenism, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package hellenism

import anticipation.*
import contingency.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*

object ClasspathRef:
  type Forbidden = "" | ".*\\/.*"

  inline given (using Tactic[PathError]) => Decoder[ClasspathRef] as decoder:
    def decode(text: Text): ClasspathRef = Navigable.decode[ClasspathRef](text)

  given ClasspathRef is Nominable = _.text

  given ClasspathRef is Navigable[Forbidden, Classpath.type]:
    def root(ref: ClasspathRef): Classpath.type = Classpath
    def prefix(classpathCompanion: Classpath.type): Text = t""
    def descent(ref: ClasspathRef): List[Name[Forbidden]] = ref.descent
    def separator(ref: ClasspathRef): Text = t"/"

  given creator: PathCreator[ClasspathRef, Forbidden, Classpath.type] = (_, descent) => ClasspathRef(descent)
  given rootParser: RootParser[ClasspathRef, Classpath.type] = (Classpath, _)
  given ClasspathRef is Showable = _.text

  given (using Classloader, Tactic[ClasspathError]) => ClasspathRef is Readable by Bytes =
    _().stream[Bytes]

case class ClasspathRef(descent: List[Name[ClasspathRef.Forbidden]]):
  def text: Text = descent.reverse.map(_.render).join(t"/")
  def apply()(using classloader: Classloader): Resource = Resource(classloader, this)
  def exists()(using classloader: Classloader): Boolean =
    classloader.java.getResource(text.s) != null
