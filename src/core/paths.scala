/*
    Serpentine, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import spectacular.*
import gossamer.*

import language.experimental.captureChecking

object Root

object SimplePath:
  inline def parse(text: Text)(using path: CanThrow[PathError]): SimplePath^{path} =
    reachable.parse(text)
  
  given show: Show[SimplePath] = _.render
  given mainRoot: MainRoot[SimplePath] = () => SimplePath(Nil)

  given reachable: ParsableReachable[SimplePath, ".*\\/.*"] with
    type Root = serpentine.Root.type
    def separator(path: SimplePath): Text = t"/"
    def root(path: SimplePath): Root = serpentine.Root
    def prefix(root: Root): Text = t"/"
    
    def make(root: Root, descent: List[PathName[".*\\/.*"]]): SimplePath =
      SimplePath(descent)

    def parseRoot(text: Text): Maybe[(Root, Text)] =
      if text.starts(t"/") then (Root, text.drop(1)) else Unset
    
    def descent(path: SimplePath): List[PathName[".*\\/.*"]] = path.descent
    
case class SimplePath(descent: List[PathName[".*\\/.*"]])

object SimpleLink:
  given show: Show[SimpleLink] = _.render
  inline def parse(text: Text)(using path: CanThrow[PathError]): SimpleLink^{path} =
    followable.parse(text)
  
  given followable: Followable[SimpleLink, ".*\\/.*", "..", "."] with
    def separator(link: SimpleLink): Text = t"/"
    val separators: Set[Char] = Set('/')
    def ascent(path: SimpleLink): Int = path.ascent

    def make(ascent: Int, descent: List[PathName[".*\\/.*"]]): SimpleLink =
      SimpleLink(ascent, descent)

    def descent(path: SimpleLink): List[PathName[".*\\/.*"]] = path.descent
    
case class SimpleLink(ascent: Int, descent: List[PathName[".*\\/.*"]])

package hierarchies:
  erased given simple: Hierarchy[SimplePath, SimpleLink] = ###