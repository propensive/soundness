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

import gossamer.*
import rudiments.*

import language.experimental.captureChecking

object Relative:
  @targetName("RelativeRoot")
  object `?` extends Relative[GenericPath](0, Nil)
  
  case class Self[ForbiddenType <: Label]() extends PathRoot:
    def prefix = t"./"
  
  given [ForbiddenType <: Label]: PathBounds[Relative[ForbiddenType]] = path =>
    path match
      case Relative(ascent, head :: tail) => Relative(ascent, tail)
      case Relative(ascent, Nil)          => Relative(ascent + 1, Nil)
  
  given Show[Relative[? <: Label]] = rel =>
    if rel == ? then t"."
    else (List.fill(rel.ascent)(t"..") ::: rel.elements.reverse.map(_.show)).join(t"", t"/", t"")

  given [PathType](using hierarchy: Hierarchy[PathType])
      : Hierarchy[Relative[hierarchy.Forbidden]] =
    new Hierarchy[Relative[hierarchy.Forbidden]]:
      type Forbidden = hierarchy.Forbidden
      type RootType = Self[hierarchy.Forbidden]
      
      def separator: Text = hierarchy.separator
      def selfName: Text = hierarchy.selfName
      def parentName: Text = hierarchy.parentName
      def root(path: Relative[Forbidden]): Self[Forbidden] = Self()
      def elements(path: Relative[Forbidden]): List[PathElement[Forbidden]] = path.elements
      
      def remake
          (path: {*} Relative[Forbidden], elements: List[PathElement[Forbidden]])
          : {path} Relative[Forbidden] =
        Relative(path.ascent, elements)
      
      def parseElement(text: Text): PathElement[Forbidden] throws PathError =
        hierarchy.parseElement(text)
      
  def parse
      [PathType]
      (text: Text)(using hierarchy: Hierarchy[PathType])
      : Relative[hierarchy.Forbidden] throws PathError =
    
    val parentPrefix = t"${hierarchy.parentName}${hierarchy.separator}"
    def recur(text: Text, ascent: Int): Relative[hierarchy.Forbidden] =
      if text == hierarchy.selfName then Relative(0, Nil)
      else if text == hierarchy.parentName then Relative(ascent + 1, Nil)
      else if text.starts(parentPrefix) then recur(text.drop(parentPrefix.length), ascent + 1)
      else
        val elements = text.cut(hierarchy.separator).filter(_ != t"").map(hierarchy.parseElement(_))
        Relative(ascent, elements.reverse)
    
    recur(text, 0)

case class Relative[ForbiddenType <: Label](ascent: Int, elements: List[PathElement[ForbiddenType]])

export Relative.`?`