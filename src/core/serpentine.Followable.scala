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
import vacuous.*
import spectacular.*
import anticipation.*
import gossamer.*
import contingency.*
import prepositional.*
import symbolism.*

object Followable:
  def add[LinkType <: Matchable, NameType <: Label]
      (using creator: PathCreator[LinkType, NameType, Int], followable: LinkType is Followable[NameType, ?, ?])
          : LinkType is Addable by LinkType =
    new Addable:
      type Self = LinkType
      type Result = LinkType
      type Operand = LinkType

      def add(left: LinkType, right: LinkType): LinkType =
        val ascent2 =
          if followable.descent(left).length < followable.ascent(right)
          then followable.ascent(left) + followable.ascent(right) - followable.descent(left).length
          else followable.ascent(left)

        val descent2 =
          followable.descent(right) ++ followable.descent(left).drop(followable.ascent(right))

        creator.path(ascent2, descent2)

  inline def decoder[LinkType <: Matchable](using path: Tactic[PathError])
      [NameType <: Label, ParentRefType <: Label, SelfRefType <: Label]
      (using followable: LinkType is Followable[NameType, ParentRefType, SelfRefType],
             creator: PathCreator[LinkType, NameType, Int])
          : Decoder[LinkType] =

    new Decoder[LinkType]:
      def decode(text: Text): LinkType =
        import followable.*

        val foundSeparator: Char = unsafely(text.where(separators.contains(_)).let(text.at(_))).or('/')
        val ascentPrefix: Text = t"$parentRef$foundSeparator"

        def recur(text: Text, ascent: Int = 0): LinkType =
          if text.starts(ascentPrefix) then recur(text.skip(ascentPrefix.length), ascent + 1)
          else if text == parentRef then creator.path(ascent + 1, Nil)
          else
            val names = text.cut(foundSeparator).to(List).reverse match
              case t"" :: tail => tail
              case names       => names

            creator.path(ascent, names.map(Name(_)))

        if text == selfRef then creator.path(0, Nil) else recur(text)

@capability
trait Followable[NameType <: Label, ParentRefType <: Label, SelfRefType <: Label]
    (using ValueOf[ParentRefType], ValueOf[SelfRefType])
extends Directional[NameType, Int]:
  val parentRef: Text = Text(summon[ValueOf[ParentRefType]].value)
  val selfRef: Text = Text(summon[ValueOf[SelfRefType]].value)
  def separators: Set[Char]
  def ascent(path: Self): Int

  def ancestor[Self2 <: Self](link: Self, n: Int)
      (using creator: PathCreator[Self2, NameType, Int])
          : Self2 =

    val depth = descent(link).length
    creator.path(ascent(link) + (if n > depth then n - depth else 0), descent(link).drop(n))

  override def parent[Self2 <: Self](path: Self)
      (using creator: PathCreator[Self2, NameType, Int])
          : Self2 =

    ancestor(path, 1)

  def render(path: Self): Text =
    val prefix = t"${t"$parentRef${separator(path)}"*(ascent(path))}"

    if descent(path).isEmpty then
      if ascent(path) == 0 then selfRef
      else t"${t"$parentRef${separator(path)}"*(ascent(path) - 1)}$parentRef"
    else t"$prefix${descent(path).reverse.map(_.render).join(separator(path))}"
