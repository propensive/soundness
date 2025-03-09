                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.27.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package serpentine

import scala.compiletime.*, ops.int.*

import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*

object Relation:
  @targetName("Up")
  object ^

  @targetName("Self")
  object ? extends Ascent(0):
    type Subject = Zero
    type Constraint = 0

  def of[SubjectType <: Tuple, ConstraintType <: Int](ascent: Int, descent: Text*)
  : Relation of SubjectType under ConstraintType =
    new Relation(ascent, descent.to(List)):
      type Subject = SubjectType
      type Constraint = ConstraintType

  def apply[PlatformType, SubjectType <: Tuple, ConstraintType <: Int](ascent: Int, descent: Text*)
  : Relation of SubjectType on PlatformType under ConstraintType =
    new Relation(ascent, descent.to(List)):
      type Platform = PlatformType
      type Subject = SubjectType
      type Constraint = ConstraintType

  private def conversion[FromType, ToType](fn: FromType => ToType) =
    new Conversion[FromType, ToType]:
      def apply(from: FromType): ToType = fn(from)

  given decodable: [PlatformType: System]
  =>    (Relation on PlatformType) is Decodable in Text = text =>
    if text == PlatformType.self then ? else
      text.cut(PlatformType.separator).pipe: parts =>
        (if parts.last == t"" then parts.init else parts).pipe: parts =>
          (if parts.head == PlatformType.self then parts.tail else parts).pipe: parts =>
            val ascent = parts.takeWhile(_ == PlatformType.parent).length
            val descent = parts.drop(ascent).reverse

            Relation(ascent, descent*)

  inline given [SubjectType, AscentType <: Int, PlatformType]
  =>    Conversion
         [Relation of SubjectType under AscentType,
          Relation of SubjectType under AscentType on PlatformType] =
    conversion(_.on[PlatformType])

  given [PlatformType: System] => Relation on PlatformType is Encodable in Text = relative =>
    if relative.descent.isEmpty then
      if relative.ascent == 0 then PlatformType.self
      else List.fill(relative.ascent)(PlatformType.parent).join(PlatformType.separator)
    else
      val ascender = PlatformType.parent+PlatformType.separator
      relative
      . descent
      . reverse
      . join(ascender*relative.ascent, PlatformType.separator, t"")

case class Relation(ascent: Int, descent: List[Text] = Nil):
  type Platform
  type Subject <: Tuple
  type Constraint <: Int

  def delta: Int = descent.length - ascent

  private inline def check[SubjectType, PlatformType](path: List[Text]): Unit =
    inline !![SubjectType] match
      case _: (head *: tail) =>
        summonInline[head is Admissible on PlatformType].check(path.head)
        check[tail, PlatformType](path.tail)

      case EmptyTuple =>

  inline def on[PlatformType]: Relation of Subject under Constraint on PlatformType =
    check[Subject, PlatformType](descent.to(List))
    this.asInstanceOf[Relation of Subject under Constraint on PlatformType]

  transparent inline def parent = inline !![Subject] match
    case head *: tail => Relation[Platform, tail.type, Constraint](ascent, descent.tail*)
    case EmptyTuple   => Relation[Platform, Zero, S[Constraint]](ascent)
    case _ =>
      if descent.isEmpty then Relation[Platform, Subject, S[Constraint]](ascent + 1)
      else Relation[Platform, Subject, Constraint](ascent, descent.tail*)

  transparent inline def / (child: Any)(using navigable: Navigable by child.type)
  :     Relation of (child.type *: Subject) under Constraint =
    summonFrom:
      case given (child.type is Admissible on Platform) =>
        Relation[Platform, child.type *: Subject, Constraint]
         (ascent, navigable.follow(child) +: descent*)

      case _ =>
        type Subject0 = Subject
        type Constraint0 = Constraint

        Relation.of[child.type *: Subject0, Constraint0]
         (ascent, navigable.follow(child) :: descent*)
