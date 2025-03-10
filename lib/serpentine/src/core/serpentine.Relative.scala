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

object Relative:
  @targetName("Up")
  object ^

  @targetName("Self")
  object ? extends Ascent(0):
    type Subject = Zero
    type Constraint = 0

  def of[SubjectType <: Tuple, ConstraintType <: Int](ascent: Int, descent: Text*)
  : Relative of SubjectType under ConstraintType =
    new Relative(ascent, descent.to(List)):
      type Subject = SubjectType
      type Constraint = ConstraintType

  def apply[SystemType, SubjectType <: Tuple, ConstraintType <: Int](ascent: Int, descent: Text*)
  : Relative of SubjectType on SystemType under ConstraintType =
    new Relative(ascent, descent.to(List)):
      type Platform = SystemType
      type Subject = SubjectType
      type Constraint = ConstraintType

  private def conversion[FromType, ToType](fn: FromType => ToType) =
    new Conversion[FromType, ToType]:
      def apply(from: FromType): ToType = fn(from)

  given decodable: [SystemType: System]
  =>    (Relative on SystemType) is Decodable in Text = text =>
    if text == SystemType.self then ? else
      text.cut(SystemType.separator).pipe: parts =>
        (if parts.last == t"" then parts.init else parts).pipe: parts =>
          (if parts.head == SystemType.self then parts.tail else parts).pipe: parts =>
            val ascent = parts.takeWhile(_ == SystemType.parent).length
            val descent = parts.drop(ascent).reverse

            Relative(ascent, descent*)

  inline given [SubjectType, AscentType <: Int, SystemType]
  =>    Conversion
         [Relative of SubjectType under AscentType,
          Relative of SubjectType under AscentType on SystemType] =
    conversion(_.on[SystemType])

  given [SystemType: System] => Relative on SystemType is Encodable in Text = relative =>
    if relative.descent.isEmpty then
      if relative.ascent == 0 then SystemType.self
      else List.fill(relative.ascent)(SystemType.parent).join(SystemType.separator)
    else
      val ascender = SystemType.parent+SystemType.separator
      relative
      . descent
      . reverse
      . join(ascender*relative.ascent, SystemType.separator, t"")

case class Relative(ascent: Int, descent: List[Text] = Nil):
  type Platform
  type Subject <: Tuple
  type Constraint <: Int

  def delta: Int = descent.length - ascent

  private inline def check[SubjectType, SystemType](path: List[Text]): Unit =
    inline !![SubjectType] match
      case _: (head *: tail) =>
        summonInline[head is Admissible on SystemType].check(path.head)
        check[tail, SystemType](path.tail)

      case EmptyTuple =>

  inline def on[SystemType]: Relative of Subject under Constraint on SystemType =
    check[Subject, SystemType](descent.to(List))
    this.asInstanceOf[Relative of Subject under Constraint on SystemType]

  transparent inline def parent = inline !![Subject] match
    case head *: tail => Relative[Platform, tail.type, Constraint](ascent, descent.tail*)
    case EmptyTuple   => Relative[Platform, Zero, S[Constraint]](ascent)
    case _ =>
      if descent.isEmpty then Relative[Platform, Subject, S[Constraint]](ascent + 1)
      else Relative[Platform, Subject, Constraint](ascent, descent.tail*)

  transparent inline def / [ChildType](child: ChildType)(using navigable: child.type is Navigable)
  :     Relative of (child.type *: Subject) under Constraint =
    summonFrom:
      case given (child.type is Admissible on Platform) =>
        Relative[Platform, child.type *: Subject, Constraint]
         (ascent, navigable.follow(child) +: descent*)

      case _ =>
        type Subject0 = Subject
        type Constraint0 = Constraint

        Relative.of[child.type *: Subject0, Constraint0]
         (ascent, navigable.follow(child) :: descent*)

// case class Relative(ascent: Int, descent: Text*):
//   type Platform
//   type Subject <: Tuple
//   type Constraint <: Int

// object Relative:

//   given [ElementType] => (Relative by ElementType) is Addable by (Relative by ElementType) into
//           (Relative by ElementType) =
//     (left, right) =>
//       def recur(ascent: Int, descent: List[Text], ascent2: Int): Relative by ElementType =
//         if ascent2 > 0 then
//           if descent.isEmpty then recur(ascent + 1, Nil, ascent - 1)
//           else recur(ascent, descent.tail, ascent - 1)
//         else Relative.from(ascent, right.textDescent ++ descent, left.separator)

//       recur(left.ascent, left.textDescent, right.ascent)
