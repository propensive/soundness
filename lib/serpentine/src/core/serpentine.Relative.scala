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
┃    Soundness, version 0.34.0.                                                                    ┃
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
import spectacular.*
import symbolism.*
import vacuous.*

object Relative:
  @targetName("Up")
  object ^

  @targetName("Self")
  object ? extends Ascent(0):
    type Subject = Zero
    type Constraint = 0

  def of[subject <: Tuple, constraint <: Int](ascent: Int, descent: Text*)
  : Relative of subject under constraint =

      new Relative(ascent, descent.to(List)):
        type Subject = subject
        type Constraint = constraint


  def apply[system, subject <: Tuple, constraint <: Int](ascent: Int, descent: Text*)
  : Relative of subject on system under constraint =

      new Relative(ascent, descent.to(List)):
        type Platform = system
        type Subject = subject
        type Constraint = constraint


  private def conversion[from, to](fn: from => to) =
    new Conversion[from, to]:
      def apply(from: from): to = fn(from)

  given decodable: [system: System]
        =>  (Relative on system) is Decodable in Text = text =>
    if text == system.self then ? else
      text.cut(system.separator).pipe: parts =>
        (if parts.last == t"" then parts.init else parts).pipe: parts =>
          (if parts.head == system.self then parts.tail else parts).pipe: parts =>
            val ascent = parts.takeWhile(_ == system.parent).length
            val descent = parts.drop(ascent).reverse

            Relative(ascent, descent*)

  inline given [subject, ascent <: Int, system]
         =>  Conversion
              [Relative of subject under ascent, Relative of subject under ascent on system] =
    conversion(_.on[system])

  given encodable: [system: System] => Relative on system is Encodable in Text =
    relative =>
      if relative.descent.isEmpty then
        if relative.ascent == 0 then system.self
        else List.fill(relative.ascent)(system.parent).join(system.separator)
      else
        val ascender = system.parent+system.separator
        relative
        . descent
        . reverse
        . join(ascender*relative.ascent, system.separator, t"")

  given showable: [system: System] => Relative on system is Showable = _.encode

  transparent inline given quotient: [system, relative <: (Relative on system) | Text]
                           => relative is Quotient =
    relative0 =>
      relative0 match
        case _: Relative =>
          val relative = relative0.asInstanceOf[Relative on system]

          relative.descent match
            case Nil | _ :: Nil => None
            case _ :: _ :: Nil  => Some((relative.descent(1), relative.descent(0)))
            case _              => Some((relative.descent.last, Relative(0, relative.descent.init*)))

        case _ => None

  : relative is Quotient of Text over (Relative on system) | Text


case class Relative(ascent: Int, descent: List[Text] = Nil):
  type Platform
  type Subject <: Tuple
  type Constraint <: Int

  def delta: Int = descent.length - ascent

  transparent inline def rename(lambda: (prior: Text) ?=> Text): Optional[Relative] =
    descent.prim.let(parent / lambda(using _))

  private inline def check[subject, system](path: List[Text]): Unit =
    inline !![subject] match
      case _: (head *: tail) =>
        infer[head is Admissible on system].check(path.head)
        check[tail, system](path.tail).unit

      case EmptyTuple =>
        ()

      case _ =>
        path.each(infer[Text is Admissible on system].check(_))

  inline def on[system]: Relative of Subject under Constraint on system =
    check[Subject, system](descent.to(List))
    this.asInstanceOf[Relative of Subject under Constraint on system]

  transparent inline def parent = inline !![Subject] match
    case head *: tail => Relative[Platform, tail.type, Constraint](ascent, descent.tail*)
    case EmptyTuple   => Relative[Platform, Zero, S[Constraint]](ascent)

    case _ =>
      if descent.isEmpty then Relative[Platform, Subject, S[Constraint]](ascent + 1)
      else Relative[Platform, Subject, Constraint](ascent, descent.tail*)


  transparent inline def / (child: Any): Relative of (child.type *: Subject) under Constraint =
    summonFrom:
      case given (child.type is Admissible on Platform) =>
        Relative[Platform, child.type *: Subject, Constraint]
          (ascent, infer[child.type is Navigable].follow(child) +: descent*)

      case _ =>
        Relative.of[child.type *: Subject, Constraint]
          (ascent, infer[child.type is Navigable].follow(child) :: descent*)


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
