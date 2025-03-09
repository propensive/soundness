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

import anticipation.*
import contingency.*
import prepositional.*
import rudiments.*

import scala.compiletime.*, ops.int.*

// object Relative:
//   def of[PlatformType, AscentType <: Int, SubjectType <: Tuple](ascent: Int, descent: Text*)
//   :     Relative on PlatformType of SubjectType under AscentType =

//     new Relative(ascent, descent*):
//       type Subject = SubjectType
//       type Platform = PlatformType
//       type Constraint = AscentType

// case class Relative(ascent: Int, descent: Text*):
//   type Platform
//   type Subject <: Tuple
//   type Constraint <: Int

//   transparent inline def parent: Relative = inline !![Subject] match
//     case head *: tail => Relative.of[Platform, Constraint, tail.type](ascent, descent.tail*)
//     case EmptyTuple   => Relative.of[Platform, S[Constraint], EmptyTuple](ascent + 1)
//     case _ =>
//       given Tactic[PathError] = summonInline[Tactic[PathError]]

//       if descent.isEmpty then
//         raise(PathError(PathError.Reason.RootParent))
//         Relative.of[Platform, Constraint, Tuple](ascent, descent*)

//       else Relative.of[Platform, Constraint, Tuple](ascent, descent.tail*)

// object Relative:
//   def parse[ElementType](using navigable: Navigable by ElementType)(text: Text)
//   :     Relative by ElementType =
//     def recur(start: Int, ascent: Int, elements: List[ElementType]): Relative by ElementType =
//       if start >= text.length then Relative(ascent, elements)
//       else
//         val end = text.s.indexOf(navigable.separator.s, start).puncture(-1).or(text.length)
//         val element = text.s.substring(start, end).nn.tt
//         val start2 = end + navigable.separator.length

//         if element == navigable.parentElement then
//           if elements.isEmpty then recur(start2, ascent + 1, Nil)
//           else recur(start2, ascent, elements.tail)
//         else recur(start2, ascent, navigable.element(element) :: elements)

//     if text == navigable.selfText then Relative(0, Nil) else recur(0, 0, Nil)


//   given [ElementType] => (Relative by ElementType) is Addable by (Relative by ElementType) into
//           (Relative by ElementType) =
//     (left, right) =>
//       def recur(ascent: Int, descent: List[Text], ascent2: Int): Relative by ElementType =
//         if ascent2 > 0 then
//           if descent.isEmpty then recur(ascent + 1, Nil, ascent - 1)
//           else recur(ascent, descent.tail, ascent - 1)
//         else Relative.from(ascent, right.textDescent ++ descent, left.separator)

//       recur(left.ascent, left.textDescent, right.ascent)

// abstract class Relative(val ascent: Int, val textDescent: List[Text], val separator: Text)
// extends Pathlike:
//   type Operand

//   def delta: Int = textDescent.length - ascent

//   def parent: Relative =
//     if textDescent.isEmpty then Relative.from(ascent + 1, Nil, separator)
//     else Relative.from(ascent, textDescent.tail, separator)
