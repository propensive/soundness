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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package gossamer

import scala.language.experimental.into
import scala.language.experimental.pureFunctions

import fulminate.*
import prepositional.*
import rudiments.*

object Joinable:
  given textual: [textual: Textual] => textual is Joinable = elements =>
    val builder = textual.builder(elements.sumBy(_.length))
    elements.each(builder.append(_))
    builder()

  given message: Message is Joinable = _.fuse(m"")(state+next)

  // The ordered native shapes join by concatenation, and a `Set` by union, so a collection
  // of collections joins with a separator collection interleaved: `List(List(1), List(2))
  // .join(List(0))`. Each instance is written for the shape's plain type: the elements being
  // joined may be intersections (`List[Int] & Populated`, from a literal), and `Self` is
  // instantiated from the instance, widening them to the plain shape.
  given list: [element] => List[element] is Joinable =
    elements => List.from(elements.iterator.flatMap(List.iterator(_)))

  given chain: [element] => Chain[element] is Joinable =
    elements => Chain.from(elements.iterator.flatMap(Chain.iterator(_)))

  given sequence: [element] => Sequence[element] is Joinable =
    elements => Sequence.from(elements.iterator.flatMap(Sequence.iterator(_)))

  given set: [element] => Set[element] is Joinable =
    elements => Set.from(elements.iterator.flatMap(Set.iterator(_)))

  // The receiver-side typeclass for `join`: anything `Traversable` (the native shapes and
  // external `Iterable`s), plus `scala.IArray` (the frozen array's `readable` view), which is
  // neither `Iterable` nor `Traversable`. One typeclass rather than per-receiver extension
  // blocks: overload specificity cannot compare same-name extension alternatives whose clause
  // shapes differ, so a second `join` block would make every receiver ambiguous.
  object Source:
    given traversable: [self, element] => (traversable: self is Traversable by element)
    =>  self is Source by element =
      traversable.traverse(_)

    given iarray: [element] => scala.IArray[element] is Source by element =
      values =>
        scala.collection.immutable.ArraySeq
        . unsafeWrapArray(values.asInstanceOf[scala.Array[element]])
        . iterator

  trait Source extends Typeclass.Pure, Operable:
    def traverse(self: Self): Iterator[Operand]

  // How a receiver's elements (`Operand`) are assembled into the `Result` of `join`, and what
  // type its separators (`Part`) take. Two assemblies exist, tried in priority order, and the
  // same `join` serves both:
  //
  //  - `joinable`: the elements are themselves `Joinable` (text, messages, collections), so
  //    elements and separators alike are `Part`s of that type, and the result is one of them:
  //    `List(t"a", t"b").join(t",")` is `t"a,b"`.
  //  - `interleaving`, the fallback: the elements are not `Joinable`, so the separators are
  //    *elements*, interleaved into a rebuilt collection of the receiver's own shape:
  //    `List(1, 2, 3).join(0)` is `List(1, 0, 2, 0, 3)`.
  //
  // One typeclass rather than a second `join` overload: for a `List[Text]` both readings apply,
  // and overload resolution cannot rank same-name extension alternatives, so the choice is a
  // given-priority decision instead. `Part` is lower-bounded by `Operand` so that the elements
  // can always be passed as parts.
  object Assembly extends Assembly.Fallback:
    given joinable: [self, element, textual >: element]
    =>  (joinable: textual is Joinable)
    =>  ((self is Assembly by element to textual) { type Part = textual }) =
      new Assembly:
        type Self = self
        type Operand = element
        type Result = textual
        type Part = textual

        def assemble(parts: Iterator[textual]): textual = joinable.join(parts.to(Iterable))

    trait Fallback:
      given interleaving: [self, element, result]
      =>  (reshapable: self is Reshapable by element to result)
      =>  ((self is Assembly by element to result) { type Part = element }) =
        new Assembly:
          type Self = self
          type Operand = element
          type Result = result
          type Part = element

          def assemble(parts: Iterator[element]): result = reshapable.reshape(parts)

  trait Assembly extends Typeclass.Pure, Operable, Resultant:
    type Part >: Operand
    def assemble(parts: Iterator[Part]): Result

trait Joinable extends Typeclass.Pure, Operable:
  def join(elements: Iterable[Self]): Self
