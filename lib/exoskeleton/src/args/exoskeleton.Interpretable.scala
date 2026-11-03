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
package exoskeleton

import scala.caps
import scala.compiletime.summonInline
import scala.language.experimental.pureFunctions

import anticipation.*
import contingency.*
import distillate.*
import fulminate.Hazard
import rudiments.{head, reverse}
import gossamer.*
import prepositional.*
import vacuous.*

object Interpretable:
  given unit: Unit is Interpretable:
    override def operand: Boolean = false
    def interpret(arguments: List[Argument]): Unit = ()

  given decoder: [operand: Decodable in Text] => operand is Interpretable = arguments =>
    arguments.head.let(_().as[operand])

  // A pure instance for an operand that may be absent or malformed: either is `Unset`, nothing
  // raises, so it meets `Flag.apply`'s pure requirement. The decoder is summoned at the
  // expansion site over the tactic it may raise through, as `validate` does, and run under
  // `safely`, so the instance itself captures nothing. `Optional[operand]` is a union, which no
  // type parameter can be inferred through, so the instances are per type, each made here:
  // `given Optional[Port] is Interpretable = Interpretable.optional[Port]` for another.
  inline given optionalInt: Optional[Int] is Interpretable = optional[Int]
  inline given optionalLong: Optional[Long] is Interpretable = optional[Long]
  inline given optionalDouble: Optional[Double] is Interpretable = optional[Double]

  inline def optional[operand]: Optional[operand] is Interpretable =
    new Interpretable:
      type Self = Optional[operand]

      def interpret(arguments: List[Argument]): Optional[Optional[operand]] =
        arguments.head.let: argument =>
          safely[Hazard]:
            summonInline
              [ (tactic: Tactic[Hazard]^) ?=> (operand is Decodable in Text)^{tactic, caps.any} ]

            . decoded(argument())

  // Every operand of a flag, each interpreted on its own, so a repeatable flag's occurrences
  // (`--tag a --tag b`) read as `List(a, b)`; one malformed operand makes the whole `Unset`. As
  // pure as its element's instance: `List[Text]` and `List[Optional[Int]]` read in the pure
  // section, `List[Int]` through `validate`.
  given list: [element]
  =>  ( element: (element is Interpretable)^ )
  =>  ( (List[element] is Interpretable)^{element} ) =
    new Interpretable:
      type Self = List[element]
      override def operand: Boolean = element.operand
      override def placeholder: Optional[Text] = element.placeholder

      def interpret(arguments: List[Argument]): Optional[List[element]] =
        def recur(arguments: List[Argument], done: List[element]): Optional[List[element]] =
          arguments match
            case Nil          => done.reverse

            case head :: tail =>
              element.interpret(List(head)).let: value => recur(tail, value :: done)

        recur(arguments, Nil)

trait Interpretable extends Typeclass:
  def operand: Boolean = true

  // The display name for this operand type in `help` output, e.g. the `file` of `--log <file>`.
  // `Unset` means the type suggests no particular name, and a generic `value` is shown instead.
  def placeholder: Optional[Text] = Unset

  // The name to display for the operand, or `Unset` if the type takes no operand at all.
  def operandName: Optional[Text] = if operand then placeholder.or(t"value") else Unset

  def interpret(arguments: List[Argument]): Optional[Self]

  def map[self2](lambda: Self => self2): (self2 is Interpretable)^{this, lambda} =
    new Interpretable:
      type Self = self2
      override def operand: Boolean = Interpretable.this.operand
      override def placeholder: Optional[Text] = Interpretable.this.placeholder

      def interpret(arguments: List[Argument]): Optional[Self] =
        Interpretable.this.interpret(arguments).let(lambda(_))
