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
┃    Soundness, version 0.54.0.                                                                    ┃
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

import anticipation.*
import denominative.*
import gossamer.*
import hypotenuse.*
import rudiments.*
import spectacular.*
import symbolism.*
import vacuous.*

object Argument:
  enum Format:
    case Full, FlagSuffix, EqualityPrefix, EqualitySuffix
    case CharFlag(index: Ordinal)

  def unapply(argument: Argument): Some[Text] = Some(argument())

  given inspectable: Argument is Inspectable = argument =>
    t"${argument.position}: ${argument.value.inspect} / ${argument.format} => ${argument().inspect}"


case class Argument
  ( position: Int,
    value:    Text,
    cursor:   Optional[Int],
    tab:      Optional[Ordinal],
    format:   Argument.Format ):

  override def toString(): String = this.inspect.s

  def wrap(suggestion: Suggestion): Suggestion = format match
    case Argument.Format.Full            => suggestion
    case Argument.Format.FlagSuffix      => suggestion.copy(prefix = value.keep(2))
    case Argument.Format.CharFlag(index) => suggestion // FIXME

    case Argument.Format.EqualityPrefix =>
      suggestion.copy(core = suggestion.core+t"="+value.after(value.ordinal("=").or(Prim)))

    case Argument.Format.EqualitySuffix =>
      val suggestion2 = suggestion.copy(prefix = value.before(value.ordinal("=").or(Prim))+t"=")
      suggestion2

  def apply(): Text = format match
    case Argument.Format.Full            => value
    case Argument.Format.FlagSuffix      => value.skip(2)
    case Argument.Format.CharFlag(index) => t"-${value.at(index + 1).or('-')}"
    case Argument.Format.EqualityPrefix  => value.before(value.ordinal("=").or(Prim))
    case Argument.Format.EqualitySuffix  => value.after(value.ordinal("=").or(Prim))

  def prefix: Optional[Text] = cursor.let(value.keep(_))
  def suffix: Optional[Text] = cursor.let(value.skip(_))

  def contains(ordinal: Ordinal) = format match
    case Argument.Format.Full            => true
    case Argument.Format.FlagSuffix      => ordinal > Sec
    case Argument.Format.CharFlag(index) => ordinal - 2 == index
    case Argument.Format.EqualityPrefix  => value.ordinal("=").or(Prim) > ordinal
    case Argument.Format.EqualitySuffix  => value.ordinal("=").or(Prim) < ordinal

  def suggest(using cli: Cli)(update: (List[Suggestion] aka "prior") ?=> List[Suggestion]) =
    val (prefix, suffix) = format match
      case Argument.Format.Full            => (t"", t"")
      case Argument.Format.FlagSuffix      => (value.keep(2), t"")
      case Argument.Format.CharFlag(index) => (value.before(index + 1), value.after(index + 1))
      case Argument.Format.EqualityPrefix  => (t"", value.after(value.ordinal("=").or(Prim)))
      case Argument.Format.EqualitySuffix  => (value.before(value.ordinal("=").or(Prim)), t"")

    cli.suggest(this, update.aka["prior"], prefix, suffix)

  def select[operand: Suggestible](options: Seq[operand])(using cli: Cli, interpreter: Interpreter)
  :   Optional[operand] =

    val mapping: Map[Text, operand] =
      options.map { option => (operand.suggest(option).text, option) }.to(Map)

    suggest(options.to(List).map(operand.suggest(_)))
    mapping.at(this())
