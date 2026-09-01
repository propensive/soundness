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
    // Completing inside a clustered short flag (#1888). The characters already typed become a
    // hidden prefix and only the new one is inserted, so choosing `-c` at `-ab` extends the word
    // to `-abc` rather than replacing it; `display` keeps the menu naming the flag as `-c`, and
    // `incomplete` stops the shell terminating the word, so the next letter can follow. A
    // suggestion that is not a flag (an operand value offered at the same position) is left
    // alone: there is no cluster to extend.
    case Argument.Format.CharFlag(index) =>
      if !suggestion.core.starts(t"-") then suggestion
      else
        suggestion.copy
         ( core       = suggestion.core.skip(1),
           prefix     = value,
           display    = suggestion.core,
           incomplete = true )

    case Argument.Format.EqualityPrefix =>
      suggestion.copy(core = suggestion.core+t"="+value.after(value.offsetOf("=").or(Prim)))

    case Argument.Format.EqualitySuffix =>
      val suggestion2 = suggestion.copy(prefix = value.before(value.offsetOf("=").or(Prim))+t"=")
      suggestion2

  def apply(): Text = format match
    case Argument.Format.Full       => value
    case Argument.Format.FlagSuffix => value.skip(2)

    // The `val` gives the deindexing's skolem-dependent type a single widening point (`Char`):
    // inlined directly into the interpolation, the `t` macro's resplice retypes the deindexing
    // and mints a fresh, non-identical skolem (upstream #26563, from 3.9.0-RC5).
    case Argument.Format.CharFlag(index) =>
      val flag = value(index + 1).or('-')
      t"-$flag"

    case Argument.Format.EqualityPrefix => value.before(value.offsetOf("=").or(Prim))
    case Argument.Format.EqualitySuffix => value.after(value.offsetOf("=").or(Prim))

  def prefix: Optional[Text] = cursor.let(value.keep(_))
  def suffix: Optional[Text] = cursor.let(value.skip(_))

  def contains(ordinal: Ordinal) = format match
    case Argument.Format.Full            => true
    case Argument.Format.FlagSuffix      => ordinal > Sec
    case Argument.Format.CharFlag(index) => ordinal - 2 == index
    case Argument.Format.EqualityPrefix  => value.offsetOf("=").or(Prim) > ordinal
    case Argument.Format.EqualitySuffix  => value.offsetOf("=").or(Prim) < ordinal

  def suggest(using cli: Cli)(update: (List[Suggestion] aka "prior") ?=> List[Suggestion]) =
    val (prefix, suffix) = format match
      case Argument.Format.Full            => (t"", t"")
      case Argument.Format.FlagSuffix      => (value.keep(2), t"")
      case Argument.Format.CharFlag(index) => (value.before(index + 1), value.after(index + 1))
      case Argument.Format.EqualityPrefix  => (t"", value.after(value.offsetOf("=").or(Prim)))
      case Argument.Format.EqualitySuffix  => (value.before(value.offsetOf("=").or(Prim)), t"")

    cli.suggest(this, update, prefix, suffix)

  def select[operand: Suggestible](options: List[operand])(using cli: Cli, interpreter: Interpreter)
  :   Optional[operand] =

    val mapping: Map[Text, operand] =
      options.map: option => (operand.suggest(option).text, option)
      . to[Map]

    // Marked as operand values, not subcommands: they are candidates *for* this argument, so the
    // help tree must not descend into them (see `Suggestion.operand`).
    suggest(options.map(operand.suggest(_).copy(operand = true)))
    mapping(this())
