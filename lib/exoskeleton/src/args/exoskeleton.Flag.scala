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

import scala.language.experimental.pureFunctions

import scala.compiletime.*

import anticipation.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import spectacular.*
import vacuous.*

object Flag:
  def serialize(name: Text | Char): Text = name.absolve match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

  given showable: Flag is Showable = _.name.absolve match
    case name: Text => t"--$name"
    case name: Char => t"-$name"

  @targetName("make")
  def apply[topic]
    ( name:        Text | Char,
      repeatable:  Boolean           = false,
      aliases:     List[Text | Char] = Nil,
      description: Optional[Text]    = Unset,
      secret:      Boolean           = false )
    ( using erased defaulting: topic is Defaulting to Text )
  :   Flag of topic =

    new Flag(name, repeatable, aliases, description, secret):
      type Topic = topic


case class Flag
  ( name:        Text | Char,
    repeatable:  Boolean,
    aliases:     List[Text | Char],
    description: Optional[Text],
    secret:      Boolean )
extends Topical:
  def suggest(using interpretable: Topic is Interpretable, discoverable: Topic is Discoverable)
    ( using cli: Cli )
  :   Unit =

    cli.register(this, discoverable, interpretable.operandName)


  def matches(key: Argument): Boolean =
    val flag =
      if key().starts(t"--") then key().skip(2) else if key().starts(t"-")
      then key()(Sec) else Unset

    flag == name || aliases.stdlib.contains(flag)


  // Both `apply` and `require` dispatch on the erased `Effectful` capability, which only an
  // `execute` block provides. In the pure section they return a handle — a `Prospective` or a
  // `Requisite` — resolved eagerly from the arguments; inside `execute` they resolve directly,
  // to an `Optional` value and to the value itself respectively.
  transparent inline def apply()
    ( using cli:           Cli,
            interpreter:   Interpreter,
            interpretable: Topic is Interpretable,
            suggestions:   (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Prospective[Topic] | Optional[Topic] =

    cli.register(this, suggestions, interpretable.operandName)

    summonFrom:
      case _: Effectful => cli.parameter[Topic](this)
      case _            => Prospective(this, cli.parameter[Topic](this))


  // Requiring a flag in the pure section never fails fast: there may be several missing
  // requirements, and they are accrued on the `Cli` so that `execute` can report them all in
  // one friendly message. Inside `execute`, where the guard has already run, a missing flag
  // raises a `MissingFlagError` immediately instead, for the backstop to handle.
  transparent inline def require()
    ( using cli:           Cli,
            interpreter:   Interpreter,
            interpretable: Topic is Interpretable,
            suggestions:   (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Requisite[Topic] | Topic =

    cli.register(this, suggestions, interpretable.operandName)
    val value = cli.parameter[Topic](this)
    cli.demand(this, value.present)

    summonFrom:
      case _: Effectful =>
        // Unchecked deliberately: the failure is a user error, reported by the backstop, not a
        // condition for the application to handle.
        import scala.unsafeExceptions.canThrowAny
        import fulminate.errorDiagnostics.emptyDiagnostics
        value.or(throw MissingFlagError(this))

      case _ =>
        Requisite(this, value)


  transparent inline def select(options: Iterable[Topic])
    ( using cli: Cli, interpreter: Interpreter, suggestible: Topic is Suggestible )
  :   Prospective[Topic] | Optional[Topic] =

    val mapping: Map[Text, Topic] =
      (options.map { option => (suggestible.suggest(option).text, option) }).to(Map)

    given interpretable: Topic is Interpretable =
      case List(value) => mapping(value())
      case _           => Unset

    // Marked as operand values, not subcommands: they are candidates for this flag's operand, so
    // the help tree must not descend into them (see `Suggestion.operand`).
    given suggestions: Topic is Discoverable =
      _ => options.map(suggestible.suggest(_).copy(operand = true))

    this()
