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

import ambience.*
import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Setting:
  @targetName("make")
  def apply[topic]
    ( name:        Text,
      description: Optional[Text]    = Unset,
      aliases:     List[Text | Char] = Nil,
      variable:    Optional[Text]    = Unset,
      secret:      Boolean           = false )
    ( using erased defaulting: topic is Defaulting to Text )
  :   Setting of topic =

    new Setting(name, Flag(name.uncamel.kebab, false, aliases, description, secret), variable):
      type Topic = topic

// A configurable application setting, declared once by its canonical camelCase `name` and read
// from a cascade of configuration sources: its command-line flag (derived as `--kebab-case`)
// always takes priority, then each source of the contextual `Configurator` in composition
// order. `variable` names an environment variable that is consulted (verbatim) between the
// flag and the configurator cascade. Reading a setting registers its flag for shell
// completions, exactly as reading the flag directly would; like a flag, a setting must be
// read outside `execute` to be discoverable in completion mode.
//
// A setting is inherently single-valued, so its command-line operand is read as `Text` and
// decoded by the same `Decodable` as every other source, which may capture a `Tactic` (the
// `Interpretable` bridge cannot, which is why the flag is not read at type `Topic`).
case class Setting(name: Text, flag: Flag, variable: Optional[Text]) extends Topical:
  def apply()
    ( using cli:          Cli,
      interpreter:  Interpreter,
      configurator: Configurator^,
      decodable:    (Topic is Decodable in Text)^,
      suggestions:  (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Optional[Topic] =

    given textOperands: (Text is Discoverable) = Discoverable.noSuggestions[Text]
    val parameter = cli.parameter[Text](flag)

    // Registered after the flag read, whose own registration carries no suggestions, so that
    // this setting's `Discoverable` is the one a completion retains.
    cli.register(flag, suggestions, t"value")

    parameter.let(decodable.decoded(_))
      .or(variable.let(cli.environment.variable(_)).let(decodable.decoded(_)))
      .or(configurator.read(name).let(decodable.decoded(_)))
