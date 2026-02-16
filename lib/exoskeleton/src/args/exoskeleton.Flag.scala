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
import fulminate.*
import gossamer.*
import prepositional.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

import language.experimental.pureFunctions

object Flag:
  def serialize(name: Text | Char): Text = name.absolve match
    case char: Char => t"-$char"
    case text: Text => t"--$text"

  given showable: Flag is Showable = _.name.absolve match
    case name: Text => t"--$name"
    case name: Char => t"-$name"

  @targetName("make")
  def apply[topic: Defaulting to Text]
    ( name:        Text | Char,
      repeatable:  Boolean           = false,
      aliases:     List[Text | Char] = Nil,
      description: Optional[Text]    = Unset,
      secret:      Boolean           = false )
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

    cli.register(this, discoverable)


  def matches(key: Argument): Boolean =
    val flag =
      if key().starts(t"--") then key().skip(2) else if key().starts(t"-")
      then key().at(Sec) else Unset

    flag == name || aliases.contains(flag)

  def apply()
    ( using cli:           Cli,
            interpreter:   Interpreter,
            interpretable: Topic is Interpretable,
            suggestions:   (? <: Topic) is Discoverable = Discoverable.noSuggestions )
  :   Optional[Topic] =

    cli.register(this, suggestions)
    cli.parameter(this)


  def select(options: Iterable[Topic])
    ( using cli: Cli, interpreter: Interpreter, suggestible: Topic is Suggestible )
  :   Optional[Topic] =

    val mapping: Map[Text, Topic] =
      options.map { option => (suggestible.suggest(option).text, option) }.to(Map)

    given interpretable: Topic is Interpretable =
      case List(value) => mapping.at(value())
      case _           => Unset

    given suggestions: Topic is Discoverable = _ => options.map(suggestible.suggest(_))

    this()
