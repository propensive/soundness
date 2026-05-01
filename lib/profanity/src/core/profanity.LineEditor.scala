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
package profanity

import anticipation.*
import contingency.*
import denominative.*
import fulminate.*
import gossamer.{where as _, *}
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*

case class LineEditor(value: Text = t"", position0: Optional[Int] = Unset) extends Question[Text]:
  val position = position0.or(value.length)

  import Keypress.*

  def apply(keypress: TerminalEvent): LineEditor = try keypress match
    case CharKey(ch) => copy(t"${value.keep(position)}$ch${value.skip(position)}", position + 1)
    case Ctrl('U')   => copy(value.skip(position), 0)
    case Delete      => copy(t"${value.keep(position)}${value.skip(position + 1)}")
    case Home        => copy(position0 = 0)
    case End         => copy(position0 = value.length)
    case Left        => copy(position0 = (position - 1) `max` 0)
    case Right       => copy(position0 = (position + 1) `min` value.length)

    case Ctrl('W') =>
      val prefix = value.keep(0 max (position - 1)).skip(_ != ' ', Rtl)
      copy(t"$prefix${value.skip(position)}", prefix.length)

    case Backspace =>
      copy(t"${value.keep(position - 1)}${value.skip(position)}", (position - 1) max 0)

    case Ctrl(Left) =>
      val position2 =
        ((position - 2 `max` 0) to 0 by -1).where: index =>
          value.at(index.z) == ' '

      copy(position0 = position2.lay(0)(_ + 1))

    case Ctrl(Right) =>
      val range = ((position + 1) `min` (value.length - 1)) to (value.length - 1)
      val position2 = range.where { index => value.at(index.z) == ' ' }.lay(value.length)(_ + 1)
      copy(position0 = position2 `min` value.length)

    case _ =>
      this

  catch case e: RangeError => this


  def ask
    ( using interactivity: Interactivity[TerminalEvent],
            interaction:   Interaction[Text, LineEditor] )
    [ result ]
    ( lambda: Interactivity[TerminalEvent] ?=> Text => result )
  :   result raises DismissError =

    interaction(interactivity.eventStream(), this)(_(_)).lay(abort(DismissError())):
      (result, stream) => lambda(using Interactivity(stream))(result)
