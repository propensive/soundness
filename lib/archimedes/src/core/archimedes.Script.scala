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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package archimedes

import anticipation.*
import gossamer.*
import vacuous.*

// Script and limit schemata. `Msub`/`Msup`/`Msubsup` and `Munder`/`Mover`/
// `Munderover` are positional; `Mmultiscripts` is a container whose children
// interleave base, postscripts, an `Mprescripts` marker and prescripts, using
// `Mnone` as an empty-script placeholder.

sealed trait Script extends Mathml:
  def text: Optional[Text] = Unset

case class Msub(base: Mathml, subscript: Mathml, attributes: List[(Text, Text)] = Nil)
extends Script:
  def label: Text = t"msub"
  def contents: List[Mathml] = List(base, subscript)

case class Msup(base: Mathml, superscript: Mathml, attributes: List[(Text, Text)] = Nil)
extends Script:
  def label: Text = t"msup"
  def contents: List[Mathml] = List(base, superscript)

case class Msubsup
  ( base:        Mathml,
    subscript:   Mathml,
    superscript: Mathml,
    attributes:  List[(Text, Text)] = Nil )
extends Script:
  def label: Text = t"msubsup"
  def contents: List[Mathml] = List(base, subscript, superscript)

case class Munder(base: Mathml, underscript: Mathml, attributes: List[(Text, Text)] = Nil)
extends Script:
  def label: Text = t"munder"
  def contents: List[Mathml] = List(base, underscript)

case class Mover(base: Mathml, overscript: Mathml, attributes: List[(Text, Text)] = Nil)
extends Script:
  def label: Text = t"mover"
  def contents: List[Mathml] = List(base, overscript)

case class Munderover
  ( base:        Mathml,
    underscript: Mathml,
    overscript:  Mathml,
    attributes:  List[(Text, Text)] = Nil )
extends Script:
  def label: Text = t"munderover"
  def contents: List[Mathml] = List(base, underscript, overscript)

object Mmultiscripts:
  def apply(children: Mathml*): Mmultiscripts = Mmultiscripts(children.to(List))

case class Mmultiscripts(contents: List[Mathml], attributes: List[(Text, Text)] = Nil)
extends Script:
  def label: Text = t"mmultiscripts"

case class Mprescripts(attributes: List[(Text, Text)] = Nil) extends Script:
  def label: Text = t"mprescripts"
  def contents: List[Mathml] = Nil

case class Mnone(attributes: List[(Text, Text)] = Nil) extends Script:
  def label: Text = t"mnone"
  def contents: List[Mathml] = Nil
