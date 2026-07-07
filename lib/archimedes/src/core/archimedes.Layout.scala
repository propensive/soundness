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

// General layout schemata. The uniform containers (`Mrow`, `Msqrt`, `Mstyle`,
// `Merror`, `Mpadded`, `Mphantom`, `Menclose`, `Mfenced`) hold an ordered list
// of children; each provides a varargs `apply` for ergonomic construction. The
// positional schemata (`Mfrac`, `Mroot`) name their children instead.

sealed trait Layout extends Mathml:
  def text: Optional[Text] = Unset

object Mrow:
  def apply(children: Mathml*): Mrow = Mrow(children.to(List))

case class Mrow(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"mrow"

case class Mfrac(numerator: Mathml, denominator: Mathml, attributes: List[(Text, Text)] = Nil)
extends Layout:
  def label: Text = t"mfrac"
  def contents: List[Mathml] = List(numerator, denominator)

object Msqrt:
  def apply(children: Mathml*): Msqrt = Msqrt(children.to(List))

case class Msqrt(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"msqrt"

case class Mroot(base: Mathml, index: Mathml, attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"mroot"
  def contents: List[Mathml] = List(base, index)

object Mstyle:
  def apply(children: Mathml*): Mstyle = Mstyle(children.to(List))

case class Mstyle(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"mstyle"

object Merror:
  def apply(children: Mathml*): Merror = Merror(children.to(List))

case class Merror(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"merror"

object Mpadded:
  def apply(children: Mathml*): Mpadded = Mpadded(children.to(List))

case class Mpadded(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"mpadded"

object Mphantom:
  def apply(children: Mathml*): Mphantom = Mphantom(children.to(List))

case class Mphantom(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"mphantom"

object Menclose:
  def apply(children: Mathml*): Menclose = Menclose(children.to(List))

case class Menclose(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"menclose"

object Mfenced:
  def apply(children: Mathml*): Mfenced = Mfenced(children.to(List))

case class Mfenced(contents: List[Mathml], attributes: List[(Text, Text)] = Nil) extends Layout:
  def label: Text = t"mfenced"
