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
package phoenicia

import scala.compiletime.constValue

import anticipation.*
import gossamer.*
import prepositional.*
import spectacular.*

object Typeface:
  // A typeface is named by its family name, which is also its `Topic`: the value
  // `Typeface["Inter"]` has the type `Typeface of "Inter"`, so a provision for one typeface is
  // distinguishable from a provision for another at compile time. The name is what a document
  // writes where it refers to the typeface: a CSS `font-family`, a PDF `/BaseFont`.
  inline def apply[family <: Label]: Typeface of family = named[family](constValue[family].tt)
  private def named[family <: Label](name: Text): Typeface of family =
    new Typeface(name) { type Topic = family }

  // The CSS generic families. A browser sets these from whatever it has, so they need no
  // provision; cataclysm supplies their `Typesettable in Web` instances.
  val SansSerif: Typeface of "sans-serif" = Typeface["sans-serif"]
  val Serif: Typeface of "serif" = Typeface["serif"]
  val Monospace: Typeface of "monospace" = Typeface["monospace"]
  val Cursive: Typeface of "cursive" = Typeface["cursive"]
  val Fantasy: Typeface of "fantasy" = Typeface["fantasy"]
  val SystemUi: Typeface of "system-ui" = Typeface["system-ui"]

  given showable: [typeface <: Typeface] => typeface is Showable = _.name

  given inspectable: [typeface <: Typeface] => typeface is Inspectable =
    typeface => t"Typeface[${typeface.name.inspect}]"

class Typeface(val name: Text) extends Topical:
  type Topic <: Label

  // The typeface's regular face, from which the others are chosen.
  def face: Face.Shape[Topic, 400, "upright", Nothing] =
    type family = Topic

    new Face(this, Weight.Regular, Slant.Upright, Stretch.Normal, Nil, Nil):
      type Topic = family
      type Weights = 400
      type Slanting = "upright"
      type Enabled = Nothing

  def regular: Face.Shape[Topic, 400, "upright", Nothing] = face
  def light: Face.Shape[Topic, 300, "upright", Nothing] = face.light
  def medium: Face.Shape[Topic, 500, "upright", Nothing] = face.medium
  def semibold: Face.Shape[Topic, 600, "upright", Nothing] = face.semibold
  def bold: Face.Shape[Topic, 700, "upright", Nothing] = face.bold
  def black: Face.Shape[Topic, 900, "upright", Nothing] = face.black
  def italic: Face.Shape[Topic, 400, "italic", Nothing] = face.italic

  override def equals(that: Any): Boolean = that match
    case other: Typeface => other.name == name
    case _               => false

  override def hashCode: Int = name.hashCode
  override def toString: String = name.s
