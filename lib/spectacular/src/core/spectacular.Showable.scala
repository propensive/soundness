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
┃    Soundness, version 0.46.0.                                                                    ┃
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
package spectacular

import scala.quoted.*

import anticipation.*
import denominative.*
import digression.*
import fulminate.*
import inimitable.*
import prepositional.*
import proscenium.*
import rudiments.*
import stenography.*
import vacuous.*

trait Showable extends Typeclass, Communicable:
  def text(value: Self): Text
  def message(value: Self): Message = Message(text(value))

object Showable:
  given showable: [value: Textualizable] => value is Showable = value.textual(_)

  given text: [text <: Text] => text is Showable = identity(_)
  given string: String is Showable = _.tt
  given char: Char is Showable = char => char.toString.tt
  given long: Long is Showable = long => long.toString.tt
  given int: Int is Showable = int => int.toString.tt
  given short: Short is Showable = short => short.toString.tt
  given byte: Byte is Showable = byte => byte.toString.tt
  given message: Message is Showable = _.text
  given double: (decimalizer: DecimalConverter) => Double is Showable = decimalizer.decimalize(_)
  given boolean: (affirmation: Affirmation) => Boolean is Showable = affirmation(_)
  given option: [value: Showable] => Option[value] is Showable = _.fold("none".tt)(value.text(_))
  given uuid: Uuid is Showable = _.text
  given memory: Memory is Showable = _.text
  given enumeration: [enumeration <: reflect.Enum] => enumeration is Showable = _.toString.tt

  given set: [element: Showable] => Set[element] is Showable =
    _.map(_.show).mkString("{", ", ", "}").tt

  given list: [element: Showable] => List[element] is Showable =
    _.map(_.show).mkString("[", ", ", "]").tt

  given trie: [element: Showable] => Trie[element] is Showable =
    _.map(_.show).mkString("[ ", " ", " ]").tt

  given none: None.type is Showable = none => "none".tt

  given specializable: Specializable is Showable = value =>
    value.getClass.nn.getName.nn.split("\\.").nn.last.nn.dropRight(1).toLowerCase.nn.tt

  given zerary: Ordinal is Showable = ordinal => s"${ordinal.n0}.₀".tt

  given typeRepr: (quotes: Quotes) => quotes.reflect.TypeRepr is Showable = repr =>
    Stenography.name(using repr.asType)

  given meta: [meta] => (quotes: Quotes) => Type[meta] is Showable =
    Stenography.name[meta](using _)

  given stackTrace: StackTrace is Showable = stack =>
    val methodWidth = stack.frames.map(_.method.method.s.length).maxOption.getOrElse(0)
    val classWidth = stack.frames.map(_.method.className.s.length).maxOption.getOrElse(0)
    val fileWidth = stack.frames.map(_.file.s.length).maxOption.getOrElse(0)
    val fullClass = s"${stack.component}.${stack.className}".tt
    val init = s"$fullClass: ${stack.message}".tt

    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.method.className.s.endsWith("#")
        val drop = if obj then 1 else 0
        val file = (" "*(fileWidth - frame.file.s.length))+frame.file
        val dot = if obj then ".".tt else "#".tt
        val className = frame.method.className.s.dropRight(drop)
        val classPad = (" "*(classWidth - className.length)).tt
        val method = frame.method.method
        val methodPad = (" "*(methodWidth - method.s.length)).tt
        val line = frame.line.let(_.show).or("?".tt)

        s"$msg\n  at $classPad$className$dot$method$methodPad $file:$line".tt

    stack.cause.lay(root): cause =>
      s"$root\ncaused by:\n$cause".tt
