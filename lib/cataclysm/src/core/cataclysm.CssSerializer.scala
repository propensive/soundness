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
package cataclysm

import anticipation.*
import gossamer.*
import parasite.*
import spectacular.*
import vacuous.*
import zephyrine.*

// Serializes a `Css` tree back to CSS text. Output is produced lazily through a
// `zephyrine.Producer` (the same streaming mechanism Honeycomb uses for HTML), so
// a large stylesheet never needs to be held in memory at once. The output style
// is chosen by a contextual `CssFormatting` (`formatting.standardCssFormatting` or
// `.compact`).
object CssSerializer:
  def emit(css: Css)(using CssFormatting, Monitor, Probate): Iterator[Text] =
    val producer = Producer[Text](4096)

    async:
      write(css)(producer.put(_))
      producer.finish()

    producer.iterator

  def render(css: Css)(using CssFormatting): Text =
    Producer.collect[Text](): producer =>
      write(css)(producer.put(_))

  private def write(css: Css)(put: Text => Unit)(using formatter: CssFormatting): Unit =
    def newline(indent: Int): Unit = if formatter.newlines then put(indentText(indent))

    def block(body: List[Css.Node], indent: Int): Unit =
      put(if formatter.spaces then t" {" else t"{")

      body.foreach: child =>
        newline(indent + 1)
        emitNode(child, indent + 1)

      newline(indent)
      put(t"}")

    def emitNode(node: Css.Node, indent: Int): Unit = node match
      case Css.Node.Rule(selector, body) =>
        put(selector.show)
        block(body, indent)

      case Css.Node.Declaration(property, value) =>
        put(property)
        put(if formatter.spaces then t": " else t":")
        put(value)
        put(t";")

      case Css.Node.At(name, prelude, body) =>
        put(t"@$name")
        if prelude != t"" then put(t" $prelude")

        body.lay(put(t";")): nodes =>
          block(nodes, indent)

    var first = true

    css.rules.foreach: child =>
      if first then first = false else newline(0)
      emitNode(child, 0)

    if formatter.newlines then put(t"\n")

  private def indentText(indent: Int): Text = ("\n" + " "*(2*indent)).tt
