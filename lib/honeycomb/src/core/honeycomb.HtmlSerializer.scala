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
┃    Soundness, version 0.38.0.                                                                    ┃
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
package honeycomb

import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.*
import spectacular.*
import vacuous.*
import xylophone.*

trait HtmlSerializer[result]:
  def serialize(doc: HtmlDoc, maxWidth: Optional[Int] = Unset): result

object HtmlSerializer:
  given text: HtmlSerializer[Text] = (doc, maxWidth) =>
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var emptyLine: Boolean = true
    var pos: Int = 0

    def newline(n: Int = 0): Unit =
      indent += n
      linebreak = true

    def append(strings: Text*): Unit =
      for str <- strings do
        buf.add(str)
        pos += str.length

      emptyLine = false

    def whitespace(): Unit =
      if linebreak then
        buf.add(t"\n")
        for i <- 1 to indent do buf.add(t"  ")
        pos = indent*2
      linebreak = false
      emptyLine = true

    def next(node: Html[?], verbatim: Boolean): Unit = node.absolve match
      case HtmlXml(xml) =>
        append(Xml.print(xml))

      case node: Node[?] =>
        whitespace()
        append(t"<", node.label)

        for attribute <- node.attributes do attribute.absolve match
          case (key: Text, value: Text) => append(t" ", key, t"=\"", value, t"\"")
          case (key: Text, Unset)       => append(t" ", key)

        append(t">")
        if node.block then newline(1)

        for child <- node.children do
          val splitLine = child match
            case element: Element[?] => element.block
            case _                   => false
          if splitLine then newline()
          next(child, node.verbatim)
          if splitLine then newline()

        if node.block then newline(-1)

        if !node.unclosed then
          whitespace()
          append(t"</", node.label, t">")
          if node.block then newline(0)

      case text: Text =>
        whitespace()
        if maxWidth.absent then append(text) else
          if verbatim || pos + text.length <= maxWidth.or(0) then append(text)
          else
            text.cut(t"\\s+").nn.each: word =>
              if !(pos + 1 + word.nn.length < maxWidth.or(0) || emptyLine) then
                linebreak = true
                whitespace()
                append(t" ")

              append(if !emptyLine then t" " else t"", word.nn)

            if text.chars.last.isWhitespace then append(t" ")

      case Unset =>
        ()


    append(t"<!DOCTYPE html>\n")
    next(doc.root, false)

    buf.text
