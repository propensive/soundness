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
┃    Soundness, version 0.44.0.                                                                    ┃
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
package xylophone

import anticipation.*
import gossamer.*
import rudiments.*
import spectacular.*

class StandardXmlPrinter(compact: Boolean = false) extends XmlPrinter[Text]:
  def print(doc: Xml): Text =
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var pos: Int = 0

    def newline(n: Int = 0): Unit =
      if !compact then
        indent += n
        linebreak = true

    def append(strings: Text*): Unit =
      for str <- strings do
        buf.add(str)
        pos += str.length

    def whitespace(): Unit =
      if !compact && linebreak then
        buf.add(t"\n")
        for i <- 1 to indent do buf.add(t"  ")
        pos = indent*2
      linebreak = false

    def inline(element: XmlAst.Element): Boolean = element.children.all:
      case XmlAst.Textual(_) => true
      case _              => false

    def next(node: XmlAst): Unit = node match
      case element@XmlAst.Element(tagName, children, attributes, namespaces) =>
        whitespace()
        append(t"<", tagName.show)

        for attribute <- attributes do attribute match
          case (key, value) => append(t" ", key.show, t"=\"", value, t"\"")

        if element.children.isEmpty then append(t"/")
        append(t">")
        if !inline(element) then newline(1)

        for child <- element.children do
          val splitLine = child match
            case XmlAst.Textual(_) => false
            case _              => true
          if splitLine then newline()
          next(child)
          if splitLine then newline()

        if !inline(element) then newline(-1)

        whitespace()
        if !element.children.isEmpty then
          append(t"</", tagName.show, t">")
          if !inline(element) then newline(0)

      case XmlAst.Textual(text) =>
        whitespace()
        append(text)

      case XmlAst.ProcessingInstruction(target, content) =>
        whitespace()
        append(t"<?", target, t" ", content, t"?>")
        newline()

      case XmlAst.Comment(content) =>
        whitespace()
        append(t"<!--", content, t"-->")
        newline()

      case e => ()

    doc.root.content.each(next(_))

    buf.text
