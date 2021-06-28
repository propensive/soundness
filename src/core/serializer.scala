/*
    Honeycomb, version 0.2.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package honeycomb

trait HtmlSerializer[T]:
  def serialize(doc: HtmlDoc, maxWidth: Int = -1): T

object HtmlSerializer:
  given HtmlSerializer[String] = (doc, maxWidth) =>
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var emptyLine: Boolean = true
    var pos: Int = 0
    
    def newline(n: Int = 0): Unit =
      indent += n
      linebreak = true

    def append(strings: String*): Unit =
      for str <- strings do
        buf.append(str)
        pos += str.length
      emptyLine = false

    def whitespace(): Unit =
      if linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
        pos = indent*2
      linebreak = false
      emptyLine = true

    def next(node: Content[?], verbatim: Boolean): Unit = node match
      case node: Item[?] =>
        whitespace()
        append("<", node.label)
        for attribute <- node.attributes do attribute match
          case (key, value: String) => append(" ", key, "=\"", value, "\"")
          case (key, true)          => append(" ", key)
          case (key, false)         => ()
        append(">")
        if !node.inline then newline(1)
        for child <- node.children do
          val splitLine = child match
            case node: Node[?] => !node.inline
            case _             => false
          if splitLine then newline()
          next(child, node.verbatim)
          if splitLine then newline()
        if !node.inline then newline(-1)
        if !node.unclosed then
          whitespace()
          append("</", node.label, ">")
          if !node.inline then newline(0)

      case text: String =>
        whitespace()
        if maxWidth == -1 then append(text) else
          if verbatim || pos + text.length <= maxWidth then append(text)
          else
            text.split("\\s+").foreach { word =>
              if !(pos + 1 + word.length < maxWidth || emptyLine) then
                linebreak = true
                whitespace()
                append(" ")
              append(if !emptyLine then " " else "", word)
            }
            if text.last.isWhitespace then append(" ")
    
    append("<!DOCTYPE html>\n")
    next(doc.root, false)

    buf.toString