/*
    Xylophone, version 0.1.0. Copyright 2018-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package xylophone

trait XmlPrinter[T]:
  def print(doc: Xml): T

object XmlPrinter:
  given XmlPrinter[String] = StandardXmlPrinter(false)

object printers:
  given compact: XmlPrinter[String] = StandardXmlPrinter(true)

class StandardXmlPrinter(compact: Boolean = false) extends XmlPrinter[String]:
  def print(doc: Xml): String =
    var indent: Int = 0
    var linebreak: Boolean = false
    val buf: StringBuilder = StringBuilder()
    var pos: Int = 0

    def newline(n: Int = 0): Unit =
      if !compact then
        indent += n
        linebreak = true

    def append(strings: String*): Unit =
      for str <- strings do
        buf.append(str)
        pos += str.length

    def whitespace(): Unit =
      if !compact && linebreak then
        buf.append("\n")
        for i <- 1 to indent do buf.append("  ")
        pos = indent*2
      linebreak = false

    def inline(element: Ast.Element): Boolean = element.children.forall {
      case Ast.Text(_) => true
      case _       => false
    }

    def next(node: Ast): Unit = node match
      case element@Ast.Element(tagName, children, attributes, namespaces) =>
        whitespace()
        append("<", tagName.toString)

        for attribute <- attributes do attribute match
          case (key, value) => append(" ", key.toString, "=\"", value, "\"")
        
        if element.children.isEmpty then append("/")
        append(">")
        if !inline(element) then newline(1)

        for child <- element.children do
          val splitLine = child match
            case Ast.Text(_) => false
            case _           => true
          if splitLine then newline()
          next(child)
          if splitLine then newline()

        if !inline(element) then newline(-1)

        whitespace()
        if !element.children.isEmpty then
          append("</", tagName.toString, ">")
          if !inline(element) then newline(0)

      case Ast.Text(text) =>
        whitespace()
        append(text)

      case Ast.ProcessingInstruction(target, content) =>
        whitespace()
        append("<?", target, " ", content, "?>")
        newline()

      case Ast.Comment(content) =>
        whitespace()
        append("<!--", content, "-->")
        newline()

      case e => ()

    doc.root.content.foreach(next(_))

    buf.toString