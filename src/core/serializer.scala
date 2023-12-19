/*
    Honeycomb, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import vacuous.*
import gossamer.*
import anticipation.*
import spectacular.*

trait HtmlSerializer[ResultType]:
  def serialize(doc: HtmlDoc, maxWidth: Int = -1): ResultType

object HtmlSerializer:
  given HtmlSerializer[Text] = (doc, maxWidth) =>
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

    def next(node: Html[?], verbatim: Boolean): Unit = (node: @unchecked) match
      case node: Node[?] =>
        whitespace()
        append(t"<", node.label)
        
        for attribute <- node.attributes do (attribute: @unchecked) match
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
        if maxWidth == -1 then append(text) else
          if verbatim || pos + text.length <= maxWidth then append(text)
          else
            text.cut(t"\\s+").nn.foreach: word =>
              if !(pos + 1 + word.nn.length < maxWidth || emptyLine) then
                linebreak = true
                whitespace()
                append(t" ")
              
              append(if !emptyLine then t" " else t"", word.nn)
            
            if text.chars.last.isWhitespace then append(t" ")
      
      case int: Int =>
        next(int.show, verbatim)
        
    
    append(t"<!DOCTYPE html>\n")
    next(doc.root, false)

    buf.text
