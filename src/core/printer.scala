/*
    Cellulose, version [unreleased]. Copyright 2024 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package cellulose

import rudiments.*
import vacuous.*
import anticipation.*
import gossamer.*

import java.io as ji

object CodlPrinter:
  val standardPrinter: CodlPrinter = codl =>
    val writer: ji.Writer = ji.StringWriter()
    Printer.print(writer, codl)
    writer.toString.tt

trait CodlPrinter:
  def serialize(codl: CodlDoc): Text

package codlPrinters:
  given standard: CodlPrinter = CodlPrinter.standardPrinter

object Printer:

  def print(out: ji.Writer, doc: CodlDoc): Unit =

    @tailrec
    def printBlock(indent: Int, text: Text, start: Int = 0): Unit =
      if start < (text.s.length - 1) then
        for i <- 0 until indent do out.write(' ')
        
        text.s.indexOf('\n', start) match
          case -1 =>
            out.write(text.s.substring(start))
            out.write('\n')
          
          case end =>
            out.write(text.s.substring(start, end))
            out.write('\n')
            printBlock(indent, text, end + 1)
      

    def recur(node: CodlNode, indent: Int): Unit = node match
      case CodlNode(data, meta) =>
        meta.let: meta =>
          for i <- 0 until meta.blank do out.write('\n')
          meta.comments.each: comment =>
            for i <- 0 until indent do out.write(' ')
            out.write("#")
            out.write(comment.s)
            out.write('\n')
        
        (data: Optional[Data]) match
          case Data(key, children, layout, schema) =>
            for i <- 0 until indent do out.write(' ')
            out.write(key.s)
            
            schema match
              case Field(_, _) =>
                children.each: child =>
                  (child: @unchecked) match
                    case CodlNode(Data(key, _, layout, _), _) =>
                      out.write(' ')
                      out.write(key.s)
                out.write('\n')

              case Struct(_, _) =>
                val ps = children.take(if layout.multiline then layout.params - 1 else layout.params)
                var col = indent - doc.margin + key.or(t"").length
                ps.each: param =>
                  (param: @unchecked) match
                    case CodlNode(Data(key, IArray(CodlNode(Data(value, _, layout, _), _)), _, _), _) =>
                      if layout.multiline then
                        out.write('\n')
                        printBlock(indent + 4, key)
                      else
                        val spaces = layout.col - col
                        for i <- 0 until spaces.max(1) do out.write(' ')
                        col += spaces
                        out.write(value.s)
                        col += value.length
                    
                    case CodlNode(Data(key, IArray(), layout, _), _) =>
                      if layout.multiline then
                        out.write('\n')
                        printBlock(indent + 4, key)
                      else
                        val spaces = layout.col - col
                        for i <- 0 until spaces.max(1) do out.write(' ')
                        col += spaces
                        out.write(key.s)
                        col += key.length
                
                meta.let(_.remark).let: remark =>
                  out.write(" # ")
                  out.write(remark.s)
                
                if layout.multiline then
                  out.write('\n')
                  
                  if children.length >= layout.params then (children(layout.params - 1): @unchecked) match
                    case CodlNode(Data(key, _, _, _), _) =>
                      for i <- 0 until (indent + 4) do out.write(' ')
                      for ch <- key.chars do
                        out.write(ch)
                        if ch == '\n' then for i <- 0 until (indent + 4) do out.write(' ')
  
                out.write('\n')
                children.drop(layout.params).each(recur(_, indent + 2))
          case Unset =>
            ()
    doc.children.each(recur(_, doc.margin))
