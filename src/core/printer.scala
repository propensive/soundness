/*
    Cellulose, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import java.io as ji

import language.experimental.captureChecking

object Printer:
  def print(out: ji.Writer, doc: CodlDoc): Unit =
    
    def recur(node: Nodule, indent: Int): Unit = node match
      case Nodule(data, meta) =>
        meta.mm: meta =>
          for i <- 0 until meta.blank do out.write('\n')
          meta.comments.foreach: comment =>
            for i <- 0 until indent do out.write(' ')
            out.write("#")
            out.write(comment.s)
            out.write('\n')
        
        data match
          case Data(key, children, layout, schema) =>
            for i <- 0 until indent do out.write(' ')
            out.write(key.s)
            
            schema match
              case Field(_, _) =>
                children.foreach:
                  case Nodule(Data(key, _, _, _), _) =>
                    out.write(' ')
                    out.write(key.s)
                  case _ => throw Mistake("Should never match")
                out.write('\n')
              case Struct(_, _) =>
                val ps = children.take(if layout.multiline then layout.params - 1 else layout.params)
                ps.foreach:
                  case Nodule(Data(key, IArray(Nodule(Data(value, _, _, _), _)), _, _), _) =>
                    out.write(' ')
                    out.write(value.s)
                  case matched =>
                    throw Mistake("Should never match")
                
                meta.mm(_.remark).mm: remark =>
                  out.write(" # ")
                  out.write(remark.s)
                
                if layout.multiline then
                  out.write('\n')
                  if children.length >= layout.params then children(layout.params - 1) match
                    case Nodule(Data(key, _, _, _), _) =>
                      for i <- 0 until (indent + 4) do out.write(' ')
                      for ch <- key.chars do
                        out.write(ch)
                        if ch == '\n' then for i <- 0 until (indent + 4) do out.write(' ')
                    case _ => throw Mistake("Should never match")
  
                out.write('\n')
                children.drop(layout.params).foreach(recur(_, indent + 2))
          case Unset =>
            ()
    doc.children.foreach(recur(_, doc.margin))
