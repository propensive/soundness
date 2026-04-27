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
package cellulose

import java.io as ji

import anticipation.*
import gossamer.*
import proscenium.*
import rudiments.*
import vacuous.*

object Printer:
  def print(out: ji.Writer, doc: CodlDoc): Unit =

    @tailrec
    def printBlock(indent: Int, text: Text, start: Int = 0): Unit =
      if start < (text.length - 1) then
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
      case CodlNode(data, extra) =>
        extra.let: extra =>
          for i <- 0 until extra.blank do out.write('\n')
          extra.comments.each: comment =>
            for i <- 0 until indent do out.write(' ')
            out.write("#")
            out.write(comment.s)
            out.write('\n')

        (data: Optional[Atom]) match
          case Atom(key, children, layout, schema) =>
            for i <- 0 until indent do out.write(' ')
            out.write(key.s)

            schema match
              case Field(_) =>
                children.each: child =>
                  child.absolve match
                    case CodlNode(Atom(key, _, layout, _), _) =>
                      out.write("  ")
                      out.write(key.s)
                out.write('\n')

              case Struct(_, _) =>
                val ps = children.take:
                  if layout.multiline then layout.params - 1 else layout.params

                var col = indent - doc.margin + key.length
                ps.each: param =>
                  param.absolve match
                    case CodlNode
                          (Atom(key, IArray(CodlNode(Atom(value, _, layout, _), _)), _, _), _) =>
                      if layout.multiline then
                        out.write('\n')
                        printBlock(indent + 4, key)
                      else
                        val spaces = layout.col - col
                        for i <- 0 until spaces.max(1) do out.write(' ')
                        col += spaces
                        out.write(value.s)
                        col += value.length

                    case CodlNode(Atom(key, IArray(), layout, _), _) =>
                      if layout.multiline then
                        out.write('\n')
                        printBlock(indent + 4, key)
                      else
                        val spaces = layout.col - col
                        for i <- 0 until spaces.max(1) do out.write(' ')
                        col += spaces
                        out.write(key.s)
                        col += key.length

                extra.let(_.remark).let: remark =>
                  out.write(" # ")
                  out.write(remark.s)

                if layout.multiline then
                  out.write('\n')

                  if children.length >= layout.params then children(layout.params - 1).absolve match
                    case CodlNode(Atom(key, _, _, _), _) =>
                      for i <- 0 until (indent + 4) do out.write(' ')
                      for ch <- key.chars do
                        out.write(ch)
                        if ch == '\n' then for i <- 0 until (indent + 4) do out.write(' ')

                out.write('\n')
                children.drop(layout.params).each(recur(_, indent + 2))

          case Unset =>
            ()
    doc.children.each(recur(_, doc.margin))
