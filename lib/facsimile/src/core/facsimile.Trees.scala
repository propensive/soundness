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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package facsimile

import proscenium.compat.*

import anticipation.*
import contingency.*
import gossamer.*
import rudiments.*
import vacuous.*

// Name trees and number trees (ISO 32000-2 §7.9.6–7.9.7): balanced trees of `/Kids` whose
// leaves carry sorted `/Names` or `/Nums` pair arrays. Both flatten to their in-order pairs,
// with reference cycles guarded.
private[facsimile] object Trees:
  def names(root: Cos)(using Pdf): List[(Text, Cos)] raises PdfError =
    pairs(root, t"Names", scala.collection.immutable.Set()).bind: (key, value) =>
      key.text.let(text => List((text, value))).or(List())

  def numbers(root: Cos)(using Pdf): List[(Long, Cos)] raises PdfError =
    pairs(root, t"Nums", scala.collection.immutable.Set()).bind: (key, value) =>
      key.long.let(number => List((number, value))).or(List())

  private def pairs(node: Cos, key: Text, visited: scala.collection.immutable.Set[Int])(using pdf: Pdf)
  :   List[(Cos, Cos)] raises PdfError =

    node match
      case Cos.Ref(number, _) =>
        if visited.has(number) then List()
        else pairs(pdf.resolved(node), key, visited + number)

      case Cos.Dictionary(entries) =>
        entries.at(t"Kids").let(pdf.resolved(_).elements).lay(leaf(entries, key)): kids =>
          kids.bind(pairs(_, key, visited))

      case _ =>
        List()

  private def leaf(entries: Map[Text, Cos], key: Text)(using pdf: Pdf)
  :   List[(Cos, Cos)] raises PdfError =

    pdf.resolved(entries.at(key).or(Cos.Nil)).elements.lay(List()): elements =>
      elements.grouped(2).to(List).bind:
        case List(key, value) => List((pdf.resolved(key), value))
        case _                => List()
