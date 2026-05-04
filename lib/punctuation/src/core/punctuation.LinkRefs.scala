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
package punctuation

import scala.collection.mutable

import anticipation.*

// Accumulates link reference definitions discovered during the block-parse
// pass; consulted by the inline-parse pass for shortcut/collapsed/full
// reference links. Stage 4 will populate this from `[label]: dest "title"`
// definitions in paragraphs; for now it stays empty.
final class LinkRefs:
  private val table: mutable.LinkedHashMap[Text, Markdown.LinkRef] =
    mutable.LinkedHashMap()

  // CommonMark normalisation: trim, collapse internal whitespace runs, then
  // case-fold. We use `toUpperCase` rather than `toLowerCase` because Java's
  // upper-casing handles a broader set of Unicode case-equivalences (e.g.
  // ẞ ↔ SS) that the spec's case-fold semantics require.
  def normalize(label: Text): Text =
    val s = label.s
    val n = s.length
    val builder = new StringBuilder
    var i = 0
    var inSpace = false
    var trimmingLeft = true
    while i < n do
      val c = s.charAt(i)
      if c == ' ' || c == '\t' || c == '\n' || c == '\r' then
        if !trimmingLeft then inSpace = true
      else
        if inSpace then builder.append(' ')
        builder.append(c)
        inSpace = false
        trimmingLeft = false
      i += 1
    Text(builder.toString.toUpperCase.nn)

  // First definition wins (per CommonMark).
  def add(ref: Markdown.LinkRef): Unit =
    val key = normalize(ref.label)
    if !table.contains(key) then table(key) = ref

  def lookup(label: Text): vacuous.Optional[Markdown.LinkRef] =
    table.get(normalize(label)) match
      case Some(ref) => ref
      case None      => vacuous.Unset

  def all: List[Markdown.LinkRef] = table.values.to(List)
