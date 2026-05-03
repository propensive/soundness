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

import anticipation.*
import vacuous.*

// Inline parser. Two passes:
//   1. Tokenize the paragraph's raw text into a doubly-linked list of
//      `InlineNode`s, including delimiter-run nodes for `*`/`_`.
//   2. Run the CommonMark emphasis algorithm over the delimiter runs in the
//      list, wrapping matched openers/closers as `EmphasisData`/`StrongData`.
// The resulting list is then converted to a `Seq[Prose]`.

object InlineParser:
  def parse(text: Text, refs: LinkRefs): Seq[Prose] =
    val s = text.s
    val n = s.length

    // CommonMark §4.8: strip trailing whitespace from the paragraph's raw
    // content before inline parsing. Per-line trailing whitespace is kept so
    // hard-break detection still works at internal `\n` positions.
    var end = n
    while end > 0 && (s.charAt(end - 1) == ' ' || s.charAt(end - 1) == '\t') do end -= 1

    val list = InlineList()
    val pending = new StringBuilder

    def flushPending(): Unit =
      if pending.length > 0 then
        list.append(TextData(Text(pending.toString)))
        pending.clear()

    var i = 0
    while i < end do
      val c = s.charAt(i)
      c match
        case '\\' =>
          if i + 1 < end then
            val nextCh = s.charAt(i + 1)
            if nextCh == '\n' then
              flushPending()
              list.append(LinebreakData)
              i += 2
              while i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1
            else if InlineSupport.isAsciiPunctuation(nextCh) then
              pending.append(nextCh)
              i += 2
            else
              pending.append('\\')
              i += 1
          else
            pending.append('\\')
            i += 1

        case '&' =>
          InlineSupport.parseEntity(s, i, end) match
            case e: EntityMatch =>
              pending.append(e.decoded)
              i = e.end

            case Unset =>
              pending.append('&')
              i += 1

        case '`' =>
          InlineSupport.parseCodeSpan(s, i, end) match
            case cs: CodeSpanMatch =>
              flushPending()
              list.append(CodeData(cs.content))
              i = cs.end

            case Unset =>
              pending.append('`')
              i += 1

        case '<' =>
          InlineSupport.parseAutolink(s, i, end) match
            case al: AutolinkMatch =>
              flushPending()
              al.link match
                case link: Prose.Link =>
                  list.append(LinkData(link.destination, link.title, link.prose))

                case other =>
                  list.append(TextData(Text(other.toString)))

              i = al.end

            case Unset =>
              pending.append('<')
              i += 1

        case '\n' =>
          var j = pending.length
          var spaces = 0
          while j > 0 && pending.charAt(j - 1) == ' ' do
            j -= 1
            spaces += 1
          pending.setLength(j)
          flushPending()
          if spaces >= 2 then list.append(LinebreakData)
          else list.append(SoftbreakData)
          i += 1
          while i < end && (s.charAt(i) == ' ' || s.charAt(i) == '\t') do i += 1

        case '*' | '_' =>
          // Delimiter run: count consecutive same-char characters.
          var j = i
          while j < end && s.charAt(j) == c do j += 1
          val length = j - i

          // Flanking detection uses the source characters immediately before
          // the run and immediately after.
          val prevChar = if i == 0 then ' ' else s.charAt(i - 1)
          val nextChar = if j >= end then ' ' else s.charAt(j)

          val (canOpen, canClose) =
            EmphasisProcessor.classifyDelim(c, prevChar, nextChar)

          flushPending()
          list.append(DelimData(c, length, canOpen, canClose))
          i = j

        case _ =>
          pending.append(c)
          i += 1

    flushPending()

    // Pass 2: emphasis processing
    EmphasisProcessor.process(list, null)

    EmphasisProcessor.toProse(list)
