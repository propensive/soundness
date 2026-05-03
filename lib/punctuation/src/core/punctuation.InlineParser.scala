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
import gossamer.*
import vacuous.*

// Inline parser. Two passes:
//   1. Tokenize the paragraph's raw text into a doubly-linked list of
//      `InlineNode`s — including delimiter-run nodes for `*`/`_` and
//      bracket-marker nodes for `[`/`![` — and try-match links/images on
//      `]`. Successful link matches wrap the bracketed content as
//      `LinkData` / `ImageData` and process emphasis within it.
//   2. After tokenization, run a final emphasis pass over any remaining
//      delimiters outside successfully-matched links.
// The resulting list is then converted to a `Seq[Prose]`.

object InlineParser:

  // Bracket stack entry. `node` is the BracketData node in the inline list
  // marking the `[` or `![`. `sourceStart` is the source position
  // immediately AFTER the opening bracket (used to extract the literal
  // bracket content for shortcut/collapsed reference labels). `active` is
  // false when a containing link has been matched (no nested links).
  private final class BracketEntry
    ( val node:        InlineNode,
      val isImage:     Boolean,
      val sourceStart: Int,
      var active:      Boolean = true )

  def parse(text: Text, refs: LinkRefs): Seq[Prose] =
    val s = text.s
    val n = s.length

    var end = n
    while end > 0 && (s.charAt(end - 1) == ' ' || s.charAt(end - 1) == '\t') do end -= 1

    val list = InlineList()
    val pending = new StringBuilder
    val brackets = mutable.Stack[BracketEntry]()

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
                  val child = InlineNode(TextData(link.prose.head match
                    case Prose.Textual(t) => t
                    case _                => link.destination))
                  list.append(LinkData(link.destination, link.title, List(child)))

                case _ =>
                  list.append(TextData(Text(al.link.toString)))

              i = al.end

            case Unset =>
              InlineSupport.parseRawHtml(s, i, end) match
                case h: InlineSupport.HtmlInlineMatch =>
                  flushPending()
                  list.append(HtmlInlineData(h.html))
                  i = h.end

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
          var j = i
          while j < end && s.charAt(j) == c do j += 1
          val length = j - i

          val prevChar = if i == 0 then ' ' else s.charAt(i - 1)
          val nextChar = if j >= end then ' ' else s.charAt(j)

          val (canOpen, canClose) =
            EmphasisProcessor.classifyDelim(c, prevChar, nextChar)

          flushPending()
          list.append(DelimData(c, length, canOpen, canClose))
          i = j

        case '[' =>
          flushPending()
          val node = list.append(BracketData(isImage = false))
          brackets.push(BracketEntry(node, isImage = false, sourceStart = i + 1))
          i += 1

        case '!' if i + 1 < end && s.charAt(i + 1) == '[' =>
          flushPending()
          val node = list.append(BracketData(isImage = true))
          brackets.push(BracketEntry(node, isImage = true, sourceStart = i + 2))
          i += 2

        case ']' =>
          flushPending()
          val newPos = handleCloseBracket(list, brackets, s, i, end, refs)
          i = newPos

        case _ =>
          pending.append(c)
          i += 1

    flushPending()

    EmphasisProcessor.process(list, null)
    EmphasisProcessor.toProse(list)

  // Handle a closing `]` at source position `closePos`. Returns the new
  // source index to continue from.
  private def handleCloseBracket
    ( list:     InlineList,
      brackets: mutable.Stack[BracketEntry],
      s:        String,
      closePos: Int,
      end:      Int,
      refs:     LinkRefs )
  :   Int =

    if brackets.isEmpty then
      list.append(TextData(t"]"))
      return closePos + 1

    val entry = brackets.pop()

    if !entry.active then
      // Inactive marker: drop bracket node, emit literal `]`
      list.remove(entry.node)
      list.append(TextData(t"]"))
      return closePos + 1

    // Try to match link/image syntax starting at the character after `]`
    val afterClose = closePos + 1
    tryMatchLink(s, afterClose, end, entry, refs) match
      case Unset =>
        // No link match: bracket and `]` become literal text
        list.remove(entry.node)
        list.append(TextData(if entry.isImage then t"![" else t"["))
        list.append(TextData(t"]"))
        afterClose

      case lm: LinkResolution =>
        // Process emphasis in the bracket content (between bracket node and
        // current end of list), so emphasis WITHIN the link is wrapped first.
        EmphasisProcessor.process(list, entry.node)

        // Collect children: everything strictly after the bracket node, to
        // the end of the list.
        val children = mutable.ListBuffer[InlineNode]()
        var cursor: InlineNode | Null = entry.node.next
        while cursor != null do
          val n = cursor
          val nxt = n.next
          list.remove(n)
          children += n
          cursor = nxt

        // Replace bracket node with the link/image wrapper
        list.remove(entry.node)
        if entry.isImage then list.append(ImageData(lm.dest, lm.title, children.toList))
        else
          list.append(LinkData(lm.dest, lm.title, children.toList))
          // Deactivate any earlier `[` markers (no nested links)
          brackets.foreach(_.active = false)

        lm.end

  private case class LinkResolution(dest: Text, title: Optional[Text], end: Int)

  private def tryMatchLink
    ( s:        String,
      after:    Int,
      end:      Int,
      entry:    BracketEntry,
      refs:     LinkRefs )
  :   Optional[LinkResolution] =

    // 1. Inline link: ( dest title? )
    if after < end && s.charAt(after) == '(' then
      InlineSupport.parseInlineLinkBody(s, after, end) match
        case b: InlineSupport.InlineLinkBody =>
          return LinkResolution(b.dest, b.title, b.end)

        case Unset => return Unset

    // 2. Reference forms
    val bracketContent = Text(s.substring(entry.sourceStart, after - 1).nn)

    if after < end && s.charAt(after) == '[' then
      InlineSupport.parseRefLabel(s, after, end) match
        case r: InlineSupport.RefLabelMatch =>
          val label = if r.label.s.isEmpty then bracketContent else r.label
          val resolved = refs.lookup(label)
          if resolved.present then
            val ref = resolved.vouch
            return LinkResolution(ref.destination, ref.title, r.end)
          // ref not found; fall through to shortcut

        case Unset => ()  // fall through to shortcut

    // 3. Shortcut reference
    val resolved = refs.lookup(bracketContent)
    if resolved.present then
      val ref = resolved.vouch
      LinkResolution(ref.destination, ref.title, after)
    else
      Unset
