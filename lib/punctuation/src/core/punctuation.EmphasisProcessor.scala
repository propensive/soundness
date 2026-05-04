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
import vacuous.*

// Working representation for inline parsing. The inline parser builds a
// doubly-linked list of `InlineNode`s, including delimiter-run nodes for
// `*`/`_`. `processEmphasis` then walks the linked list, pairing openers
// with closers per the CommonMark rules and inserting `EmphasisData` /
// `StrongData` wrappers in their place.

sealed trait InlineData

case class TextData(text: Text)                                                extends InlineData
case class CodeData(code: Text)                                                extends InlineData
case class HtmlInlineData(html: Text)                                          extends InlineData
case object SoftbreakData                                                      extends InlineData
case object LinebreakData                                                      extends InlineData
case class LinkData
  ( dest: Text, title: Optional[Text], children: List[InlineNode] )
extends InlineData

case class ImageData
  ( dest: Text, title: Optional[Text], children: List[InlineNode] )
extends InlineData
case class EmphasisData(children: List[InlineNode])                            extends InlineData
case class StrongData(children: List[InlineNode])                              extends InlineData

// `[` or `![` marker placed in the inline list when a link/image bracket
// opens. After tokenization the corresponding `BracketEntry` in the parser's
// bracket stack tells us whether to wrap as a link, leave as literal text,
// or skip (if the marker was deactivated by a containing link).
final class BracketData(val isImage: Boolean) extends InlineData

// `*` or `_` delimiter run. `length` may be reduced as the emphasis
// algorithm consumes characters from the run.
final class DelimData
  ( val char: Char,
    var length: Int,
    val canOpen: Boolean,
    val canClose: Boolean )
extends InlineData

final class InlineNode(var data: InlineData):
  var prev: InlineNode | Null = null
  var next: InlineNode | Null = null


final class InlineList:
  var first: InlineNode | Null = null
  var last:  InlineNode | Null = null

  def append(data: InlineData): InlineNode =
    val node = InlineNode(data)
    node.prev = last
    if last == null then first = node else last.nn.next = node
    last = node
    node

  def insertAfter(node: InlineNode, newNode: InlineNode): Unit =
    newNode.prev = node
    newNode.next = node.next
    if node.next == null then last = newNode else node.next.nn.prev = newNode
    node.next = newNode

  def remove(node: InlineNode): Unit =
    val p = node.prev
    val n = node.next
    if p == null then first = n else p.next = n
    if n == null then last  = p else n.prev = p
    node.prev = null
    node.next = null

  def iterator: Iterator[InlineNode] = new Iterator[InlineNode]:
    var cur: InlineNode | Null = first
    def hasNext: Boolean = cur != null

    def next(): InlineNode =
      val n = cur.nn
      cur = n.next
      n


object EmphasisProcessor:

  // CommonMark left-flanking: not followed by Unicode whitespace AND
  // (not followed by Unicode punctuation OR preceded by Unicode whitespace
  // or punctuation).
  def isLeftFlanking(prevChar: Char, nextChar: Char): Boolean =
    if isUnicodeWhitespace(nextChar) then return false
    if !isUnicodePunctuation(nextChar) then return true
    isUnicodeWhitespace(prevChar) || isUnicodePunctuation(prevChar)

  def isRightFlanking(prevChar: Char, nextChar: Char): Boolean =
    if isUnicodeWhitespace(prevChar) then return false
    if !isUnicodePunctuation(prevChar) then return true
    isUnicodeWhitespace(nextChar) || isUnicodePunctuation(nextChar)

  def isUnicodeWhitespace(c: Char): Boolean =
    c == ' ' || c == '\t' || c == '\n' || c == '\r' || c == ''
    || Character.getType(c) == Character.SPACE_SEPARATOR

  def isUnicodePunctuation(c: Char): Boolean =
    val t = Character.getType(c)
    // CommonMark §6.2: Unicode punctuation means a character in categories
    // Pc, Pd, Pe, Pf, Pi, Po, Ps, OR in any Symbol category Sc/Sk/Sm/So.
    t == Character.CONNECTOR_PUNCTUATION
    || t == Character.DASH_PUNCTUATION
    || t == Character.START_PUNCTUATION
    || t == Character.END_PUNCTUATION
    || t == Character.INITIAL_QUOTE_PUNCTUATION
    || t == Character.FINAL_QUOTE_PUNCTUATION
    || t == Character.OTHER_PUNCTUATION
    || t == Character.CURRENCY_SYMBOL
    || t == Character.MODIFIER_SYMBOL
    || t == Character.MATH_SYMBOL
    || t == Character.OTHER_SYMBOL

  // Result is bit-packed (bit 0 = canOpen, bit 1 = canClose) so the hot path
  // doesn't allocate a Tuple2 per delimiter run.
  def classifyDelim(char: Char, prevChar: Char, nextChar: Char): Int =
    val left = isLeftFlanking(prevChar, nextChar)
    val right = isRightFlanking(prevChar, nextChar)

    val canOpen =
      if char == '*' then left
      else left && (!right || isUnicodePunctuation(prevChar))

    val canClose =
      if char == '*' then right
      else right && (!left || isUnicodePunctuation(nextChar))

    (if canOpen then 1 else 0) | (if canClose then 2 else 0)

  inline def hasOpen(flags: Int): Boolean = (flags & 1) != 0
  inline def hasClose(flags: Int): Boolean = (flags & 2) != 0

  // openers_bottom in the spec is keyed by `(char, length%3, canOpen)`. There
  // are exactly 12 keys (2 × 3 × 2), so a fixed Array beats a Map. Layout:
  // [* : 0..5][_ : 6..11], stride 2 over lenMod3, +1 for canOpen.
  private inline def floorIdx
    ( inline char: Char, inline lenMod3: Int, inline canOpen: Boolean )
  :   Int =

    (if char == '*' then 0 else 6) + lenMod3 * 2 + (if canOpen then 1 else 0)

  // Implements CommonMark's "process emphasis" algorithm (§6.2). Walks the
  // doubly-linked list, pairing matching opener/closer delimiter runs and
  // wrapping the inline nodes between them as `EmphasisData` / `StrongData`.
  def process(list: InlineList, stackBottom: InlineNode | Null): Unit =
    // Locate first delimiter strictly above stackBottom
    var current: InlineNode | Null =
      if stackBottom == null then list.first else stackBottom.next

    val openersBottom = new Array[InlineNode | Null](12)
    if stackBottom != null then
      var k = 0
      while k < 12 do { openersBottom(k) = stackBottom; k += 1 }

    inline def floorOf(closer: DelimData): InlineNode | Null =
      openersBottom(floorIdx(closer.char, closer.length % 3, closer.canOpen))

    while current != null do
      val curNode = current
      val curData = curNode.data
      curData match
        case closer: DelimData if closer.canClose =>
          val floor = floorOf(closer)
          var opener: InlineNode | Null = curNode.prev
          var matched = false

          while opener != null && opener != floor && opener != stackBottom && !matched do
            val openerNode = opener
            openerNode.data match
              case od: DelimData if od.canOpen && od.char == closer.char =>
                val ruleOf3 =
                  (od.canClose || closer.canOpen)
                  && (od.length + closer.length) % 3 == 0
                  && (od.length % 3 != 0 || closer.length % 3 != 0)

                if !ruleOf3 then
                  val strong = od.length >= 2 && closer.length >= 2
                  val take = if strong then 2 else 1

                  // Collect children strictly between opener and closer
                  val children = mutable.ListBuffer[InlineNode]()
                  var cursor: InlineNode | Null = openerNode.next
                  while cursor != null && cursor != curNode do
                    val n = cursor
                    val nx = n.next
                    list.remove(n)
                    children += n
                    cursor = nx

                  val wrapper =
                    if strong then InlineNode(StrongData(children.toList))
                    else InlineNode(EmphasisData(children.toList))

                  list.insertAfter(openerNode, wrapper)

                  od.length -= take
                  closer.length -= take

                  if od.length == 0 then list.remove(openerNode)
                  if closer.length == 0 then
                    val nxt = curNode.next
                    list.remove(curNode)
                    current = nxt
                    matched = true
                  else
                    matched = true
                    // Continue with the now-shorter closer at the same node
                else
                  opener = openerNode.prev

              case _ => opener = openerNode.prev

          if !matched then
            openersBottom(floorIdx(closer.char, closer.length % 3, closer.canOpen)) = curNode.prev
            if !closer.canOpen then
              // Remove from the delimiter "stack" but keep its characters as
              // literal text in the inline list.
              val asText = TextData(Text(closer.char.toString * closer.length))
              curNode.data = asText
              current = curNode.next
            else
              current = curNode.next

        case _ => current = curNode.next

  // Convert the (post-emphasis) inline list to a Seq[Prose] for embedding
  // in a Layout.Paragraph or Layout.Heading.
  def toProse(list: InlineList): Seq[Prose] =
    val builder = mutable.ListBuffer[Prose]()
    var cur: InlineNode | Null = list.first
    while cur != null do
      appendProse(cur, builder)
      cur = cur.next
    builder.toSeq

  private def appendProse(node: InlineNode, builder: mutable.ListBuffer[Prose]): Unit =
    node.data match
      case TextData(t)         => builder += Prose.Textual(t)
      case CodeData(c)         => builder += Prose.Code(c)
      case HtmlInlineData(h)   => builder += Prose.HtmlInline(h)
      case SoftbreakData       => builder += Prose.Softbreak
      case LinebreakData       => builder += Prose.Linebreak
      case LinkData(d, t, ch)  => builder += Prose.Link(d, t, childProse(ch)*)
      case ImageData(d, t, ch) => builder += Prose.Image(d, t, childProse(ch)*)
      case EmphasisData(ch)    => builder += Prose.Emphasis(childProse(ch)*)
      case StrongData(ch)      => builder += Prose.Strong(childProse(ch)*)
      case b: BracketData      => builder += Prose.Textual(Text(if b.isImage then "![" else "["))
      case d: DelimData        => appendUnmatchedDelim(d, builder)

  private def appendUnmatchedDelim(d: DelimData, builder: mutable.ListBuffer[Prose]): Unit =
    if d.length > 0 then builder += Prose.Textual(Text(d.char.toString.repeat(d.length).nn))

  private def childProse(children: List[InlineNode]): Seq[Prose] =
    val builder = mutable.ListBuffer[Prose]()
    children.foreach(appendProse(_, builder))
    builder.toSeq
