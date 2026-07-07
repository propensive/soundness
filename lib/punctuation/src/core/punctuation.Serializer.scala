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
package punctuation

import anticipation.*
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*
import zephyrine.*

// Round-trips a `Markdown` AST back into CommonMark source text. The output is not byte-identical
// to whatever the parser originally consumed, but is guaranteed to re-parse to an equal AST
// (modulo `Ordinal` `line` metadata). This is internal machinery driving the `Markdown … is
// Showable` instances (use `markdown.show`); it is shared by the `Layout` and `Prose` instances,
// so it stays a single private engine rather than being inlined into each.
private[punctuation] object Serializer:
  def apply(markdown: Markdown of Layout)(using formatting: Markdown.Formatting): Text =
    Producer.collect[Text](): producer =>
      writeDocument(Writer(producer, formatting.width.lay(Int.MaxValue) { c => c }), markdown)

  @scala.annotation.targetName("applyProse")
  def apply(markdown: Markdown of Prose)(using formatting: Markdown.Formatting): Text =
    Producer.collect[Text](): producer =>
      flow
        ( Writer(producer, formatting.width.lay(Int.MaxValue) { c => c }),
          markdown.children,
          protectFirst = false )

  private def writeDocument(writer: Writer, markdown: Markdown of Layout): Unit =
    var first = true

    markdown.children.each: node =>
      if !first then writer.blankLine()
      first = false
      layout(writer, node)

    linkRefs(writer, markdown.linkRefs)

  private def linkRefs(writer: Writer, refs: List[Markdown.LinkRef]): Unit =
    if !refs.nil then
      if writer.written && !writer.atLineStart then writer.newline()

      refs.each: ref =>
        writer.raw(t"[")
        writer.raw(escapeLinkLabel(ref.label))
        writer.raw(t"]: ")
        writer.raw(linkDestination(ref.destination))

        ref.title.let: title =>
          writer.raw(t" \"")
          writer.raw(title.sub(t"\"", t"\\\""))
          writer.raw(t"\"")

        writer.newline()

  // Characters that, if unescaped in a `Textual` node, could start a new
  // inline construct when the output is re-parsed. We escape conservatively:
  // every occurrence, not only "active" ones. CommonMark is happy to accept
  // backslash escapes ahead of any ASCII punctuation character.
  private inline def inlineSpecial(char: Char): Boolean = char match
    case '\\' | '`' | '*' | '_' | '[' | ']' | '<' | '>' | '&' => true
    case _                                                    => false

  // Block-start characters: a paragraph whose first line begins with one of
  // these would re-parse as a different block (heading, list, blockquote,
  // thematic break, etc.). We rewrite the first character with a backslash
  // escape so the round-trip lands on `Paragraph` again.
  private inline def blockStart(char: Char): Boolean = char match
    case '#' | '>' | '-' | '+' | '*' | '=' | '_' | '~' | '`' | '|' => true
    case _                                                         => false

  private def escapeTextual(text: Text): Text =
    val s = text.s
    val buf = StringBuilder()
    var i = 0

    while i < s.length do
      val c = s.charAt(i)
      if inlineSpecial(c) then buf.add('\\')
      buf.add(c)
      i += 1

    buf.text

  private def escapeLinkLabel(text: Text): Text =
    val s = text.s
    val buf = StringBuilder()
    var i = 0

    while i < s.length do
      val c = s.charAt(i)
      if c == '\\' || c == '[' || c == ']' then buf.add('\\')
      buf.add(c)
      i += 1

    buf.text

  // Choose the angle-bracket form `<…>` when the destination contains
  // characters that would break the bare form, otherwise emit it directly.
  // Inside angles, only `<` `>` and newline need escaping.
  private def linkDestination(dest: Text): Text =
    val s = dest.s
    var needsAngles = s.isEmpty

    var i = 0

    while i < s.length && !needsAngles do
      val c = s.charAt(i)

      if c == ' ' || c == '\t' || c == '(' || c == ')' || c == '<' || c == '>' || c <= 0x20
      then needsAngles = true

      i += 1

    if !needsAngles then dest else
      val buf = StringBuilder()
      buf.add('<')

      var j = 0

      while j < s.length do
        val c = s.charAt(j)
        if c == '<' || c == '>' || c == '\\' then buf.add('\\')
        if c != '\n' then buf.add(c)
        j += 1

      buf.add('>')
      buf.text

  // Pick the smallest backtick-fence width that does not appear as a run
  // inside `code`. CommonMark §6.1: at least one more backtick than the
  // longest internal run.
  private def codeSpanDelimiter(code: Text): Text =
    val s = code.s
    var longest = 0
    var current = 0
    var i = 0

    while i < s.length do
      if s.charAt(i) == '`' then
        current += 1
        if current > longest then longest = current
      else
        current = 0

      i += 1

    t"`"*(longest + 1)

  // The needed extra-space padding for code spans whose content begins or
  // ends with a backtick or is wholly whitespace.
  private def codeSpanNeedsPadding(code: Text): Boolean =
    val s = code.s

    if s.isEmpty then false
    else if s.charAt(0) == '`' || s.charAt(s.length - 1) == '`' then true
    else
      var blank = true
      var i = 0

      while i < s.length && blank do
        val c = s.charAt(i)
        if c != ' ' && c != '\n' then blank = false
        i += 1

      blank

  private def codeBlockFence(code: Text): Text =
    val s = code.s
    var longestRun = 2
    var current = 0
    var i = 0

    while i < s.length do
      val c = s.charAt(i)

      if c == '`' then
        current += 1
        if current > longestRun then longestRun = current
      else
        current = 0

      i += 1

    t"`"*(longestRun + 1)

  private def prose(builder: StringBuilder, node: Prose): Unit = node match
    case Prose.Textual(text) =>
      builder.add(escapeTextual(text))

    case Prose.Softbreak =>
      builder.add('\n')

    case Prose.Linebreak =>
      builder.add(t"\\\n")

    case Prose.Code(code) =>
      val fence = codeSpanDelimiter(code)
      builder.add(fence)
      if codeSpanNeedsPadding(code) then builder.add(' ')
      builder.add(code)
      if codeSpanNeedsPadding(code) then builder.add(' ')
      builder.add(fence)

    case Prose.Emphasis(children*) =>
      builder.add('*')
      children.each(prose(builder, _))
      builder.add('*')

    case Prose.Strong(children*) =>
      builder.add(t"**")
      children.each(prose(builder, _))
      builder.add(t"**")

    case Prose.Link(destination, title, children*) =>
      builder.add('[')
      children.each(prose(builder, _))
      builder.add(t"](")
      builder.add(linkDestination(destination))

      title.let: t =>
        builder.add(t" \"")
        builder.add(t.sub(t"\"", t"\\\""))
        builder.add(t"\"")

      builder.add(')')

    case Prose.Image(destination, title, children*) =>
      builder.add(t"![")
      children.each(prose(builder, _))
      builder.add(t"](")
      builder.add(linkDestination(destination))

      title.let: t =>
        builder.add(t" \"")
        builder.add(t.sub(t"\"", t"\\\""))
        builder.add(t"\"")

      builder.add(')')

    case Prose.HtmlInline(html) =>
      builder.add(html)

  // Render the inline content of a heading to a fresh `Text`, applying first-character block-start
  // protection so a leading `#` etc. does not re-parse as a new block.
  private def inlineLine(children: Seq[Prose]): Text =
    val buf = StringBuilder()
    children.each(prose(buf, _))
    val out = buf.text

    if out.length == 0 then out
    else if blockStart(out.s.charAt(0)) then t"\\$out"
    else out

  // Render one inline construct (code span, emphasis, link, …) to a `Text`, emitted as a single
  // unbreakable unit by the paragraph word-wrapper.
  private def inlineUnit(node: Prose): Text =
    val buf = StringBuilder()
    prose(buf, node)
    buf.text

  // Render a sequence of blocks to a `Text` — the indented body of a block quote or list item,
  // which is then re-emitted with a line prefix.
  private def render(nodes: Seq[Layout], width: Int): Text =
    Producer.collect[Text](): producer =>
      val inner = Writer(producer, width)
      var first = true

      nodes.each: node =>
        if !first then inner.blankLine()
        first = false
        layout(inner, node)

  private def trimNewline(text: Text): Text = if text.ends(t"\n") then text.skip(1, Rtl) else text

  // Word-wrap a paragraph's inline content: textual nodes break at their spaces, inline constructs
  // are atomic. `protectFirst` backslash-escapes a leading block-start character.
  private def flow(writer: Writer, children: Seq[Prose], protectFirst: Boolean): Unit =
    if protectFirst then writer.protectNext()

    children.each:
      case Prose.Textual(text) => writer.text(escapeTextual(text))
      case Prose.Softbreak     => writer.soft()
      case Prose.Linebreak     => writer.hard()
      case node                => writer.atom(inlineUnit(node))

    writer.endFlow()

  private def layout(writer: Writer, node: Layout): Unit = node match
    case Layout.Heading(_, level, children*) =>
      writer.raw(t"#"*level)
      writer.raw(t" ")
      writer.raw(inlineLine(children))
      writer.newline()

    case Layout.Paragraph(_, children*) =>
      flow(writer, children, protectFirst = true)
      writer.newline()

    case Layout.ThematicBreak(_) =>
      writer.raw(t"---")
      writer.newline()

    case Layout.CodeBlock(_, info, code) =>
      val fence = codeBlockFence(code)
      writer.raw(fence)
      if !info.nil then writer.raw(info.map(_.s).mkString(" ").tt)
      writer.newline()
      writer.raw(code)
      if !code.ends(t"\n") then writer.newline()
      writer.raw(fence)
      writer.newline()

    case Layout.BlockQuote(_, children*) =>
      trimNewline(render(children, writer.width)).cut(t"\n").each: line =>
        if line.length == 0 then writer.raw(t">")
        else
          writer.raw(t"> ")
          writer.raw(line)

        writer.newline()

    case Layout.HtmlBlock(_, html) =>
      writer.raw(html)
      if !html.ends(t"\n") then writer.newline()

    case Layout.BulletList(_, tight, items*) =>
      var first = true

      items.each: item =>
        if !first && !tight then writer.blankLine()
        first = false
        listItem(writer, item, t"- ", t"  ")

    case Layout.OrderedList(_, start, tight, delimiter, items*) =>
      var n = start
      var first = true

      items.each: item =>
        if !first && !tight then writer.blankLine()
        first = false
        val marker = t"$n${delimiter.or('.')} "
        listItem(writer, item, marker, t" "*marker.length)
        n += 1

  // Emit a list item: the first line carries the marker, subsequent lines the hanging indent.
  private def listItem(writer: Writer, item: List[Layout], marker: Text, hanging: Text): Unit =
    if item.nil then writer.newline() else
      var first = true

      trimNewline(render(item, writer.width)).cut(t"\n").each: line =>
        writer.raw(if first then marker else hanging)
        writer.raw(line)
        writer.newline()
        first = false

  // A streaming sink for Markdown source. Block separation, indentation and the first-character
  // block-start escape are carried as state (no looking back at emitted output), and paragraph text
  // is word-wrapped at `width` (the default `Int.MaxValue` never wraps, preserving the AST).
  class Writer(producer: Producer[Text], val width: Int):
    private var trailing = 0          // consecutive trailing newlines just emitted
    private var col = 0               // current column, for wrapping
    private var pendingSpaces = 0     // spaces buffered before the current word
    private val seg = StringBuilder() // the current unbreakable run (word + adjacent atoms)
    private var protect = false       // escape the next word's leading block-start char
    var written: Boolean = false

    def atLineStart: Boolean = col == 0

    // Emit text verbatim (it may contain newlines), tracking the trailing-newline count and column.
    def raw(text: Text): Unit =
      val s = text.s

      if s.length > 0 then
        producer.put(text)
        written = true
        var nls = 0
        var k = s.length

        while k > 0 && s.charAt(k - 1) == '\n' do
          nls += 1
          k -= 1

        if k == 0 then
          trailing += nls
          col = 0
        else
          trailing = nls
          val lastNl = s.lastIndexOf('\n')
          col = if lastNl < 0 then col + s.length else s.length - lastNl - 1

    def newline(): Unit =
      producer.put(t"\n")
      written = true
      trailing += 1
      col = 0

    // Ensure blocks are separated by a blank line (two trailing newlines), once content exists.
    def blankLine(): Unit =
      if written then
        if trailing == 0 then
          newline()
          newline()
        else if trailing == 1 then
          newline()

    def protectNext(): Unit = protect = true

    def text(content: Text): Unit =
      val s = content.s
      var i = 0

      while i < s.length do
        if s.charAt(i) == ' ' then
          flushSeg()
          pendingSpaces += 1
        else
          seg.add(s.charAt(i))

        i += 1

    def atom(content: Text): Unit = seg.add(content)

    def soft(): Unit =
      flushSeg()
      pendingSpaces = 0
      newline()

    def hard(): Unit =
      flushSeg()
      pendingSpaces = 0
      raw(t"\\")
      newline()

    def endFlow(): Unit = flushSeg()

    private def flushSeg(): Unit =
      if seg.length > 0 then
        val word = seg.text
        seg.clear()

        if pendingSpaces > 0 then
          if width != Int.MaxValue && col > 0 && col + pendingSpaces + word.length > width
          then newline()
          else raw(t" "*pendingSpaces)

          pendingSpaces = 0

        if protect then
          protect = false
          if blockStart(word.s.charAt(0)) then raw(t"\\")

        raw(word)
