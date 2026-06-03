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
import denominative.*
import gossamer.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

// Round-trips a `Markdown` AST back into CommonMark source text. The output is
// not byte-identical to whatever the parser originally consumed, but is
// guaranteed to re-parse to an equal AST (modulo `Ordinal` `line` metadata).
object Serializer:
  def apply(markdown: Markdown of Layout): Text =
    val builder = StringBuilder()
    layoutSeq(builder, markdown.children, t"")
    appendLinkRefs(builder, markdown.linkRefs)
    builder.text

  @scala.annotation.targetName("applyProse")
  def apply(markdown: Markdown of Prose): Text =
    val builder = StringBuilder()
    List.from(markdown.children).each(prose(builder, _))
    builder.text

  private def appendLinkRefs(builder: StringBuilder, refs: List[Markdown.LinkRef]): Unit =
    if !refs.nil then
      if builder.length > 0 && !builder.text.ends(t"\n") then builder.add(t"\n")

      refs.each: ref =>
        builder.add('[')
        builder.add(escapeLinkLabel(ref.label))
        builder.add(t"]: ")
        builder.add(linkDestination(ref.destination))
        ref.title.let: title =>
          builder.add(t" \"")
          builder.add(title.sub(t"\"", t"\\\""))
          builder.add('"')
        builder.add('\n')

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
      List.from(children).each(prose(builder, _))
      builder.add('*')

    case Prose.Strong(children*) =>
      builder.add(t"**")
      List.from(children).each(prose(builder, _))
      builder.add(t"**")

    case Prose.Link(destination, title, children*) =>
      builder.add('[')
      List.from(children).each(prose(builder, _))
      builder.add(t"](")
      builder.add(linkDestination(destination))
      title.let: t =>
        builder.add(t" \"")
        builder.add(t.sub(t"\"", t"\\\""))
        builder.add(t"\"")
      builder.add(')')

    case Prose.Image(destination, title, children*) =>
      builder.add(t"![")
      List.from(children).each(prose(builder, _))
      builder.add(t"](")
      builder.add(linkDestination(destination))
      title.let: t =>
        builder.add(t" \"")
        builder.add(t.sub(t"\"", t"\\\""))
        builder.add(t"\"")
      builder.add(')')

    case Prose.HtmlInline(html) =>
      builder.add(html)

  // Render the inline content of a paragraph or heading to a fresh `Text`,
  // applying first-character block-start protection so a leading `#` etc.
  // does not re-parse as a new block.
  private def inlineLine(children: Seq[Prose]): Text =
    val buf = StringBuilder()
    List.from(children).each(prose(buf, _))
    val out = buf.text

    if out.length == 0 then out
    else if blockStart(out.s.charAt(0)) then t"\\$out"
    else out

  private def layoutSeq(builder: StringBuilder, nodes: Seq[Layout], indent: Text): Unit =
    var first = true

    List.from(nodes).each: node =>
      if !first then ensureBlankLine(builder)
      first = false
      layout(builder, node, indent)

  private def ensureBlankLine(builder: StringBuilder): Unit =
    val txt = builder.text

    if txt.length == 0 then ()
    else if txt.ends(t"\n\n") then ()
    else if txt.ends(t"\n") then builder.add('\n')
    else builder.add(t"\n\n")

  private def writeWithIndent(builder: StringBuilder, text: Text, indent: Text): Unit =
    val s = text.s
    var lineStart = true
    var i = 0

    while i < s.length do
      val c = s.charAt(i)
      if lineStart && indent.length > 0 then builder.add(indent)
      builder.add(c)
      lineStart = c == '\n'
      i += 1

  private def layout(builder: StringBuilder, node: Layout, indent: Text): Unit = node match
    case Layout.Heading(_, level, children*) =>
      builder.add(indent)
      builder.add(t"#"*level)
      builder.add(' ')
      builder.add(inlineLine(children))
      builder.add('\n')

    case Layout.Paragraph(_, children*) =>
      writeWithIndent(builder, inlineLine(children), indent)
      builder.add('\n')

    case Layout.ThematicBreak(_) =>
      builder.add(indent)
      builder.add(t"---\n")

    case Layout.CodeBlock(_, info, code) =>
      val fence = codeBlockFence(code)
      builder.add(indent)
      builder.add(fence)
      if !info.nil then builder.add(info.map(_.s).scala.mkString(" ").tt)
      builder.add('\n')
      writeWithIndent(builder, code, indent)
      if !code.ends(t"\n") then builder.add('\n')
      builder.add(indent)
      builder.add(fence)
      builder.add('\n')

    case Layout.BlockQuote(_, children*) =>
      val inner = StringBuilder()
      layoutSeq(inner, children, t"")
      val text = inner.text
      // Strip a single trailing newline so the loop doesn't emit a phantom
      // empty final line.
      val body = if text.ends(t"\n") then text.skip(1, Rtl) else text

      body.cut(t"\n").each: line =>
        builder.add(indent)
        if line.length == 0 then builder.add('>')
        else
          builder.add(t"> ")
          builder.add(line)
        builder.add('\n')

    case Layout.HtmlBlock(_, html) =>
      writeWithIndent(builder, html, indent)
      if !html.ends(t"\n") then builder.add('\n')

    case Layout.BulletList(_, tight, items*) =>
      var first = true

      List.from(items).each: item =>
        if !first && !tight then ensureBlankLine(builder)
        first = false
        builder.add(indent)
        builder.add(t"- ")
        renderItemBody(builder, item, t"$indent  ", tight)

    case Layout.OrderedList(_, start, tight, delimiter, items*) =>
      var n = start
      var first = true

      List.from(items).each: item =>
        if !first && !tight then ensureBlankLine(builder)
        first = false
        val marker = t"$n${delimiter.or('.')} "
        val hangingSpaces = t" "*marker.length
        builder.add(indent)
        builder.add(marker)
        renderItemBody(builder, item, t"$indent$hangingSpaces", tight)
        n += 1

  // A list item is itself a sequence of `Layout` blocks. We emit the first
  // block's first line right after the marker, then everything else with
  // the hanging-indent applied. Tight lists collapse the inter-paragraph
  // blank lines exactly as the parser does.
  private def renderItemBody
    ( builder: StringBuilder, item: List[Layout], hangingIndent: Text, tight: Boolean )
  :   Unit =

    if item.nil then builder.add('\n')
    else
      val inner = StringBuilder()
      layoutSeq(inner, item.scala, t"")
      val rendered = inner.text.s

      var i = 0
      var lineStart = false

      while i < rendered.length do
        val c = rendered.charAt(i)
        if lineStart then builder.add(hangingIndent)
        builder.add(c)
        lineStart = c == '\n'
        i += 1

      if rendered.isEmpty || rendered.charAt(rendered.length - 1) != '\n'
      then builder.add('\n')
