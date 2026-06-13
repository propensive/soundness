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
import escapade.*
import frontier.*
import gossamer.*
import polysyllabic.*
import prepositional.*
import rudiments.*
import symbolism.*
import vacuous.*

// Renders a parsed `Markdown` document into an Escapade `Teletype`, suitable
// for printing to a terminal. Inline elements (emphasis, strong, code, links,
// images) gain styling and OSC 8 hyperlinks; blocks (headings, lists,
// blockquotes, thematic breaks) gain Unicode glyphs; paragraphs are wrapped
// to the given column width using Polysyllabic for end-of-line hyphenation;
// code blocks are syntax-highlighted via `TeletypeFormattable` givens.
object Renderer:

  // U+2010 HYPHEN — the visible-but-not-soft variant used at wrap points.
  private val Hyphen: Teletype = Teletype(t"‐")
  private val Space: Teletype = Teletype(t" ")
  private val Newline: Teletype = Teletype(t"\n")

  def render
    ( markdown: Markdown of Layout, width: Int )
    ( using hyphenation: Hyphenation,
            formattables: Every[TeletypeFormattable],
            palette: MarkdownPalette )
  :   Teletype =

    val blocks = markdown.children.map(layoutLines(_, width)).filter(_.nonEmpty)
    blocks.map(joinLines(_)).to(Seq).join(e"\n\n")

  // -- inline ---------------------------------------------------------------

  // Renders inline `Prose` to a `Teletype` with the appropriate styling.
  // Softbreaks become a literal space (the wrap pass treats them as word
  // boundaries); linebreaks become the sentinel '\n' so the wrap pass can
  // split on them.
  private def inlineProse(node: Prose)(using palette: MarkdownPalette): Teletype = node match
    case Prose.Textual(text) =>
      Teletype(text)

    case Prose.Softbreak =>
      Space

    case Prose.Linebreak =>
      Newline

    case Prose.Code(code) =>
      e"${Bg(palette.codeBg)}(${Fg(palette.code)}($code))"

    case Prose.Emphasis(children*) =>
      val inner = children.map(inlineProse(_)).to(Seq).join
      e"$Italic($inner)"

    case Prose.Strong(children*) =>
      val inner = children.map(inlineProse(_)).to(Seq).join
      e"$Bold($inner)"

    case Prose.Link(destination, _, children*) =>
      val inner = children.map(inlineProse(_)).to(Seq).join
      val link = Hyperlink(destination)
      e"$link(${Fg(palette.link)}($Underline($inner)))"

    case Prose.Image(destination, title, children*) =>
      val alt = children.map(plainTextOf(_)).to(Seq).join
      val label = if alt.length == 0 then title.or(t"image") else alt
      val link = Hyperlink(destination)
      e"$link(${Fg(palette.subdued)}([$label]))"

    case Prose.HtmlInline(html) =>
      e"${Fg(palette.subdued)}($html)"


  // For image alt-text and similar uses where we need the plain text inside
  // an inline subtree without any markup.
  private def plainTextOf(node: Prose): Text = node match
    case Prose.Textual(text)                  => text
    case Prose.Code(code)                     => code
    case Prose.Softbreak                      => t" "
    case Prose.Linebreak                      => t" "
    case Prose.HtmlInline(_)                  => t""
    case Prose.Emphasis(children*)            => children.map(plainTextOf(_)).to(Seq).join
    case Prose.Strong(children*)              => children.map(plainTextOf(_)).to(Seq).join
    case Prose.Link(_, _, children*)          => children.map(plainTextOf(_)).to(Seq).join

    case Prose.Image(_, title, children*) =>
      val inner = children.map(plainTextOf(_)).to(Seq).join
      if inner.length == 0 then title.or(t"") else inner


  // -- block ---------------------------------------------------------------

  // Render a single block to a non-empty list of physical lines. Lines do
  // NOT carry trailing newlines; the outer driver joins them.
  private def layoutLines
    ( node: Layout, width: Int )
    ( using hyphenation: Hyphenation,
            formattables: Every[TeletypeFormattable],
            palette: MarkdownPalette )
  :   List[Teletype] = node match

    case Layout.Heading(_, level, children*) =>
      val text = children.map(inlineProse(_)).to(Seq).join
      val styled = e"$Bold(${Fg(palette.heading)}($text))"
      val wrapped = wrap(styled, width)

      level match
        case 1 =>
          val rule = e"${Fg(palette.heading)}(${t"━"*width})"
          wrapped :+ rule

        case 2 =>
          val rule = e"${Fg(palette.heading)}(${t"─"*width})"
          wrapped :+ rule

        case _ =>
          val prefix = e"${Fg(palette.subdued)}(${t"#"*level} )"

          wrapped match
            case Nil =>
              List(prefix)

            case head :: tail =>
              (prefix + head) :: tail.map(indent(_, t" "*(level + 1)))

    case Layout.Paragraph(_, children*) =>
      val styled = children.map(inlineProse(_)).to(Seq).join
      wrap(styled, width)

    case Layout.ThematicBreak(_) =>
      List(e"${Fg(palette.rule)}(${t"─"*width})")

    case Layout.BlockQuote(_, children*) =>
      val bar = e"${Fg(palette.quoteBar)}(▎)"
      val innerWidth = (width - 2).max(1)
      val innerBlocks = children.map(layoutLines(_, innerWidth)).filter(_.nonEmpty)
      val joined = interleaveBlanks(innerBlocks.to(List))

      joined.map: line =>
        if line.plain.length == 0 then bar
        else bar + Space + line

    case Layout.BulletList(_, tight, items*) =>
      renderList(items.to(List), tight, width, _ => bullet(palette))

    case Layout.OrderedList(_, start, tight, delimiter, items*) =>
      val delim = delimiter.or('.')
      renderList(items.to(List), tight, width, n => e"${Fg(palette.subdued)}(${start + n}$delim)")

    case Layout.CodeBlock(_, info, code) =>
      val available = summon[Every[TeletypeFormattable]].values

      val formatted = available.foldLeft(Unset: Optional[Teletype]):
        (acc, fmt) => acc.or(fmt.format(info, code))

      val body: Teletype = formatted.or:
        e"${Fg(palette.subdued)}($code)"

      val raw = body.cut(t"\n")
      // Drop a trailing empty line that comes from a final '\n' in `code`.
      val lines = if raw.lastOption.exists(_.plain.length == 0) then raw.dropRight(1) else raw

      lines.map: line =>
        e"  $line"

    case Layout.HtmlBlock(_, html) =>
      val text: Teletype = e"${Fg(palette.subdued)}($html)"

      text.cut(t"\n") match
        case Nil            => Nil
        case lines @ _ :: _ => if lines.last.plain.length == 0 then lines.dropRight(1) else lines


  private def bullet(palette: MarkdownPalette): Teletype =
    e"${Fg(palette.subdued)}(•)"


  // Renders a list, dispatching to the marker-generator `marker` (called with
  // the zero-based index). Continuation lines of each item are hung off the
  // marker by `markerWidth + 1` spaces. `tight` lists omit the blank line
  // between items.
  private def renderList
    ( items:  List[List[Layout]],
      tight:  Boolean,
      width:  Int,
      marker: Int => Teletype )
    ( using hyphenation: Hyphenation,
            formattables: Every[TeletypeFormattable],
            palette: MarkdownPalette )
  :   List[Teletype] =

    if items.isEmpty then Nil
    else
      val rendered = items.zipWithIndex.map: (item, idx) =>
        val mk = marker(idx)
        val markerSize = mk.plain.length + 1   // marker + one space
        val hang = t" "*markerSize
        val inner = item.map(layoutLines(_, (width - markerSize).max(1))).filter(_.nonEmpty)
        val joined = if tight then concatItems(inner) else interleaveBlanks(inner)

        joined match
          case Nil            => List(mk)

          case head :: tail =>
            (mk + Space + head) :: tail.map(indent(_, hang))

      if tight then rendered.flatten
      else interleaveBlanks(rendered)


  // -- helpers --------------------------------------------------------------

  // Concatenate per-block line lists with no separator (used for tight lists).
  private def concatItems(blocks: List[List[Teletype]]): List[Teletype] =
    blocks.flatten


  // Concatenate per-block line lists with a single blank line between blocks.
  private def interleaveBlanks(blocks: List[List[Teletype]]): List[Teletype] =
    blocks match
      case Nil          => Nil
      case head :: Nil  => head
      case head :: tail => head ::: Teletype.empty :: interleaveBlanks(tail)


  // Prefix a single line's content with `prefix` (no newlines should appear
  // in `line`).
  private def indent(line: Teletype, prefix: Text): Teletype =
    if line.plain.length == 0 then Teletype(prefix) else Teletype(prefix)+line


  // Join a list of lines into one Teletype with embedded newlines.
  private def joinLines(lines: List[Teletype]): Teletype =
    lines.to(Seq).join(Newline)


  // Wrap an inline `Teletype` into width-bounded lines, applying soft
  // hyphenation via `polysyllabic` when a single word would overflow but a
  // syllable break would fit.
  private def wrap
    ( content: Teletype, width: Int )
    ( using hyphenation: Hyphenation )
  :   List[Teletype] =

    if width < 1 then return List(content)

    // Hard line-breaks (Linebreak) embedded as '\n' produce a forced break.
    // Split on them first, wrap each segment independently, then concatenate.
    val segments = content.cut(t"\n")
    segments.flatMap(wrapSegment(_, width))


  private def wrapSegment
    ( segment: Teletype, width: Int )
    ( using hyphenation: Hyphenation )
  :   List[Teletype] =

    val out = scala.collection.mutable.ListBuffer[Teletype]()
    var line: Teletype = Teletype.empty
    var col: Int = 0

    inline def flush(): Unit =
      out += line
      line = Teletype.empty
      col = 0

    def placeWord(word: Teletype): Unit =
      val wlen = word.plain.length

      if wlen == 0 then ()
      else
        val needsSpace = col > 0
        val needed = if needsSpace then wlen + 1 else wlen

        if col + needed <= width then
          // Fits on current line.
          if needsSpace then
            line = line + Space
            col += 1

          line = line + word
          col += wlen
        else
          // Doesn't fit. Try hyphenation.
          val avail = if needsSpace then (width - col - 1).max(0) else width - col
          val breaks = word.plain.breakPoints
          var best = -1
          var k = 0

          while k < breaks.length do
            val b = breaks(k)
            // After break, prefix is `b` chars; we add a hyphen, total `b + 1`.
            if b + 1 <= avail && b > best then best = b
            k += 1

          if best >= 1 then
            if needsSpace then
              line = line + Space
              col += 1

            line = line + word.takeChars(best) + Hyphen
            flush()
            placeWord(word.dropChars(best))
          else if col > 0 then
            // Move to a new line and try again.
            flush()
            placeWord(word)
          else
            // Word exceeds the width and has no break point. Emit it on its
            // own line, overflowing if necessary.
            line = word
            col = wlen
            flush()

    segment.cut(t" ").each: word =>
      placeWord(word)

    if col > 0 then flush()

    if out.isEmpty then List(Teletype.empty) else out.to(List)
