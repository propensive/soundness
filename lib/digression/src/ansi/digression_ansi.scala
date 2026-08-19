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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package digression

import proscenium.compat.*

import anticipation.*
import escapade.*
import escritoire.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import rudiments.*
import spectacular.*
import tessellate.*
import vacuous.*
import symbolism.*

trait StackTracePalette extends iridescence.Palette:
  type Form = Srgb
  def message:   Color in Srgb
  def file:      Color in Srgb
  def method:    Color in Srgb
  def line:      Color in Srgb
  def separator: Color in Srgb
  def accent1:   Color in Srgb
  def accent2:   Color in Srgb
  def accent3:   Color in Srgb
  def accent4:   Color in Srgb
  def accent5:   Color in Srgb

object StackTracePalette:
  private def hex(n: Int): Color in Srgb =
    Srgb(((n >> 16) & 255)/255.0, ((n >> 8) & 255)/255.0, (n & 255)/255.0)

  given default: StackTracePalette = new StackTracePalette:
    def background: Color in Srgb = hex(0x000000)
    def foreground: Color in Srgb = hex(0xffffff)
    def message:    Color in Srgb = hex(0xffffff)
    def file:       Color in Srgb = hex(0x5f9e9f)
    def method:     Color in Srgb = hex(0xabcfdf)
    def line:       Color in Srgb = hex(0x47d1cc)
    def separator:  Color in Srgb = hex(0x808080)
    def accent1:    Color in Srgb = hex(0xf84020)
    def accent2:    Color in Srgb = hex(0xd88600)
    def accent3:    Color in Srgb = hex(0xfefe00)
    def accent4:    Color in Srgb = hex(0xfeae00)
    def accent5:    Color in Srgb = hex(0xaefe00)

// The styled renderings, imported decisively: `import digression.teletypeables.*`.
package teletypeables:
  given exceptionTeletype: (Text is Measurable, StackTrace.Resolver) => Exception is Teletypeable =
    exception => summon[StackTrace is Teletypeable].teletype(StackTrace(exception))

  given stackTraceTeletype: (Text is Measurable) => (palette: StackTracePalette)
  =>  StackTrace is Teletypeable = stack =>

    // A static match, not `selectDynamic(s"accent$n")`: structural selection reflects through
    // `Class.getMethod`, unsupported on Scala Native (and these are real trait members anyway).
    def accent(level: Int): Color in Srgb = (level % 5) + 1 match
      case 1 => palette.accent1
      case 2 => palette.accent2
      case 3 => palette.accent3
      case 4 => palette.accent4
      case _ => palette.accent5

    def dedup[element](todo: List[element], seen: Set[element] = Set(), done: List[element] = Nil)
    :   List[element] =

      todo match
        case Nil          => done.reverse

        case head :: tail =>
          if seen.has(head) then dedup(tail, seen, done)
          else dedup(tail, seen :+ head, head :: done)

    val packages: Map[Text, Color in Srgb] =
      Map.from:
        dedup[Text](stack.frames.map(_.method.prefix), Set(), Nil).stdlib
        . zipWithIndex.map: (prefix, index) =>
            prefix -> accent(index)

    val fullClass = e"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = e"${palette.message}($fullClass): ${stack.message}"

    // A row is a frame, or one level of inlining beneath it when the frame's classfile carried an
    // SMAP: extra detail about the frame above, not a frame of its own.
    case class Row
      ( frame:     StackTrace.Frame,
        sameClass: Boolean,
        sameFile:  Boolean,
        inlined:   Optional[StackTrace.Frame.Inlined] = Unset )

    val rows: List[Row] =
      List.of:
        stack.frames.fold((List.empty[Row].stdlib, t"", t"")):
          case ((acc, lastClass, lastFile), frame) =>
            val sameClass = frame.displayClass == lastClass
            val sameFile = frame.file == lastFile
            val subRows = frame.inlined.map(Row(frame, true, false, _)).reverse.stdlib
            (subRows ::: Row(frame, sameClass, sameFile) :: acc, frame.displayClass, frame.file)

        . _1.reverse

    // A frame the compiler generated—a bridge, a forwarder, an initializer—is rarely what the
    // reader is looking for, so it stays legible but recedes.
    def plumbing(row: Row): Boolean = row.frame.source.lay(false)(_.kind.plumbing)

    import StackTrace.Frame.Inlined

    def classCell(row: Row): Teletype = row.inlined match
      case origin: Inlined =>
        origin.source.lay(e""): source =>
          e"${palette.subdue(accent(0), 0.85)}(${source.owner})"

      case _ =>
        val frame = row.frame
        val obj = frame.displaySegment.starts(t"Ξ")
        val methodCls = if obj then frame.displaySegment.skip(1) else frame.displaySegment
        // Every row's prefix was registered when `packages` was built from the same frames; an
        // unregistered prefix falls back to the first accent colour.
        val color = packages(frame.method.prefix).or(accent(0))

        if row.sameClass || plumbing(row)
        then e"${palette.subdue(color, 0.85)}(${frame.displayPrefix}.$methodCls)"
        else
          e"${palette.subdue(color, 0.5)}(${frame.displayPrefix}.$Bold($color($methodCls)))"

    def dotCell(row: Row): Teletype = row.inlined match
      case origin: Inlined => origin.source.lay(e""): _ => e"${palette.separator}(.)"
      case _ =>
        // A resolved frame names a chain of source definitions, which is joined with dots however
        // the chain happens to have been compiled.
        val resolved = row.frame.source.present
        val ch = if resolved || row.frame.displaySegment.starts(t"Ξ") then t"." else t"⌗"
        e"${palette.separator}($ch)"

    def methodCell(row: Row): Teletype = row.inlined match
      case origin: Inlined =>
        origin.source.lay(e"${palette.subdue(palette.method, 0.85)}(inlined from)"): source =>
          e"${palette.subdue(palette.method, 0.85)}(${source.name})"

      case _ =>
        val color = if plumbing(row) then palette.subdue(palette.method, 0.85) else palette.method
        e"$color(${row.frame.displayMethod})"

    def codeCell(row: Row): Teletype = row.inlined match
      case origin: Inlined =>
        e"${palette.subdue(palette.file, 0.7)}(${origin.source.let(_.code).or(t"")})"

      case _ =>
        e"${palette.subdue(palette.file, 0.7)}(${row.frame.source.let(_.code).or(t"")})"

    def fileCell(row: Row): Teletype = row.inlined match
      case origin: Inlined => e"${palette.subdue(palette.file, 0.5)}(${origin.file})"
      case _ =>
        val color = if row.sameFile then palette.subdue(palette.file, 0.85) else palette.file
        e"$color(${row.frame.file})"

    def lineCell(row: Row): Teletype = row.inlined match
      case origin: Inlined => e"${palette.subdue(palette.line, 0.5)}(${origin.line})"
      case _ => e"${palette.line}(${row.frame.line.let(_.show).or(t"")})"

    val scaffold =
      Scaffold[Row]
        ( Column(e"")(row => e"${palette.separator}(${if row.inlined.present then t" ↳" else t"at"})"),
          Column(e"", textAlign = TextAlignment.Right)(classCell),
          Column(e"")(dotCell),
          Column(e"")(methodCell),
          Column(e"", textAlign = TextAlignment.Right)(fileCell),
          Column(e"")(_ => e"${palette.separator}(:)"),
          Column(e"", textAlign = TextAlignment.Right)(lineCell),
          // The quoted source is the first thing to go when the terminal is too narrow for it:
          // everything else in the row is needed to identify the frame at all.
          Column(e"", sizing = columnar.Collapsible(0.5))(codeCell) )

    given style: TableStyle =
      TableStyle
        ( padding    = 0,
          topLine    = Unset,
          bottomLine = Unset,
          titleLine  = Unset,
          sideLines  = BoxLine.Blank,
          innerLines = BoxLine.Blank,
          charset    = LineCharset.Default )

    import columnAttenuation.ignoreAttenuation

    val grid = scaffold.tabulate(rows).grid(200)
    val dataOnly = grid.copy(sections = grid.sections.tail)
    val tableLines = List.from(dataOnly.render.stdlib)

    val allLines = init :: (tableLines: List[Teletype])
    val root = allLines.stdlib.join(e"\n")

    stack.cause.lay(root): cause => e"$root\n${palette.message}(caused by:)\n$cause"

  given frameTeletype: (Text is Measurable) => (palette: StackTracePalette)
  =>  StackTrace.Frame is Teletypeable = frame =>

    val className = e"${palette.method}(${frame.displayClass.fit(40, Rtl)})"
    val method = e"${palette.method}(${frame.method.method.fit(40)})"
    val file = e"${palette.file}(${frame.file.fit(18, Rtl)})"
    val line = e"${palette.line}(${frame.line.let(_.show).or(t"?")})"
    e"$className${palette.separator}( ⌗ )$method $file${palette.separator}(:)$line"

  given methodTeletype: (palette: StackTracePalette) => StackTrace.Method is Teletypeable = method =>
    val className = e"${palette.method}(${method.className})"
    val methodName = e"${palette.method}(${method.method})"
    e"$className${palette.separator}( ⌗ )$methodName"

