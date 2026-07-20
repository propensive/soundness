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
package escapade

import anticipation.*
import digression.*
import escritoire.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import spectacular.*
import vacuous.*

object Teletypeable:
  given teletype: Teletype is Teletypeable = identity(_)
  given text: Text is Teletypeable = text => Teletype(text)


  given colorable: [value: {Showable as showable, Colorable as colorable}]
  =>  value is Teletypeable =

    value => e"${value.color}(${value.show})"


  given message: Message is Teletypeable = _.fold[Teletype](e""): (acc, next, level) =>
    level match
      case 0 => e"$acc$next"
      case 1 => e"$acc$Italic(${Fg(Chroma(0xefe68b))}($next))"
      case _ => e"$acc$Italic($Bold(${Fg(Chroma(0xffd600))}($next)))"

  given option: [value: Teletypeable] => Option[value] is Teletypeable =
    case None        => Teletype("empty".show)
    case Some(value) => value.teletype

  given showable: [value: Showable] => value is Teletypeable = value => Teletype(value.show)

  given exception: (Text is Measurable) => Exception is Teletypeable = exception =>
    summon[StackTrace is Teletypeable].teletype(StackTrace(exception))

  given error: Error is Teletypeable = _.message.teletype

  given graphical: [graphical: Graphical] => graphical is Teletypeable = graphic =>
    TeletypeBuilder().build:
      for y <- 0 until (graphical.height(graphic) - 1) by 2 do
        for x <- 0 until graphical.width(graphic)
        do
          val fg = graphical.pixel(graphic, x, y)
          val bg = graphical.pixel(graphic, x, y + 1)
          val styled = TextStyle(fg, bg).styleWord
          append(Teletype(t"▀", IArray(styled, 0L)))

        append(e"\n")

  given stackTrace: (Text is Measurable) => (palette: StackTrace.Palette)
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
          if seen.contains(head) then dedup(tail, seen, done)
          else dedup(tail, seen + head, head :: done)

    val packages: Map[Text, Color in Srgb] =
      dedup[Text](stack.frames.map(_.method.prefix), Set(), Nil)
      . zipWithIndex.map: (prefix, index) =>
          prefix -> accent(index)

      . to(Map)

    val fullClass = e"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = e"${palette.message}($fullClass): ${stack.message}"

    case class Row(frame: StackTrace.Frame, sameClass: Boolean, sameFile: Boolean)

    val rows: List[Row] =
      stack.frames.foldLeft((List.empty[Row], t"", t"")):
        case ((acc, lastClass, lastFile), frame) =>
          val sameClass = frame.method.className == lastClass
          val sameFile = frame.file == lastFile
          (Row(frame, sameClass, sameFile) :: acc, frame.method.className, frame.file)

      . _1.reverse

    def classCell(row: Row): Teletype =
      val frame = row.frame
      val obj = frame.method.cls.starts(t"Ξ")
      val methodCls = if obj then frame.method.cls.skip(1) else frame.method.cls
      val color = packages(frame.method.prefix)

      if row.sameClass
      then e"${palette.subdue(color, 0.85)}(${frame.method.prefix}.$methodCls)"
      else
        e"${palette.subdue(color, 0.5)}(${frame.method.prefix}.$Bold($color($methodCls)))"

    def dotCell(row: Row): Teletype =
      val ch = if row.frame.method.cls.starts(t"Ξ") then t"." else t"⌗"
      e"${palette.separator}($ch)"

    def methodCell(row: Row): Teletype =
      e"${palette.method}(${row.frame.method.method})"

    def fileCell(row: Row): Teletype =
      val color = if row.sameFile then palette.subdue(palette.file, 0.85) else palette.file
      e"$color(${row.frame.file})"

    def lineCell(row: Row): Teletype =
      e"${palette.line}(${row.frame.line.let(_.show).or(t"")})"

    val scaffold =
      Scaffold[Row]
        ( Column(e"")(_ => e"${palette.separator}(at)"),
          Column(e"", textAlign = TextAlignment.Right)(classCell),
          Column(e"")(dotCell),
          Column(e"")(methodCell),
          Column(e"", textAlign = TextAlignment.Right)(fileCell),
          Column(e"")(_ => e"${palette.separator}(:)"),
          Column(e"", textAlign = TextAlignment.Right)(lineCell) )

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
    val tableLines = dataOnly.render.to(List)

    val root = (init :: tableLines).join(e"\n")

    stack.cause.lay(root): cause =>
      e"$root\n${palette.message}(caused by:)\n$cause"

  given frame: (Text is Measurable) => (palette: StackTrace.Palette)
  =>  StackTrace.Frame is Teletypeable = frame =>

    val className = e"${palette.method}(${frame.method.className.fit(40, Rtl)})"
    val method = e"${palette.method}(${frame.method.method.fit(40)})"
    val file = e"${palette.file}(${frame.file.fit(18, Rtl)})"
    val line = e"${palette.line}(${frame.line.let(_.show).or(t"?")})"
    e"$className${palette.separator}( ⌗ )$method $file${palette.separator}(:)$line"

  given method: (palette: StackTrace.Palette) => StackTrace.Method is Teletypeable = method =>
    val className = e"${palette.method}(${method.className})"
    val methodName = e"${palette.method}(${method.method})"
    e"$className${palette.separator}( ⌗ )$methodName"

  given double: (decimalizer: Decimalizer) => Double is Teletypeable = double =>
    Teletype.styled(decimalizer.decimalize(double))(_.copy(fg = Chroma(0xffd600)))

  given throwable: Throwable is Teletypeable = throwable =>
    Teletype.styled[String]
      (throwable.getClass.getName.nn.show.cut(t".").last.s)(_.copy(fg = Chroma(0xdc133b)))

trait Teletypeable extends Typeclass.Pure:
  def teletype(value: Self): Teletype

  def contramap[self2](lambda: self2 -> Self): self2 is Teletypeable =
    value => teletype(lambda(value))
