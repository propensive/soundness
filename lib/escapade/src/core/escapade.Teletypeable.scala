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
package escapade

import anticipation.*
import digression.*
import fulminate.*
import gossamer.*
import hieroglyph.*
import iridescence.*
import prepositional.*
import proscenium.*
import spectacular.*
import symbolism.*
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
      case 1 => e"$acc$Italic(${Fg(0xefe68b)}($next))"
      case _ => e"$acc$Italic($Bold(${Fg(0xffd600)}($next)))"

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
          append(Teletype(t"▀", TreeMap((CharSpan(0, 1), { _ => TextStyle(fg, bg) }))))

        append(e"\n")

  private val pkgColor = Fg(0xffff00)
  private val clsColor = Fg(0xff0000)
  private val methColor = Fg(0x00ffff)

  given stackTrace: (Text is Measurable) => StackTrace is Teletypeable = stack =>
    def heat(level: Int): Int = level match
      case 0 => 0xf84020
      case 1 => 0xd88600
      case 2 => 0xfefe00
      case 3 => 0xfeae00
      case _ => 0xaefe00


    def dedup[element](todo: List[element], seen: Set[element] = Set(), done: List[element] = Nil)
    : List[element] =

        todo match
          case Nil => done

          case head :: tail =>
            if seen.contains(head) then dedup(tail, seen, done)
            else dedup(tail, seen + head, head :: done)


    val packages: Map[Text, Int] =
      dedup[Text](stack.frames.map(_.method.prefix), Set(), Nil)
      . zipWithIndex.map(_ -> heat(_))
      . to(Map)

    val methodWidth = stack.frames.map(_.method.method.length).maxOption.getOrElse(0)
    val classWidth = stack.frames.map(_.method.className.length).maxOption.getOrElse(0)
    val fileWidth = stack.frames.map(_.file.length).maxOption.getOrElse(0)

    val fullClass = e"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = e"${Fg(0xffffff)}($fullClass): ${stack.message}"

    var lastClass: Text = t""
    var lastFile: Text = t""

    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.method.cls.starts(t"Ξ")
        val methodCls = if obj then frame.method.cls.skip(1) else frame.method.cls
        val file = e"${Fg(0x5f9e9f)}(${frame.file.fit(fileWidth, Rtl)})"
        val dot = if obj then t" . " else t" ⌗ "

        val sameClass = frame.method.className == lastClass
        lastClass = frame.method.className

        val gray = Fg(0x808080)

        val className =
          val color = packages(frame.method.prefix)
          if sameClass
          then
            e"${Fg(0x222222)}(${frame.method.prefix}.$methodCls)"
            . fit(classWidth, Rtl)
          else
            e"${Fg(color/2)}(${frame.method.prefix}.$Bold(${Fg(color)}($methodCls)))"
            . fit(classWidth, Rtl)

        val method = e"${Fg(0xabcfdf)}(${frame.method.method.fit(methodWidth)})"
        val line = e"${Fg(0x47d1cc)}(${frame.line.let(_.show).or(t"")})"
        val sameFile = frame.file == lastFile
        lastFile = frame.file
        val file2 = frame.file.fit(fileWidth, Rtl)

        val foreground = Fg(if sameFile then 0x111111 else 0x5faeaf)
        e"$msg\n  $gray(at) $className$gray($dot)$method $foreground($file2)$gray(:)$line"

    stack.cause.lay(root): cause =>
      e"$root\n${Fg(0xffffff)}(caused by:)\n$cause"

  given frame: (Text is Measurable) => StackTrace.Frame is Teletypeable = frame =>
    val className = e"${Fg(0xc61485)}(${frame.method.className.fit(40, Rtl)})"
    val method = e"${Fg(0xdb6f92)}(${frame.method.method.fit(40)})"
    val file = e"${Fg(0x5f9e9f)}(${frame.file.fit(18, Rtl)})"
    val line = e"${Fg(0x47d1cc)}(${frame.line.let(_.show).or(t"?")})"
    e"$className${Fg(0x808080)}( ⌗ )$method $file${Fg(0x808080)}(:)$line"

  given method: StackTrace.Method is Teletypeable = method =>
    val className = e"${Fg(0xc61485)}(${method.className})"
    val methodName = e"${Fg(0xdb6f92)}(${method.method})"
    e"$className${Fg(0x808080)}( ⌗ )$methodName"

  given double: (decimalizer: Decimalizer) => Double is Teletypeable = double =>
    Teletype.make(decimalizer.decimalize(double), _.copy(fg = 0xffd600))

  given throwable: Throwable is Teletypeable = throwable =>
    Teletype.make[String]
     (throwable.getClass.getName.nn.show.cut(t".").last.s, _.copy(fg = 0xdc133b))

trait Teletypeable extends Typeclass:
  def teletype(value: Self): Teletype
