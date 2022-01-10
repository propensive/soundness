/*
    Escapade, version 0.1.0. Copyright 2021-22 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package escapade

import rudiments.*
import gossamer.*
import iridescence.*

trait FallbackAnsiShow:
  given AnsiShow[T: Show]: AnsiShow[T] = str => AnsiString(str.show)

object AnsiShow extends FallbackAnsiShow:
  given AnsiShow[AnsiString] = identity(_)

  given [T: AnsiShow]: AnsiShow[Option[T]] =
    case None    => AnsiString("empty".show)
    case Some(v) => summon[AnsiShow[T]].ansiShow(v)

  given AnsiShow[Exception] = e => summon[AnsiShow[StackTrace]].ansiShow(StackTrace.apply(e))

  given AnsiShow[StackTrace] = stack =>
    val methodWidth = stack.frames.map(_.method.length).max
    val classWidth = stack.frames.map(_.className.length).max
    val fileWidth = stack.frames.map(_.file.length).max
    val root = stack.frames.foldLeft(ansi"${Bg(colors.OrangeRed)}(${colors.Black}( ${stack.component} ))${colors.OrangeRed}(${Bg(colors.Orange)}())${Bg(colors.Orange)}( ${colors.Black}(${stack.className}) )${colors.Orange}() ${colors.Ivory}(${stack.message})"):
      case (msg, frame) =>
        val obj: Boolean = frame.className.endsWith(t"$$")
        ansi"$msg${'\n'}  ${colors.Gray}(at) ${colors.Plum}(${frame.className.drop(if obj then 1 else 0, Rtl).fit(classWidth, Rtl)})${colors.Gray}(${if obj then t"." else t"#"})${colors.Peru}(${frame.method.fit(methodWidth)}) ${colors.LightBlue}(${frame.file.fit(fileWidth, Rtl)})${colors.Gray}(:)${colors.Yellow}(${frame.line})"
    
    stack.cause.option match
      case None        => root
      case Some(cause) => ansi"$root${'\n'}${colors.White}(caused by:)${'\n'}${cause.ansi}"
  
  given AnsiShow[StackTrace.Frame] = frame =>
    ansi"${colors.Plum}(${frame.className.fit(40, Rtl)})${colors.Gray}(#)${colors.Peru}(${frame.method.fit(40)}) ${colors.LightBlue}(${frame.file.fit(18, Rtl)})${colors.Gray}(:)${colors.Yellow}(${frame.line})"

  private val decimalFormat =
    val df = new java.text.DecimalFormat()
    df.setMinimumFractionDigits(3)
    df.setMaximumFractionDigits(3)
    df

  given AnsiShow[Double] =
    double => AnsiString(decimalFormat.format(double).nn, _.copy(fg = colors.Gold))

  given AnsiShow[Throwable] =
    throwable =>
      AnsiString[String](throwable.getClass.getName.nn.show.cut(t".").last.s,
          _.copy(fg = colors.Crimson))

trait AnsiShow[-T] extends Show[T]:
  def show(value: T): Text = ansiShow(value).plain
  def ansiShow(value: T): AnsiString