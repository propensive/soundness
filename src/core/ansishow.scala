/*
    Escapade, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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
import digression.*
import gossamer.*
import iridescence.*
import lithography.*
import spectacular.*

object AnsiShow:
  given AnsiShow[AnsiText] = identity(_)
  given AnsiShow[Text] = text => AnsiText(text)
  given AnsiShow[Pid] = pid => ansi"${colors.FireBrick}(${pid.value.show})"

  given [T: AnsiShow]: AnsiShow[Option[T]] =
    case None    => AnsiText("empty".show)
    case Some(v) => summon[AnsiShow[T]].ansi(v)
  
  given [ValueType](using show: Show[ValueType]): AnsiShow[ValueType] = value =>
    AnsiText(show(value))

  given (using TextWidthCalculator): AnsiShow[Exception] = e =>
    summon[AnsiShow[StackTrace]].ansi(StackTrace.apply(e))

  given AnsiShow[Error] = error =>
    error.message.fold(ansi"")((msg, txt) => ansi"$msg$txt", (msg, sub) => ansi"$msg$Italic($sub)")

  given (using TextWidthCalculator): AnsiShow[StackTrace] = stack =>
    val methodWidth = stack.frames.map(_.method.length).max
    val classWidth = stack.frames.map(_.className.length).max
    val fileWidth = stack.frames.map(_.file.length).max
    
    val fullClass = ansi"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = ansi"${colors.White}($fullClass): ${stack.message}"
    
    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.className.ends(t"#")
        import colors.*
        val drop = if obj then 1 else 0
        val file = ansi"$CadetBlue(${frame.file.fit(fileWidth, Rtl)})"
        val dot = if obj then t"." else t"#"
        val cls = ansi"$MediumVioletRed(${frame.className.drop(drop, Rtl).fit(classWidth, Rtl)})"
        val method = ansi"$PaleVioletRed(${frame.method.fit(methodWidth)})"
        val line = ansi"$MediumTurquoise(${frame.line.mm(_.show).or(t"?")})"
        
        ansi"$msg\n  $Gray(at) $cls$Gray($dot)$method $file$Gray(:)$line"
    
    stack.cause.option match
      case None        => root
      case Some(cause) => ansi"$root\n${colors.White}(caused by:)\n${cause.ansi}"
  
  given (using TextWidthCalculator): AnsiShow[StackTrace.Frame] = frame =>
    import colors.*
    val cls = ansi"$MediumVioletRed(${frame.className.fit(40, Rtl)})"
    val method = ansi"$PaleVioletRed(${frame.method.fit(40)})"
    val file = ansi"$CadetBlue(${frame.file.fit(18, Rtl)})"
    val line = ansi"$MediumTurquoise(${frame.line.mm(_.show).or(t"?")})"
    ansi"$cls$Gray(#)$method $file$Gray(:)$line"

  given (using decimalizer: Decimalizer): AnsiShow[Double] = double =>
    AnsiText.make(decimalizer.decimalize(double), _.copy(fg = colors.Gold))

  given AnsiShow[Throwable] = throwable =>
    AnsiText.make[String](throwable.getClass.getName.nn.show.cut(t".").last.s,
        _.copy(fg = colors.Crimson))

trait AnsiShow[-ValueType] extends Show[ValueType]:
  def apply(value: ValueType): Text = ansi(value).plain
  def ansi(value: ValueType): AnsiText
