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

object Display:
  given Display[Output] = identity(_)
  given Display[Text] = text => Output(text)
  given Display[Pid] = pid => out"${colors.FireBrick}(${pid.value.show})"

  given [T: Display]: Display[Option[T]] =
    case None    => Output("empty".show)
    case Some(v) => summon[Display[T]](v)
  
  given [ValueType](using show: Show[ValueType]): Display[ValueType] = value =>
    Output(show(value))

  given (using TextWidthCalculator): Display[Exception] = e =>
    summon[Display[StackTrace]](StackTrace.apply(e))

  given Display[Error] = error =>
    error.message.fold(out"")((msg, txt) => out"$msg$txt", (msg, sub) => out"$msg$Italic($sub)")

  given (using TextWidthCalculator): Display[StackTrace] = stack =>
    val methodWidth = stack.frames.map(_.method.length).max
    val classWidth = stack.frames.map(_.className.length).max
    val fileWidth = stack.frames.map(_.file.length).max
    
    val fullClass = out"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = out"${colors.White}($fullClass): ${stack.message}"
    
    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.className.ends(t"#")
        import colors.*
        val drop = if obj then 1 else 0
        val file = out"$CadetBlue(${frame.file.fit(fileWidth, Rtl)})"
        val dot = if obj then t"." else t"#"
        val cls = out"$MediumVioletRed(${frame.className.drop(drop, Rtl).fit(classWidth, Rtl)})"
        val method = out"$PaleVioletRed(${frame.method.fit(methodWidth)})"
        val line = out"$MediumTurquoise(${frame.line.mm(_.show).or(t"?")})"
        
        out"$msg\n  $Gray(at) $cls$Gray($dot)$method $file$Gray(:)$line"
    
    stack.cause.option match
      case None        => root
      case Some(cause) => out"$root\n${colors.White}(caused by:)\n$cause"
  
  given (using TextWidthCalculator): Display[StackTrace.Frame] = frame =>
    import colors.*
    val cls = out"$MediumVioletRed(${frame.className.fit(40, Rtl)})"
    val method = out"$PaleVioletRed(${frame.method.fit(40)})"
    val file = out"$CadetBlue(${frame.file.fit(18, Rtl)})"
    val line = out"$MediumTurquoise(${frame.line.mm(_.show).or(t"?")})"
    out"$cls$Gray(#)$method $file$Gray(:)$line"

  given (using decimalizer: Decimalizer): Display[Double] = double =>
    Output.make(decimalizer.decimalize(double), _.copy(fg = colors.Gold))

  given Display[Throwable] = throwable =>
    Output.make[String](throwable.getClass.getName.nn.show.cut(t".").last.s,
        _.copy(fg = colors.Crimson))

trait Display[-ValueType] extends Showable[ValueType]:
  def show(value: ValueType): Text = apply(value).plain
  def apply(value: ValueType): Output
