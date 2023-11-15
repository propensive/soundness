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
import fulminate.*
import anticipation.*
import digression.*
import gossamer.*
import hieroglyph.*
import spectacular.*
import iridescence.*

import language.experimental.captureChecking

object Displayable:
  given output: Displayable[Output] = identity(_)
  given text: Displayable[Text] = text => Output(text)
  given pid: Displayable[Pid] = pid => e"${pid.value.show}"

  given message: Displayable[Message] = _.fold[Output](e""): (acc, next, level) =>
    level match
      case 0 => e"$acc${colors.Khaki}($next)"
      case 1 => e"$acc$Italic(${colors.Gold}($next))"
      case _ => e"$acc$Italic($Bold(${colors.Yellow}($next)))"

  given option[T: Displayable]: Displayable[Option[T]] =
    case None    => Output("empty".show)
    case Some(v) => summon[Displayable[T]](v)
  
  given show[ValueType](using show: Show[ValueType]): Displayable[ValueType] = value =>
    Output(show(value))

  given exception(using TextWidthCalculator): Displayable[Exception] = e =>
    summon[Displayable[StackTrace]](StackTrace.apply(e))

  given error: Displayable[Error] = _.message.display

  given (using TextWidthCalculator): Displayable[StackTrace] = stack =>
    val methodWidth = stack.frames.map(_.method.method.length).max
    val classWidth = stack.frames.map(_.method.className.length).max
    val fileWidth = stack.frames.map(_.file.length).max
    
    val fullClass = e"$Italic(${stack.component}.$Bold(${stack.className}))"
    val init = e"${colors.White}($fullClass): ${stack.message}"
    
    val root = stack.frames.foldLeft(init):
      case (msg, frame) =>
        val obj = frame.method.className.ends(t"#")
        import colors.*
        val drop = if obj then 1 else 0
        val file = e"$CadetBlue(${frame.file.fit(fileWidth, Rtl)})"
        val dot = if obj then t"." else t"#"
        val className = e"$MediumVioletRed(${frame.method.className.drop(drop, Rtl).fit(classWidth, Rtl)})"
        val method = e"$PaleVioletRed(${frame.method.method.fit(methodWidth)})"
        val line = e"$MediumTurquoise(${frame.line.mm(_.show).or(t"?")})"
        
        e"$msg\n  $Gray(at) $className$Gray($dot)$method $file$Gray(:)$line"
    
    stack.cause.option match
      case None        => root
      case Some(cause) => e"$root\n${colors.White}(caused by:)\n$cause"
  
  given (using TextWidthCalculator): Displayable[StackTrace.Frame] = frame =>
    import colors.*
    val className = e"$MediumVioletRed(${frame.method.className.fit(40, Rtl)})"
    val method = e"$PaleVioletRed(${frame.method.method.fit(40)})"
    val file = e"$CadetBlue(${frame.file.fit(18, Rtl)})"
    val line = e"$MediumTurquoise(${frame.line.mm(_.show).or(t"?")})"
    e"$className$Gray(#)$method $file$Gray(:)$line"

  given Displayable[StackTrace.Method] = method =>
    import colors.*
    val className = e"$MediumVioletRed(${method.className})"
    val methodName = e"$PaleVioletRed(${method.method})"
    e"$className$Gray(#)$methodName"
  
  given (using decimalizer: Decimalizer): Displayable[Double] = double =>
    Output.make(decimalizer.decimalize(double), _.copy(fg = colors.Gold.asInt))

  given Displayable[Throwable] = throwable =>
    Output.make[String](throwable.getClass.getName.nn.show.cut(t".").last.s,
        _.copy(fg = colors.Crimson.asInt))

trait Displayable[-ValueType] extends Showable[ValueType]:
  def show(value: ValueType): Text = apply(value).plain
  def apply(value: ValueType): Output
