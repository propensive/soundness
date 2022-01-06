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

  given AnsiShow[StackTrace] = stack =>
    stack.frames.foldLeft(ansi"${stack.className} from ${stack.component} thrown"):
      case (msg, frame) => ansi"$msg\n$frame"
  
  given AnsiShow[StackTrace.Frame] = frame =>
    ansi"  in ${frame.className}#${frame.method} at ${frame.file}:${frame.line}"

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