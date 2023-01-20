/*
    Rudiments, version 0.4.0. Copyright 2020-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import scala.quoted.*
import compiletime.*

case class ErrorMessage[+T <: Tuple](text: Seq[Text], parts: T)

extension (ctx: StringContext)
  transparent inline def err[T](value: T = EmptyTuple): ErrorMessage[Tuple] = (value: @unchecked) match
    case value: Tuple => ErrorMessage[value.type](ctx.parts.map(Text(_)), value)
    case other: T     => ErrorMessage[T *: EmptyTuple](ctx.parts.map(Text(_)), other *: EmptyTuple)
  
abstract class Error[T <: Tuple](val msg: ErrorMessage[T], val cause: Maybe[Error[?]] = Unset)
extends Exception():
  this: Error[T] =>
  def fullClass: List[Text] = List(getClass.nn.getName.nn.split("\\.").nn.map(_.nn).map(Text(_))*)
  def className: Text = fullClass.last
  def component: Text = fullClass.head

  override def getMessage: String = component.s+": "+message
  override def getCause: Exception | Null = cause.option.getOrElse(null)

  def message: Text =
    def recur[T <: Tuple](tuple: T, text: Seq[Text], value: String = ""): String = tuple match
      case EmptyTuple   => value+text.headOption.getOrElse(Text(""))
      case head *: tail => recur(tail, text.tail, value+text.head+head.toString)

    Text(recur(msg.parts, msg.text))

  def explanation: Maybe[Text] = Unset
  def stackTrace: StackTrace = StackTrace(this)

object StackTrace:
  case class Frame(className: Text, method: Text, file: Text, line: Maybe[Int], native: Boolean)
  
  // FIXME: This is going to be gloriously inefficient, and it doesn't unescape other non-ASCII characters
  def rewrite(name: String, method: Boolean = false): Text = Text:
    name
      .replaceAll("\\$anonfun", "λ").nn
      .replaceAll("\\$anon", "Γ").nn
      .replaceAll("\\$package", "Π").nn
      .replaceAll("initial\\$", "ι").nn
      .replaceAll("\\$default\\$", "δ").nn
      .replaceAll("\\$_avoid_name_clash_\\$", "′").nn
      .replaceAll("super\\$", "σ").nn
      .replaceAll("\\$tilde", "~").nn
      .replaceAll("\\$eq", "=").nn
      .replaceAll("\\$less", "<").nn
      .replaceAll("\\$greater", ">").nn
      .replaceAll("\\$bang", "!").nn
      .replaceAll("\\$hash", "#").nn
      .replaceAll("\\$percent", "%").nn
      .replaceAll("\\$up", "^").nn
      .replaceAll("\\$amp", "&").nn
      .replaceAll("\\$bar", "|").nn
      .replaceAll("\\$times", "*").nn
      .replaceAll("\\$div", "/").nn
      .replaceAll("\\$plus", "+").nn
      .replaceAll("\\$minus", "-").nn
      .replaceAll("\\$colon", ":").nn
      .replaceAll("\\$bslash", "\\").nn
      .replaceAll("\\$qmark", "?").nn
      .replaceAll("\\$at", "@").nn
      .replaceAll("\\$0", "₀").nn
      .replaceAll("\\$1", "₁").nn
      .replaceAll("\\$2", "₂").nn
      .replaceAll("\\$3", "₃").nn
      .replaceAll("\\$4", "₄").nn
      .replaceAll("\\$5", "₅").nn
      .replaceAll("\\$6", "₆").nn
      .replaceAll("\\$7", "₇").nn
      .replaceAll("\\$8", "₈").nn
      .replaceAll("\\$9", "₉").nn
      .replaceAll("\\$", if method then "." else "#").nn

  def apply(exception: Throwable): StackTrace =
    val frames = List(exception.getStackTrace.nn.map(_.nn)*).map: frame =>
      StackTrace.Frame(
        rewrite(frame.getClassName.nn),
        rewrite(frame.getMethodName.nn, method = true),
        Text(Option(frame.getFileName).map(_.nn).getOrElse("[no file]")),
        if frame.getLineNumber < 0 then Unset else frame.getLineNumber,
        frame.isNativeMethod
      )
    
    val cause = Option(exception.getCause)
    val fullClassName: Text = rewrite(exception.getClass.nn.getName.nn)
    val fullClass: List[Text] = List(fullClassName.s.split("\\.").nn.map(_.nn).map(Text(_))*)
    val className: Text = fullClass.last
    val component: Text = Text(fullClassName.s.substring(0, 0 max (fullClassName.s.length - className.s.length - 1)).nn)
    val message = Text(Option(exception.getMessage).map(_.nn).getOrElse(""))
    
    StackTrace(component, className, message, frames, cause.map(_.nn).map(StackTrace(_)).maybe)

case class StackTrace(component: Text, className: Text, message: Text, frames: List[StackTrace.Frame],
                          cause: Maybe[StackTrace])

object Codepoint:
  inline given Codepoint = ${RudimentsMacros.location}

case class Codepoint(source: Text, line: Int)

object RudimentsMacros:
  def location(using Quotes): Expr[Codepoint] =
    import quotes.*, reflect.*
    val path = Expr(Position.ofMacroExpansion.sourceFile.path)
    val line = Expr(Position.ofMacroExpansion.startLine + 1)
    '{Codepoint(Text($path), $line)}

def safely[T](value: => CanThrow[Exception] ?=> T): Maybe[T] =
  try value(using unsafeExceptions.canThrowAny) catch NonFatal => Unset

def unsafely[T](value: => CanThrow[Exception] ?=> T): T = value(using unsafeExceptions.canThrowAny)