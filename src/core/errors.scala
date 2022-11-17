package rudiments

import scala.quoted.*

case class ErrorMessage[+T <: Tuple](text: Seq[Text], parts: T)

extension (ctx: StringContext)
  transparent inline def err[T](value: T = EmptyTuple): ErrorMessage[Tuple] = value match
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
      case EmptyTuple   => value+text.head
      case head *: tail => recur(tail, text.tail, value+text.head+head.toString)

    Text(recur(msg.parts, msg.text))

  def explanation: Maybe[Text] = Unset
  def stackTrace: StackTrace = StackTrace(this)

object StackTrace:
  case class Frame(className: Text, method: Text, file: Text, line: Int, native: Boolean)
  
  private val subscripts = "₀₁₂₃₄₅₆₇₈₉"

  def apply(exception: Throwable): StackTrace =
    val frames = List(exception.getStackTrace.nn.map(_.nn)*).map: frame =>
      StackTrace.Frame(
        Text(frame.getClassName.nn),
        Text(frame.getMethodName.nn),
        Text(frame.getFileName.nn),
        frame.getLineNumber,
        frame.isNativeMethod
      )
    
    val cause = Option(exception.getCause)
    val fullClassName: String = exception.getClass.nn.getName.nn
    val fullClass: List[Text] = List(fullClassName.split("\\.").nn.map(_.nn).map(Text(_))*)
    val className: Text = fullClass.last
    val component: Text = Text(fullClassName.substring(0, 0 max (fullClassName.length - className.s.length - 1)).nn)
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
