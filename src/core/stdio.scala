package turbulence

import rudiments.*
import anticipation.*
import perforate.*
import hieroglyph.*

package basicIo:
  given jvm(using streamCut: Raises[StreamCutError]): Stdio = new Stdio:
    val encoder = CharEncoder.system
    def putOutText(text: Text): Unit = putOutBytes(encoder.encode(text))
    def putErrText(text: Text): Unit = putErrBytes(encoder.encode(text))
    
    def putOutBytes(bytes: Bytes): Unit =
      if System.out == null then raise(StreamCutError(0.b))(())
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
    def putErrBytes(bytes: Bytes): Unit =
      if System.out == null then raise(StreamCutError(0.b))(())
      else System.out.nn.writeBytes(bytes.mutable(using Unsafe))
    
@capability
trait Stdio:
  def putErrBytes(bytes: Bytes): Unit
  def putErrText(text: Text): Unit
  def putOutBytes(bytes: Bytes): Unit
  def putOutText(text: Text): Unit

object Stderr
object Stdout

object Io:
  def put(bytes: Bytes)(using io: Stdio): Unit =
    io.putOutBytes(bytes)

  def print[TextType](text: TextType)(using io: Stdio)(using printable: Printable[TextType]): Unit =
    io.putOutText(printable.print(text))
  
  def printErr[TextType](text: TextType)(using io: Stdio)(using printable: Printable[TextType]): Unit =
    io.putErrText(printable.print(text))
  
  def println[TextType](text: TextType)(using io: Stdio, printable: Printable[TextType]): Unit =
    io.putOutText(printable.print(text))
    io.putOutText("\n".tt)

  def println()(using io: Stdio): Unit = io.putOutText("\n".tt)
  def printlnErr()(using io: Stdio): Unit = io.putErrText("\n".tt)
  
  def printlnErr[TextType](text: TextType)(using io: Stdio, printable: Printable[TextType]): Unit =
    io.putErrText(printable.print(text))
    io.putErrText("\n".tt)

object Stdio:
  given default(using Quickstart)(using Raises[StreamCutError]): Stdio = basicIo.jvm
