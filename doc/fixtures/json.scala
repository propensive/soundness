// The destinations the streaming examples write to, which the tutorial does not open: a
// socket taking bytes, a sink taking one byte at a time, and an output taking text.
import soundness.*

object socket:
  def write(data: Data): Unit = ()

object sink:
  def put(byte: Byte): Unit = ()

object out:
  def write(text: Text): Unit = ()
