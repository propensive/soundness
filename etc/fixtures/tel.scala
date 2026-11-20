// The peers the streaming and acceptance examples talk to, which the tutorial does not open: a
// socket taking bytes, a sink taking one byte at a time, an output taking text, and a channel
// that sends an acceptance and receives the reply.
import soundness.*

object socket:
  def write(data: Data): Unit = ()

object sink:
  def put(byte: Byte): Unit = ()

object out:
  def write(text: Text): Unit = ()

def send(data: Data): Unit = ()
val reply: Data = Data()
