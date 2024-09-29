package galilei

import java.io as ji

import prepositional.*
import turbulence.*
import contingency.*
import anticipation.*

object Handle:
  given (using Tactic[StreamError]) => Handle is Readable by Bytes =
    Readable.inputStream.contramap(_.stream)

class Handle(private[galilei] val stream: ji.FileInputStream):
  private[galilei] def close(): Unit = stream.close()