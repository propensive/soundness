package galilei

import java.io as ji

import prepositional.*
import turbulence.*
import contingency.*
import anticipation.*
import rudiments.*

object File:
  given (using Tactic[StreamError]) => File is Readable by Bytes =
    Readable.inputStream.contramap(_.stream)

class File(private[galilei] val stream: ji.FileInputStream):
  private[galilei] def close(): Unit = stream.close()