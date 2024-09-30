package galilei

import java.nio.channels as jnc

import prepositional.*
import turbulence.*
import contingency.*
import anticipation.*

object Handle:
  given (using Tactic[StreamError]) => Handle is Readable by Bytes as readable =
    Readable.channel.contramap(_.channel)
  
  given (using Tactic[StreamError]) => Handle is Writable by Bytes as writable =
    Writable.channel.contramap(_.channel)

class Handle(private[galilei] val channel: jnc.FileChannel)