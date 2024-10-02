package galilei

import java.nio.channels as jnc

import prepositional.*
import turbulence.*
import contingency.*
import anticipation.*

object Handle:
  given (using Tactic[StreamError]) => Handle is Readable by Bytes as readable = _.reader()
  given (using Tactic[StreamError]) => Handle is Writable by Bytes as writable = _.writer(_)

class Handle
    (val reader: () => LazyList[Bytes],
     val writer: LazyList[Bytes] => Unit)