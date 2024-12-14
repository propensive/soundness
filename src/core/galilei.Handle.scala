package galilei

import anticipation.*
import contingency.*
import prepositional.*
import turbulence.*

object Handle:
  given (using Tactic[StreamError]) => Handle is Readable by Bytes as readable = _.reader()
  given (using Tactic[StreamError]) => Handle is Writable by Bytes as writable = _.writer(_)

class Handle(val reader: () => LazyList[Bytes], val writer: LazyList[Bytes] => Unit)
