package journalist

import anticipation.*
import contingency.*
import galilei.*
import prepositional.*
import turbulence.*

object Journal:

  def apply[store: {Streamable by Data, Writable by Data}, entry: Encodable by Data]
       (store: store)
  : Journal by entry =

      new Journal:
        type Operand = entry
        def record(entry: Operand): Long =
          val bytes = entry.encode
          store.write(bytes.length.bytes)
          store.write(entry.encode)

        def replay(offset: Long): Iterator[Data] =
          load(offset).iterator


trait Journal extends Typeclass:
  protected def load(offset: Long): Iterable[Data]
  def record(entry: Data): Long
  def replay(offset: Long): Iterator[Data]
