package galilei

import java.nio.file as jnf

import prepositional.*
import anticipation.*
import serpentine.*
import contingency.*

// object Eof:
//   given [PlatformType <: Filesystem](using openable: (Path on PlatformType) is Openable)
//       => Eof[PlatformType] is Openable:
//     type Self = Eof[PlatformType]
//     type Operand = openable.Operand
//     type Carrier = openable.Carrier

//     def init(value: Eof[PlatformType], options: List[Operand]): Carrier =
//       openable.init(value.path, options)

//     def readable(carrier: Carrier): () => LazyList[Bytes] = openable.readable(carrier)
//     def writable(carrier: Carrier): LazyList[Bytes] => Unit = openable.writable(carrier)
//     def close(carrier: Carrier): Unit = openable.close(carrier)

case class Eof[PlatformType <: Filesystem](path: Path on PlatformType)