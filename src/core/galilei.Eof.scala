package galilei

import java.nio.file as jnf

import prepositional.*
import serpentine.*

object Eof:
  given [PlatformType <: Filesystem]
      (using openable: (Path on PlatformType) is Openable by jnf.OpenOption)
      => Eof[PlatformType] is Openable by openable.Operand into openable.Result =
    new Openable:
      type Self = Eof[PlatformType]
      type Operand = jnf.OpenOption
      type Result = openable.Result
      protected type Carrier = openable.Carrier

      def init(value: Eof[PlatformType], options: List[Operand]): Carrier =
        openable.init(value.path, jnf.StandardOpenOption.APPEND :: options)
    
      def handle(carrier: Carrier): Result = openable.handle(carrier)
      def close(carrier: Carrier): Unit = openable.close(carrier)

case class Eof[PlatformType <: Filesystem](path: Path on PlatformType)