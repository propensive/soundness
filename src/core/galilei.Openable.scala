package galilei

import prepositional.*
import turbulence.*
import rudiments.*
import serpentine.*
import contingency.*
import anticipation.*

import java.nio.channels as jnc
import java.nio.file as jnf

object Openable:
  given [PlatformType <: Filesystem]
      (using read:        ReadAccess,
             write:       WriteAccess,
             dereference: DereferenceSymlinks,
             create:      CreateNonexistent on PlatformType,
             streamError: Tactic[StreamError],
             ioError:     Tactic[IoError])
      => (Path on PlatformType) is Openable by jnf.OpenOption over jnc.FileChannel = new Openable:

    type Self = Path on PlatformType
    type Operand = jnf.OpenOption
    type Carrier = jnc.FileChannel
  
    def init(path: Path on PlatformType, extraOptions: List[jnf.OpenOption]): jnc.FileChannel =
      val options = read.options() ++ write.options() ++ dereference.options() ++ create.options() ++
          extraOptions

      path.protect(IoError.Operation.Open)(jnc.FileChannel.open(path.javaPath, options*).nn)

    def readable(channel: jnc.FileChannel): () => LazyList[Bytes] =
      () => Readable.channel.stream(channel).stream[Bytes]
    
    def writable(channel: jnc.FileChannel): LazyList[Bytes] => Unit =
      Writable.channel.write(channel, _)

    def close(channel: jnc.FileChannel): Unit = channel.close()

trait Openable:
  type Self
  type Operand
  type Carrier

  def init(value: Self, options: List[Operand]): Carrier
  def readable(carrier: Carrier): () => LazyList[Bytes]
  def writable(carrier: Carrier): LazyList[Bytes] => Unit

  def open[ResultType](value: Self, lambda: Handle => ResultType, options: List[Operand])
          : ResultType =
    val carrier = init(value, options)
    try lambda(Handle(readable(carrier), writable(carrier))) finally close(carrier)

  def close(carrier: Carrier): Unit
