package turbulence

import rudiments.*

import java.nio as jn, java.nio.channels as jnc
import java.io as ji

object Util:
  def readInputStream(in: ji.InputStream): DataStream =
    val channel = jnc.Channels.newChannel(in).nn
    try
      val buf = jn.ByteBuffer.wrap(new Array[Byte](65536)).nn

      def recur(): DataStream =
        channel.read(buf) match
          case -1 =>
            channel.close()
            LazyList()
          case 0 =>
            recur()
          case count =>
            try
              buf.flip()
              val size = count min 65536
              val array = new Array[Byte](size)
              buf.get(array)
              buf.clear()
              array.immutable(using Unsafe) #:: recur()
            catch case e: Exception =>
              LazyList(throw StreamCutError()): DataStream
      
      recur()
    catch case err: Exception =>
      LazyList(throw StreamCutError()): DataStream

  def write(stream: DataStream, out: ji.OutputStream): Unit throws StreamCutError =
    stream.map(_.mutable(using Unsafe)).foreach(out.write(_))
