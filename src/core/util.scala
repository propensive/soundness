/*
    Turbulence, version 0.4.0. Copyright 2022-23 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

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
  
  def write(stream: LazyList[Text throws StreamCutError], out: ji.Writer): Unit throws StreamCutError =
    stream.foreach { text => out.write(text.s) }
