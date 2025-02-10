/*
    Galilei, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package galilei

import anticipation.*
import contingency.*
import prepositional.*
import proscenium.*
import serpentine.*
import turbulence.*

import java.nio.channels as jnc
import java.nio.file as jnf

object Openable:
  given [PlatformType <: Filesystem]
  =>   (read:        ReadAccess,
        write:       WriteAccess,
        dereference: DereferenceSymlinks,
        create:      CreateNonexistent on PlatformType,
        streamError: Tactic[StreamError],
        ioError:     Tactic[IoError])
  =>    (Path on PlatformType) is Openable by jnf.OpenOption into Handle = new Openable:

    type Self = Path on PlatformType
    type Operand = jnf.OpenOption
    type Result = Handle
    protected type Carrier = jnc.FileChannel

    def init(path: Path on PlatformType, extraOptions: List[jnf.OpenOption]): jnc.FileChannel =
      val options =
        read.options() ++ write.options() ++ dereference.options() ++ create.options()
        ++ extraOptions

      import jnf.StandardOpenOption as jnfsoo

      val options2 =
        if options.contains(jnfsoo.READ) && options.contains(jnfsoo.APPEND)
        then options.filter(_ != jnfsoo.READ)
        else options

      path.protect(IoError.Operation.Open)(jnc.FileChannel.open(path.javaPath, options2*).nn)

    def handle(channel: jnc.FileChannel): Handle =
      Handle
       (() => Readable.channel.stream(channel).stream[Bytes],
        Writable.channel.write(channel, _))


    def close(channel: jnc.FileChannel): Unit = channel.close()

  given openable: [FileType] => (openable: FileType is Openable by jnf.OpenOption)
  =>   Eof[FileType] is Openable by jnf.OpenOption into openable.Result = new Openable:
    type Self = Eof[FileType]
    type Operand = jnf.OpenOption
    type Result = openable.Result
    protected type Carrier = openable.Carrier

    def init(eof: Eof[FileType], options: List[Operand]): Carrier =
      openable.init(eof.file, jnf.StandardOpenOption.APPEND :: options)

    def handle(carrier: Carrier): Result = openable.handle(carrier)
    def close(carrier: Carrier): Unit = openable.close(carrier)

trait Openable:
  type Self
  type Operand
  type Result
  protected type Carrier

  def init(value: Self, options: List[Operand]): Carrier
  def handle(carrier: Carrier): Result

  def open[ResultType](value: Self, lambda: Result => ResultType, options: List[Operand])
  :     ResultType =
    val carrier = init(value, options)
    try lambda(handle(carrier)) finally close(carrier)

  def close(carrier: Carrier): Unit
