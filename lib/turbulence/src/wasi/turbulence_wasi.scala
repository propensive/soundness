                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.63.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package turbulence

import java.io as ji

import scala.annotation.nowarn

import anticipation.*
import hellenism.*
import hypotenuse.*
import prepositional.*
import rudiments.*
import vacuous.*
import soundness.{invoke, dispose}
import xenophile.*

// The WIT definitions the navigation below is typechecked against, and which the `invoke`
// materializer consults (at its downstream expansion site) for module ids and resource methods.
type WasiCliApi = Interface in Wit at "/turbulence/cli.wit"
given wasiCliApi: WasiCliApi = Interface[Wit](cp"/turbulence/cli.wit")

package stdios:
  // A `Stdio` whose standard streams go through the WASI stream resources: each write obtains the
  // stream handle (`get-stdout`/`get-stderr`), invokes its `blocking-write-and-flush` method, and
  // disposes of the handle; reads do likewise with `get-stdin` and `blocking-read`. `inline`, so
  // the `invoke`s expand at the downstream summoning site: the Wasm Component imports only
  // materialize in code compiled for a Wasm target. Summoning it requires `wasiCliApi` (and this
  // module's WIT resource) to be visible at that site.
  //
  // The per-site duplication the compiler warns about is the point: the instances must materialize
  // at the downstream summoning site, and a WASI-linked application summons them once.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasiStdio: (termcap0: Termcap) => Stdio =
    def send(error: Boolean, data: Data): Unit =
      val handle =
        if error then Foreign["stderr", Wit].`get-stderr`.invoke[WitHandle of "output-stream"]
        else Foreign["stdout", Wit].`get-stdout`.invoke[WitHandle of "output-stream"]

      val stream: Foreign of "output-stream" from Wit = handle
      stream.`blocking-write-and-flush`(data).invoke[Unit]
      handle.dispose()

    // Byte-level writes follow the same path. (The `PrintStream`s exist for `Stdio`'s API;
    // `print`/`printErr` below bypass them, sending the text's UTF-8 bytes directly.)
    def wasiStream(error: Boolean): ji.OutputStream = new ji.OutputStream:
      def write(byte: Int): Unit = write(Array[Byte](byte.toByte), 0, 1)

      override def write(array: Array[Byte] | Null, offset: Int, length: Int): Unit =
        if array != null && length > 0 then
          val slice = java.util.Arrays.copyOfRange(array, offset, offset + length).nn
          send(error, slice.immutable(using Unsafe))

    // Reads block until at least one byte is available; a closed stream (the `Err` arm of
    // `blocking-read`'s result, raised by the decoder) is end-of-input.
    def wasiInput(): ji.InputStream = new ji.InputStream:
      def read(): Int =
        val array = new Array[Byte](1)
        if read(array, 0, 1) == -1 then -1 else array(0) & 0xff

      override def read(array: Array[Byte] | Null, offset: Int, length: Int): Int =
        if array == null || length == 0 then 0 else
          val handle = Foreign["stdin", Wit].`get-stdin`.invoke[WitHandle of "input-stream"]
          val stream: Foreign of "input-stream" from Wit = handle

          try
            val data = stream.`blocking-read`(U64(length.toLong.bits)).invoke[Data]
            var index = 0

            while index < data.length do
              array(offset + index) = data(index)
              index += 1

            if data.length == 0 then -1 else data.length
          catch case error: WitError => -1
          finally handle.dispose()

    def bytes(text: Text): Data = text.s.getBytes("UTF-8").nn.immutable(using Unsafe)

    new Stdio:
      val termcap: Termcap = termcap0
      val out: ji.PrintStream = ji.PrintStream(wasiStream(false), true)
      val err: ji.PrintStream = ji.PrintStream(wasiStream(true), true)
      val in: ji.InputStream = wasiInput()

      override def print(text: Text): Unit = send(false, bytes(text))
      override def printErr(text: Text): Unit = send(true, bytes(text))
