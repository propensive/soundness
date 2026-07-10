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
package aviation

import scala.annotation.nowarn

import hellenism.*
import hypotenuse.*
import prepositional.*
import soundness.invoke
import xenophile.*

// The WIT definitions the navigation below is typechecked against, and which the `invoke`
// materializer consults (at its downstream expansion site) for the function's module id.
type WasiClockApi = Interface in Wit at "/aviation/clocks.wit"
given wasiClockApi: WasiClockApi = Interface[Wit](cp"/aviation/clocks.wit")

package clocks:
  // `inline`, so the `invoke` macro expands at the downstream summoning site: the Wasm Component
  // import (`scala.scalajs.wit.witImportCall`) only materializes in code compiled for a Wasm
  // target, where that intrinsic is on the classpath. Summoning it requires `wasiClockApi` (and
  // this module's WIT resource) to be visible at that site.
  //
  // The per-site duplication the compiler warns about is the point: the instance must materialize
  // at the downstream summoning site, and a WASI-linked application summons it once.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasiMonotonicClock: MonotonicClock = new MonotonicClock:
    def apply(): Instant over Monotonic =
      Instant.of[Monotonic](Foreign["monotonic-clock", Wit].`now`.invoke[U64].bits.s64.long)

  // The wall clock reads `wasi:clocks/wall-clock` `now`, whose `datetime` record crosses the
  // boundary as a `(seconds: U64, nanoseconds: U32)` pair, converted to POSIX millisecond ticks.
  @nowarn("msg=New anonymous class definition will be duplicated at each inline site")
  inline given wasiClock: Clock = new Clock:
    def apply(): Instant over Posix =
      val (seconds, nanoseconds) = Foreign["wall-clock", Wit].`now`.invoke[(U64, U32)]
      Instant.of[Posix](seconds.bits.s64.long*1000 + nanoseconds.bits.s32.int/1000000)
