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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package locomotion

// A plain `System.nanoTime` harness for quick, dependency-free local timing of
// the Locomotion codec — a lighter alternative to the staged `Benchmarks` suite.
// It reuses the corpora and operations defined on `Benchmarks`.
object TimingMain:
  def time(label: String, bytes: Int, iterations: Int)(operation: () => Any): Unit =
    var warmup = 0
    while warmup < iterations/10 do { operation(); warmup += 1 }

    val start = System.nanoTime
    var index = 0
    while index < iterations do { operation(); index += 1 }
    val elapsed = System.nanoTime - start

    val nsPerOp = elapsed.toDouble/iterations
    val opsPerSec = 1e9/nsPerOp
    val mbPerSec = opsPerSec*bytes/(1024.0*1024.0)
    println:
      f"$label%-40s ${nsPerOp}%9.1f ns/op  ${opsPerSec.toLong}%14d ops/s  ${mbPerSec}%8.1f MB/s"

  def main(args: Array[String]): Unit =
    val iterations = if args.length > 0 then args(0).toInt else 1_000_000

    println(s"Timing $iterations iterations per benchmark.")
    println()

    println(f"${"Corpus 1: small message (3 fields)"}%-40s   payload=${Benchmarks.bytes1.length} bytes")
    time("  Decode", Benchmarks.bytes1.length, iterations)(() => Benchmarks.decodeSmall)
    time("  Encode", Benchmarks.bytes1.length, iterations)(() => Benchmarks.encodeSmall)
    time("  Walk (protobuf-java)", Benchmarks.bytes1.length, iterations):
      () => Benchmarks.walkWithProtobufJava(Benchmarks.raw1)
    println()

    println(f"${"Corpus 2: 100 user records"}%-40s   payload=${Benchmarks.bytes2.length} bytes")
    time("  Decode", Benchmarks.bytes2.length, iterations/100)(() => Benchmarks.decodeUsers)
    time("  Encode", Benchmarks.bytes2.length, iterations/100)(() => Benchmarks.encodeUsers)
    time("  Walk (protobuf-java)", Benchmarks.bytes2.length, iterations/100):
      () => Benchmarks.walkWithProtobufJava(Benchmarks.raw2)
    println()

    println(f"${"Corpus 4: 1000 packed integers"}%-40s   payload=${Benchmarks.bytes4.length} bytes")
    time("  Decode", Benchmarks.bytes4.length, iterations/100)(() => Benchmarks.decodeInts)
    time("  Encode", Benchmarks.bytes4.length, iterations/100)(() => Benchmarks.encodeInts)
    time("  Walk (protobuf-java)", Benchmarks.bytes4.length, iterations/100):
      () => Benchmarks.walkWithProtobufJava(Benchmarks.raw4)
    println()
