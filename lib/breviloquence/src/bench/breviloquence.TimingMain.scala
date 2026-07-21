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
┃    Soundness, version 0.64.0.                                                                    ┃
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
package breviloquence

import contingency.*, strategies.throwUnsafely
import gossamer.t

object TimingMain:
  def time(label: String, payload: IArray[Byte], iterations: Int)
    ( parse: IArray[Byte] => Any ): Unit =

    val raw = payload.asInstanceOf[Array[Byte]]

    // Warmup
    var w = 0
    while w < iterations/10 do
      parse(payload)
      w += 1

    val start = System.nanoTime
    var index = 0
    while index < iterations do
      parse(payload)
      index += 1
    val elapsed = System.nanoTime - start

    val nsPerOp = elapsed.toDouble/iterations
    val opsPerSec = 1e9/nsPerOp
    val mbPerSec = opsPerSec*payload.length/(1024.0*1024.0)
    println:
      f"$label%-40s ${nsPerOp}%9.1f ns/op  ${opsPerSec.toLong}%14d ops/s  ${mbPerSec}%8.1f MB/s"

  def main(args: Array[String]): Unit =
    val iterations =
      if args.length > 0 then args(0).toInt else 1_000_000

    println(t"Timing $iterations iterations per benchmark.")
    println()

    println:
      f"${"Corpus 1: small object (3 fields)"}%-40s   payload=${Benchmarks.cborBytes1.length} bytes"
    time("  Breviloquence", Benchmarks.cborBytes1, iterations)(Cbor.Ast.parse)
    time("  Jackson", Benchmarks.cborBytes1, iterations): bytes =>
      Benchmarks.parseWithJackson(bytes.asInstanceOf[Array[Byte]])
    time("  borer", Benchmarks.cborBytes1, iterations): bytes =>
      Benchmarks.parseWithBorer(bytes.asInstanceOf[Array[Byte]])
    println()

    println(f"${"Corpus 2: 100 user records"}%-40s   payload=${Benchmarks.cborBytes2.length} bytes")
    time("  Breviloquence", Benchmarks.cborBytes2, iterations/10)(Cbor.Ast.parse)
    time("  Jackson", Benchmarks.cborBytes2, iterations/10): bytes =>
      Benchmarks.parseWithJackson(bytes.asInstanceOf[Array[Byte]])
    time("  borer", Benchmarks.cborBytes2, iterations/10): bytes =>
      Benchmarks.parseWithBorer(bytes.asInstanceOf[Array[Byte]])
    println()

    println:
      f"${"Corpus 4: 1000 small integers"}%-40s   payload=${Benchmarks.cborBytes4.length} bytes"
    time("  Breviloquence", Benchmarks.cborBytes4, iterations/10)(Cbor.Ast.parse)
    time("  Jackson", Benchmarks.cborBytes4, iterations/10): bytes =>
      Benchmarks.parseWithJackson(bytes.asInstanceOf[Array[Byte]])
    time("  borer", Benchmarks.cborBytes4, iterations/10): bytes =>
      Benchmarks.parseWithBorer(bytes.asInstanceOf[Array[Byte]])
    println()
