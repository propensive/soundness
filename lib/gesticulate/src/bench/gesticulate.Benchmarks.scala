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
package gesticulate

import scala.quoted.*

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets.US_ASCII

import ambience.*, environments.javaBaseEnvironment, systems.javaBaseSystem
import anticipation.*
import contingency.*, strategies.throwUnsafely
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import probably.*
import proscenium.*
import quantitative.*
import rudiments.*
import sedentary.*
import symbolism.*
import temporaryDirectories.systemTemporaryDirectory
import vacuous.*

object Benchmarks extends Suite(m"Gesticulate benchmarks"):
  sealed trait Information extends Dimension
  sealed trait Bytes[Power <: Nat] extends Units[Power, Information]
  val Byte: MetricUnit[Bytes[1]] = MetricUnit(1.0)

  given byteDesignation: Designation[Bytes[1]] = () => t"B"
  given decimalizer:     Decimalizer            = Decimalizer(2)
  given device:          BenchmarkDevice        = LocalhostDevice
  given prefixes:        Prefixes               = Prefixes(List(Kilo, Mega, Giga, Tera))

  // ─── inputs ───────────────────────────────────────────────────────────────

  val boundary: String = "----SoundnessBenchmarkBoundary7MA4YWxkTrZu0gW"

  // One workload's wire bytes cut into blocks of `size` bytes: the `Chain[Data]` read by
  // `Multipart.parse`, and the same blocks as plain byte arrays for the rivals, so that no arm
  // pays for the conversion inside the timed region.
  final class Split(wire: scala.Array[Byte], size: Int):
    val blocks: scala.Array[scala.Array[Byte]] =
      scala.Array.tabulate((wire.length + size - 1)/size): index =>
        java.util.Arrays.copyOfRange(wire, index*size, math.min(wire.length, (index + 1)*size)).nn

    val chain: Chain[Data] = Chain.from(blocks.iterator.map(Array.unsafeFrozen(_)).toList)

  // A multipart body, cut into 64 KiB blocks (a typical socket read) and into 1 KiB blocks
  // (which make the boundary scan and its lookahead cross block edges far more often).
  final class Workload(val wire: scala.Array[Byte]):
    val large: Split = Split(wire, 65536)
    val small: Split = Split(wire, 1024)

  private def encode(build: ByteArrayOutputStream => Unit): Workload =
    val out = ByteArrayOutputStream()
    build(out)
    out.write(s"--$boundary--\r\n".getBytes(US_ASCII).nn)
    Workload(out.toByteArray.nn)

  private def part(out: ByteArrayOutputStream, headers: String, body: scala.Array[Byte]): Unit =
    out.write(s"--$boundary\r\n$headers\r\n".getBytes(US_ASCII).nn)
    out.write(body)
    out.write("\r\n".getBytes(US_ASCII).nn)

  private def field(out: ByteArrayOutputStream, name: String, value: String): Unit =
    part(out, s"Content-Disposition: form-data; name=\"$name\"\r\n", value.getBytes(US_ASCII).nn)

  private def file(out: ByteArrayOutputStream, name: String, size: Int, seed: Int): Unit =
    val body = new scala.Array[Byte](size)
    java.util.Random(seed).nextBytes(body)

    val headers =
      s"Content-Disposition: form-data; name=\"$name\"; filename=\"$name.bin\"\r\n"
      + "Content-Type: application/octet-stream\r\n"

    part(out, headers, body)

  // Fifty short text fields: header parsing dominates.
  lazy val form: Workload = encode: out =>
    for index <- 0 until 50
    do field(out, s"field$index", s"the value of form field number $index")

  // One field and a 1 MiB binary file: the body scan dominates, and random bytes hold a `\r`
  // about once in 256, each of which starts a boundary lookahead that fails.
  lazy val upload: Workload = encode: out =>
    field(out, "description", "a single large upload")
    file(out, "upload", 1 << 20, 1)

  // Sixteen 64 KiB binary files.
  lazy val batch: Workload = encode: out =>
    for index <- 0 until 16 do file(out, s"file$index", 1 << 16, index + 2)

  // ─── helpers (called from quoted bench bodies) ────────────────────────────

  // Drains every part and every block of every body, as a consumer of the request would, and
  // returns the total body bytes.
  def soundness(blocks: Chain[Data]): Long =
    var total = 0L

    Multipart.parse(blocks).parts.each: part =>
      part.body.each: block =>
        total += block.length

    total

  def jetty(blocks: scala.Array[scala.Array[Byte]]): Long = Rivals.Jetty.parse(boundary, blocks)

  def fileUpload(blocks: scala.Array[scala.Array[Byte]]): Long =
    Rivals.FileUpload.parse(boundary, blocks)

  // ─── agreement check ──────────────────────────────────────────────────────

  private def agree(name: Text, split: Split): Unit =
    val expected = soundness(split.chain)
    val jettyTotal = jetty(split.blocks)
    val fileUploadTotal = fileUpload(split.blocks)
    assert(jettyTotal == expected, s"$name: Soundness $expected ≠ Jetty $jettyTotal")
    assert(fileUploadTotal == expected, s"$name: Soundness $expected ≠ fileupload $fileUploadTotal")

  // ─── benchmarks ───────────────────────────────────────────────────────────

  def run(): Unit =
    val bench = Bench()

    agree(t"form, 64 KiB", form.large)
    agree(t"form, 1 KiB", form.small)
    agree(t"upload, 64 KiB", upload.large)
    agree(t"upload, 1 KiB", upload.small)
    agree(t"batch, 64 KiB", batch.large)
    agree(t"batch, 1 KiB", batch.small)

    val formSize:   Quantity[Bytes[1]] = form.wire.length*Byte
    val uploadSize: Quantity[Bytes[1]] = upload.wire.length*Byte
    val batchSize:  Quantity[Bytes[1]] = batch.wire.length*Byte

    suite(m"Form: 50 short text fields"):
      bench(m"Multipart.parse, 64 KiB blocks")(target = 1*Second, operationSize = formSize):
        '{ gesticulate.Benchmarks.soundness(gesticulate.Benchmarks.form.large.chain) }

      bench(m"Jetty MultiPart.Parser, 64 KiB blocks")(target = 1*Second, operationSize = formSize):
        '{ gesticulate.Benchmarks.jetty(gesticulate.Benchmarks.form.large.blocks) }

      bench(m"fileupload MultipartInput, 64 KiB blocks")
        ( target = 1*Second, operationSize = formSize ):
        '{ gesticulate.Benchmarks.fileUpload(gesticulate.Benchmarks.form.large.blocks) }

      bench(m"Multipart.parse, 1 KiB blocks")(target = 1*Second, operationSize = formSize):
        '{ gesticulate.Benchmarks.soundness(gesticulate.Benchmarks.form.small.chain) }

      bench(m"Jetty MultiPart.Parser, 1 KiB blocks")(target = 1*Second, operationSize = formSize):
        '{ gesticulate.Benchmarks.jetty(gesticulate.Benchmarks.form.small.blocks) }

      bench(m"fileupload MultipartInput, 1 KiB blocks")
        ( target = 1*Second, operationSize = formSize ):
        '{ gesticulate.Benchmarks.fileUpload(gesticulate.Benchmarks.form.small.blocks) }

    suite(m"Upload: one 1 MiB binary file"):
      bench(m"Multipart.parse, 64 KiB blocks")(target = 1*Second, operationSize = uploadSize):
        '{ gesticulate.Benchmarks.soundness(gesticulate.Benchmarks.upload.large.chain) }

      bench(m"Jetty MultiPart.Parser, 64 KiB blocks")
        ( target = 1*Second, operationSize = uploadSize ):
        '{ gesticulate.Benchmarks.jetty(gesticulate.Benchmarks.upload.large.blocks) }

      bench(m"fileupload MultipartInput, 64 KiB blocks")
        ( target = 1*Second, operationSize = uploadSize ):
        '{ gesticulate.Benchmarks.fileUpload(gesticulate.Benchmarks.upload.large.blocks) }

      bench(m"Multipart.parse, 1 KiB blocks")(target = 1*Second, operationSize = uploadSize):
        '{ gesticulate.Benchmarks.soundness(gesticulate.Benchmarks.upload.small.chain) }

      bench(m"Jetty MultiPart.Parser, 1 KiB blocks")
        ( target = 1*Second, operationSize = uploadSize ):
        '{ gesticulate.Benchmarks.jetty(gesticulate.Benchmarks.upload.small.blocks) }

      bench(m"fileupload MultipartInput, 1 KiB blocks")
        ( target = 1*Second, operationSize = uploadSize ):
        '{ gesticulate.Benchmarks.fileUpload(gesticulate.Benchmarks.upload.small.blocks) }

    suite(m"Batch: sixteen 64 KiB binary files"):
      bench(m"Multipart.parse, 64 KiB blocks")(target = 1*Second, operationSize = batchSize):
        '{ gesticulate.Benchmarks.soundness(gesticulate.Benchmarks.batch.large.chain) }

      bench(m"Jetty MultiPart.Parser, 64 KiB blocks")
        ( target = 1*Second, operationSize = batchSize ):
        '{ gesticulate.Benchmarks.jetty(gesticulate.Benchmarks.batch.large.blocks) }

      bench(m"fileupload MultipartInput, 64 KiB blocks")
        ( target = 1*Second, operationSize = batchSize ):
        '{ gesticulate.Benchmarks.fileUpload(gesticulate.Benchmarks.batch.large.blocks) }

      bench(m"Multipart.parse, 1 KiB blocks")(target = 1*Second, operationSize = batchSize):
        '{ gesticulate.Benchmarks.soundness(gesticulate.Benchmarks.batch.small.chain) }

      bench(m"Jetty MultiPart.Parser, 1 KiB blocks")
        ( target = 1*Second, operationSize = batchSize ):
        '{ gesticulate.Benchmarks.jetty(gesticulate.Benchmarks.batch.small.blocks) }

      bench(m"fileupload MultipartInput, 1 KiB blocks")
        ( target = 1*Second, operationSize = batchSize ):
        '{ gesticulate.Benchmarks.fileUpload(gesticulate.Benchmarks.batch.small.blocks) }
