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
package sedentary

import java.lang as jl

import scala.quoted.*

import ambience.*
import anthology.*
import anticipation.*
import contingency.*
import digression.*
import fulminate.*
import galilei.*
import gossamer.*
import hellenism.*
import inimitable.*
import jacinta.*
import nomenclature.*
import prepositional.*
import probably.*
import serpentine.*
import superlunary.*
import vacuous.*


// A profile test: where `Bench` measures how fast a fragment runs and `Stress` how it
// scales, `Profile` measures where its time goes. The body is run repeatedly on a single
// thread for a fixed wall-clock window while an in-process JFR recording samples execution;
// each sample's top frame — the method actually executing, i.e. self time — is aggregated,
// and the hottest methods are reported as a `Hotspots` row, rendered as a histogram with
// methods coloured by package.
//
// `frames` caps how many methods are reported (default 25). JFR's execution sampler only
// observes runnable threads, so this is a CPU profile: time spent parked or waiting is
// invisible. It samples all threads, though, so pipeline threads the body spawns are
// profiled along with the driver.
//
// `heap` and `cpus` pin the measurement JVM's resources, as on `Stress`.
case class Profile(heap: Optional[Text] = Unset, cpus: Optional[Int] = Unset)
  ( using Classloader, Environment )
  ( using device: BenchmarkDevice )
extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = Path on Linux
  type Transport = Json


  inline def apply[duration: Abstractable across Durations to Long, report]
    ( name: Message )
    ( target: duration, frames: Optional[Int] = Unset )
    ( body0: (References over Transport) ?=> Quotes ?=> Expr[Any] )
    ( using System, TemporaryDirectory, Stageable over Transport in Form )
    ( using runner:    Runner[report],
            inclusion: Inclusion[report, Hotspots],
            suite:     Testable,
            codepoint: Codepoint )
  :   Unit raises CompilerError raises RemoteError =

    val testId = TestId(name, suite, codepoint)
    val frames0: Optional[Int] = frames
    val frames2: Int = frames0.or(25)

    val body: (References over Transport) ?=> Quotes ?=> Expr[List[Text]] =
      val target2: Expr[Long] = Expr(target.generic)
      ' {
          // Blackhole sink, exactly as in `Bench`: each body result is written here via
          // lazySet so that the JIT cannot prove the body's value is unused and elide it. The
          // never-true read at the end forces the AtomicReference to escape, preventing
          // escape-analysis from scalarising the writes away.
          val sink = new java.util.concurrent.atomic.AtomicReference[Any](null)

          // Run 10 times initially as untimed warmup
          var w = 0

          while w < 10 do
            sink.lazySet($body0)
            w += 1

          // An in-process JFR recording of execution samples at a 1 ms period: a few
          // seconds of profiling yields thousands of samples, ample for a stable top-25.
          // `jdk.ExecutionSample` cannot observe a thread executing native code (a
          // zlib-bound pipeline would be nearly invisible), so `jdk.NativeMethodSample` is
          // recorded alongside it and the two sample streams aggregate together.
          val recording = new jdk.jfr.Recording()
          recording.enable("jdk.ExecutionSample").nn.withPeriod(java.time.Duration.ofMillis(1L))

          recording.enable("jdk.NativeMethodSample").nn
          . withPeriod(java.time.Duration.ofMillis(1L))

          recording.start()

          val deadline = jl.System.nanoTime + $target2

          while jl.System.nanoTime < deadline do sink.lazySet($body0)

          recording.stop()

          val file = java.nio.file.Files.createTempFile("profile", ".jfr").nn
          recording.dump(file)
          recording.close()

          // Aggregate each sample's top frame — the method the thread was actually
          // executing — keyed by class and method name.
          val counts = new java.util.HashMap[String, Long]()
          val events = jdk.jfr.consumer.RecordingFile.readAllEvents(file).nn
          java.nio.file.Files.deleteIfExists(file)
          var total = 0L
          var i = 0

          while i < events.size do
            val event = events.get(i).nn
            val stack = event.getStackTrace

            if stack != null && !stack.getFrames.nn.isEmpty then
              val frame = stack.getFrames.nn.get(0).nn
              val method = frame.getMethod

              if method != null then
                val cls = method.getType.nn.getName
                val key = (if cls == null then "?" else cls) + "\t" + method.getName
                val previous = counts.get(key)
                counts.put(key, (if previous == null then 0L else previous) + 1L)
                total += 1L

            i += 1

          // The hottest frames, in descending order of sample count, capped.
          val sorted =
            scala.jdk.CollectionConverters.MapHasAsScala(counts).asScala.to(List)
            . sortBy(-_(1))
            . take(${Expr(frames2)})

          total.toString.tt :: sorted.map: (key, count) =>
            (count.toString + "\t" + key).tt
        }

    val results = dispatch(body)

    val hotspots =
      Hotspots
        ( results.head.s.toLong,
          results.tail.map: line =>
            line.cut(t"\t").to(List) match
              case count :: className :: method :: Nil =>
                Hotspots.Frame
                  ( StackTrace.rewrite(className.s),
                    StackTrace.rewrite(method.s, method = true),
                    count.s.toLong )

              case other =>
                Hotspots.Frame(t"?", line, 0L) )

    inclusion.include(runner.report, testId, hotspots)


  def stage(out: Path on Linux): Path on Linux = unsafely:
    val uuid = Uuid()
    val name = t"$uuid.jar"
    val jarfile = out.peer(name)
    Bundler.bundle(out, jarfile, fqcn"superlunary.Executor")
    device.deploy(jarfile, uuid)
    jarfile

  protected val scalac: Scalac[3.7, Universe.Bytecode] = Scalac(List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Text, Path on Linux]): output =
    stage.remote: input =>
      unsafely(device.invoke(stage.target, input, heap, cpus))