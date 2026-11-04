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
package turbulence

import java.io as ji

import scala.language.experimental.pureFunctions

import anticipation.*
import beneficence.*
import vacuous.*

object Stdio:
  // A source of a canonical `Stdio`: typically a CLI invocation, which is itself a tracked
  // capability but whose `stdio` member is a pure value. The conditional given lives here — in
  // the companion of the SEARCHED type, so it needs no import — making a pure `Stdio`
  // summonable wherever a `Provider` is ambient.
  trait Provider:
    val stdio: Stdio

  given provided: (provider: Provider^) => Stdio = provider.stdio

  def apply
    ( out:     ji.PrintStream | Null,
      err:     ji.PrintStream | Null,
      in:      ji.InputStream | Null,
      termcap: Termcap )
  :   Stdio =

    val safeOut: ji.PrintStream = Optional(out).or(MutePrintStream)
    val safeErr: ji.PrintStream = Optional(err).or(MutePrintStream)
    val safeIn: ji.InputStream = Optional(in).or(MuteInputStream)
    val termcap2: Termcap = termcap

    new Stdio:
      val termcap: Termcap = termcap2
      val out: ji.PrintStream = safeOut
      val err: ji.PrintStream = safeErr
      val in: ji.InputStream = safeIn


  object MuteOutputStream extends ji.OutputStream:
    def write(byte: Int): Unit = ()
    override def write(array: scala.Array[Byte] | Null): Unit = ()
    override def write(array: scala.Array[Byte] | Null, offset: Int, length: Int): Unit = ()
    override def close(): Unit = ()

  lazy val MutePrintStream = ji.PrintStream(MuteOutputStream)

  // What a block printed through the JVM's own streams while they were captured, beside the
  // block's result. Both are decoded as UTF-8, the encoding the capturing streams wrote.
  case class Capture[result](result: result, out: Text, err: Text)

  // A diversion is the thread's, not the JVM's. The first one installs one dispatcher on each
  // of `System.out` and `System.err`, kept for the life of the JVM, through which every write
  // reaches the stream the writing thread is diverted to, or the original where it is not. A
  // thread started under a diversion starts diverted the same way (the binding is inheritable),
  // and parasite carries the diversion of the thread that spawns a task to whichever thread
  // runs it, so a diversion follows the hierarchy of structured concurrency, not the accident
  // of which thread a task lands on: a host may divert around several guests at once and their
  // output never crosses. Scala's own `println` is thread-local already (`Console.withOut`);
  // this covers Java prints and anything else that writes to the `System` streams. Nothing
  // reaches a diversion through `stdios.fileDescriptorStdio`, which names the process's own
  // descriptors directly.
  private val diversions: InheritableThreadLocal[Stdio | Null] = InheritableThreadLocal()

  private class Dispatcher(select: Stdio -> ji.PrintStream, original: ji.PrintStream)
  extends ji.OutputStream:
    private def target: ji.PrintStream = diversions.get match
      case null         => original
      case stdio: Stdio => select(stdio)

    def write(byte: Int): Unit = target.write(byte)

    override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
      target.write(bytes, offset, length)

    override def flush(): Unit = target.flush()

  private class Dispatching(dispatcher: Dispatcher)
  extends ji.PrintStream(dispatcher, true, "UTF-8")

  // Installed afresh whenever the stream in place is not a dispatcher: anything else that sets
  // `System.out` — a host's own capture, say, which restores what it found when it is done —
  // would otherwise leave every later diversion writing to a dispatcher no longer attached.
  private def install(): Unit = synchronized:
    System.out match
      case _: Dispatching => ()
      case out            => System.setOut(Dispatching(Dispatcher(_.out, out.nn)))

    System.err match
      case _: Dispatching => ()
      case err            => System.setErr(Dispatching(Dispatcher(_.err, err.nn)))

  // The stdio this thread's `System` writes are diverted to, if any: what a task carries to
  // the thread it runs on.
  def diverted: Optional[Stdio] = Optional(diversions.get)

  def divert[result](stdio: Stdio)(block: => result): result =
    install()
    val previous: Stdio | Null = diversions.get
    diversions.set(stdio)

    try block finally
      // A guest's streams need not auto-flush; what it printed last must not stay buffered.
      stdio.out.flush()
      stdio.err.flush()
      if previous == null then diversions.remove() else diversions.set(previous)

  def capture[result](block: => result): Capture[result] =
    val out: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()
    val err: ji.ByteArrayOutputStream = ji.ByteArrayOutputStream()

    val stdio: Stdio =
      Stdio
        ( ji.PrintStream(out, true, "UTF-8"),
          ji.PrintStream(err, true, "UTF-8"),
          null,
          termcapDefinitions.basicTermcap )

    val result: result = divert(stdio)(block)
    Capture(result, out.toString("UTF-8").nn.tt, err.toString("UTF-8").nn.tt)

  object MuteInputStream extends ji.InputStream:
    def read(): Int = -1
    override def read(array: scala.Array[Byte] | Null): Int = 0
    override def read(array: scala.Array[Byte] | Null, offset: Int, length: Int): Int = 0
    override def reset(): Unit = ()
    override def close(): Unit = ()
    override def available(): Int = 0

// Not a capability trait: `Stdio` is mixed into exclusive types (`ultimatum.Extent`,
// `exoskeleton.Invocation`) as an interface, and a standard-streams value built from the JVM's
// own, thread-safe streams captures nothing, so it is a plain value; one that retains something
// (a terminal, a flow) says so with `Stdio^{…}`.
trait Stdio extends Findable:
  val termcap: Termcap
  val out: ji.PrintStream
  val err: ji.PrintStream
  val in: ji.InputStream

  protected[turbulence] lazy val reader: ji.Reader = ji.InputStreamReader(in)

  // Whether a character is already readable without blocking. This consults
  // the same reader that character-level input streams from, so characters
  // buffered inside its decoder count — an `available()` check on the raw
  // `InputStream` would miss them.
  def ready(): Boolean = reader.ready()

  // Read one character from the shared reader (`-1` at end of input) — the
  // companion to `ready()`, for character-at-a-time consumers (a terminal).
  def readChar(): Int = reader.read()

  // `PrintStream.write` reads the array it is handed, which its signature cannot say.
  def write(bytes: Data): Unit = out.write(Array.unsafeJvm(bytes), 0, bytes.length)
  def print(text: Text): Unit = out.print(text.s)
  def writeErr(bytes: Data): Unit = err.write(Array.unsafeJvm(bytes), 0, bytes.length)
  def printErr(text: Text): Unit = err.print(text.s)
  def read(array: scala.Array[Byte]): Int = in.read(array, 0, array.length)
  def read(array: scala.Array[Char]): Int = reader.read(array, 0, array.length)
  def platform: Boolean = System.out == out && System.in == in && System.err == err
