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
package guillotine

import java.io as ji
import java.util.concurrent as juc

import scala.annotation.tailrec
import scala.scalanative.libc.errno as cerrno
import scala.scalanative.libc.stdlib as cstdlib
import scala.scalanative.posix.fcntl
import scala.scalanative.posix.sys.ioctl
import scala.scalanative.unsafe.*
import scala.scalanative.unsigned.*
import scala.unsafeExceptions.canThrowAny

import rudiments.*
import vacuous.*

// The Scala Native twin of the JVM `PtyProcess`: the same pseudo-terminal construction — the
// master from `posix_openpt`, the child started on its slave by `posix_spawn` as a session leader,
// the master pumped continuously — over Scala Native's C interop instead of the foreign function
// API. Everything the JVM version binds by name is declared `extern` here, except the three
// variadic calls (`open`, `fcntl`, `ioctl`), which go through posixlib's glue so that the
// variadic calling convention is honoured on every platform. The constants, offsets and the macOS
// `/bin/bash` trampoline are the JVM's; see that file for the reasoning behind each.
private[guillotine] object PtyProcess:
  // The master's descriptor, closed at most once, and only by the pump, once it has stopped
  // reading: a descriptor number is reused as soon as it is closed, so a second `close`, or a read
  // racing a `close`, could reach some unrelated file.
  class Descriptor(val fd: Int):
    private val done: Atomic[Boolean] = Atomic(false)
    def closed: Boolean = done()
    def close(): Unit = if !done.ere(true) then PtyProcess.close(fd)

  val BufferSize: Int = 8192
  val Chunks: Int = 256
  val HangUp: Int = 1
  val Terminate: Int = 15
  val Kill: Int = 9

  private val Patience: Int = 100
  private val Readable: Short = 1 // `POLLIN`

  private val macOs: Boolean = System.getProperty("os.name").nn.startsWith("Mac")

  private val ReadWrite: Int = 2
  private val NoControllingTerminal: Int = if macOs then 0x20000 else 0x100
  private val CloseOnExec: Int = if macOs then 0x1000000 else 0x80000
  private val SetDescriptorFlags: Int = 2
  private val DescriptorCloseOnExec: Int = 1
  private val SetWindowSize: Long = if macOs then 0x80087467L else 0x5414L

  // The layout of `struct termios`: four flag words then the control characters, as the JVM
  // version reads them.
  private val TermiosSize: Int = 128
  private val FlagWidth: Int = if macOs then 8 else 4
  private val InputFlags: Int = 0
  private val OutputFlags: Int = FlagWidth
  private val LocalFlags: Int = FlagWidth*3
  private val Characters: Int = if macOs then 32 else 17
  private val MinimumIndex: Int = if macOs then 16 else 6
  private val TimeoutIndex: Int = if macOs then 17 else 5
  private val Now: Int = 0 // `TCSANOW`

  private val Echo: Long = 0x8L
  private val Canonical: Long = if macOs then 0x100L else 0x2L
  private val Signals: Long = if macOs then 0x80L else 0x1L
  private val Extensions: Long = if macOs then 0x400L else 0x8000L
  private val TranslateReturn: Long = 0x100L
  private val FlowControl: Long = if macOs then 0x200L else 0x400L
  private val Postprocess: Long = 0x1L
  private val NewlineReturn: Long = if macOs then 0x2L else 0x4L
  private val Interrupted: Int = 4

  // `POSIX_SPAWN_SETSID` makes the child a session leader, so the first terminal it opens — the
  // slave, as its standard input — becomes its controlling terminal. macOS can also close every
  // descriptor the child is not explicitly given. Every signal is unblocked and has its default
  // disposition in the child.
  private val SpawnFlags: Short =
    val common = 0x04 | 0x08 // `POSIX_SPAWN_SETSIGDEF`, `POSIX_SPAWN_SETSIGMASK`
    (if macOs then common | 0x400 | 0x4000 else common | 0x80).toShort

  // The sizes the opaque POSIX structures are allocated at: generous, as the JVM version's.
  private val ActionsSize: Int = 1024
  private val AttributesSize: Int = 1024
  private val SignalSetSize: Int = 128

  @extern
  private object libc:
    def posix_openpt(flags: CInt): CInt = extern
    def grantpt(fd: CInt): CInt = extern
    def unlockpt(fd: CInt): CInt = extern
    def ptsname_r(fd: CInt, buffer: Ptr[Byte], length: CSize): CInt = extern
    def close(fd: CInt): CInt = extern
    def read(fd: CInt, buffer: Ptr[Byte], length: CSize): CSSize = extern
    def write(fd: CInt, buffer: Ptr[Byte], length: CSize): CSSize = extern
    def waitpid(pid: CInt, status: Ptr[CInt], options: CInt): CInt = extern
    def kill(pid: CInt, signal: CInt): CInt = extern
    def poll(descriptors: Ptr[Byte], count: ULong, timeout: CInt): CInt = extern
    def sigfillset(set: Ptr[Byte]): CInt = extern
    def tcgetattr(fd: CInt, termios: Ptr[Byte]): CInt = extern
    def tcsetattr(fd: CInt, actions: CInt, termios: Ptr[Byte]): CInt = extern
    def tcgetpgrp(fd: CInt): CInt = extern
    def posix_spawn_file_actions_init(actions: Ptr[Byte]): CInt = extern
    def posix_spawn_file_actions_destroy(actions: Ptr[Byte]): CInt = extern
    def posix_spawn_file_actions_addchdir_np(actions: Ptr[Byte], path: CString): CInt = extern
    def posix_spawn_file_actions_adddup2(actions: Ptr[Byte], fd: CInt, newFd: CInt): CInt = extern

    def posix_spawn_file_actions_addopen
      ( actions: Ptr[Byte], fd: CInt, path: CString, flags: CInt, mode: CInt )
    :   CInt =

      extern

    def posix_spawnattr_init(attributes: Ptr[Byte]): CInt = extern
    def posix_spawnattr_destroy(attributes: Ptr[Byte]): CInt = extern
    def posix_spawnattr_setsigmask(attributes: Ptr[Byte], set: Ptr[Byte]): CInt = extern
    def posix_spawnattr_setsigdefault(attributes: Ptr[Byte], set: Ptr[Byte]): CInt = extern
    def posix_spawnattr_setflags(attributes: Ptr[Byte], flags: CShort): CInt = extern

    def posix_spawn
      ( pid:         Ptr[CInt],
        path:        CString,
        actions:     Ptr[Byte],
        attributes:  Ptr[Byte],
        arguments:   Ptr[CString],
        environment: Ptr[CString] )
    :   CInt = extern

  private def errorNumber: Int = cerrno.errno

  // Failures are `java.io.IOException`s, as the `java.io` streams of a `java.lang.Process` must
  // report them; `OnTerminal.lend` turns one from spawning into an `Exec.Error`.
  //
  // A call returning `-1` and setting `errno` on failure.
  private def check(call: String)(result: Int): Int =
    if result < 0 then throw ji.IOException(call+" failed with errno "+errorNumber) else result

  // A call returning an error number itself, and zero on success.
  private def succeed(call: String)(result: Int): Unit =
    if result != 0 then throw ji.IOException(call+" failed with error "+result)

  def close(fd: Int): Unit = libc.close(fd)
  def signal(pid: Int, signal: Int): Unit = libc.kill(pid, signal)

  @tailrec
  def read(fd: Int, buffer: Ptr[Byte], length: Int): Int =
    val count = libc.read(fd, buffer, length.toCSize).toInt
    if count < 0 && errorNumber == Interrupted then read(fd, buffer, length) else count

  @tailrec
  def write(master: Descriptor, buffer: Ptr[Byte], length: Int): Unit =
    if master.closed then throw ji.IOException("the terminal has been closed")

    if length > 0 then
      val count = libc.write(master.fd, buffer, length.toCSize).toInt

      if count >= 0 then write(master, buffer + count, length - count)
      else if errorNumber == Interrupted then write(master, buffer, length)
      else throw ji.IOException("writing to the terminal failed with errno "+errorNumber)

  // Copies `count` bytes of a native buffer into a fresh frozen array.
  private def frozen(buffer: Ptr[Byte], count: Int): Array[Byte]^{} =
    val array = Array.allocate[Byte](count)
    copyOut(buffer, array, 0, count)
    Array.freeze(array)

  private def copyOut(buffer: Ptr[Byte], array: Array[Byte]^, index: Int, count: Int): Unit =
    if index < count then
      array(index) = buffer(index)
      copyOut(buffer, array, index + 1, count)

  private def copyIn
    ( bytes: scala.Array[Byte], offset: Int, buffer: Ptr[Byte], index: Int, count: Int )
  :   Unit =

    if index < count then
      buffer(index) = bytes(offset + index)
      copyIn(bytes, offset, buffer, index + 1, count)

  // The master is read continuously from the moment the child starts: macOS discards whatever a
  // child has written to its terminal, but nobody has read, when the child exits, so the output
  // cannot wait until it is asked for. It is queued instead, up to a bound past which the pump
  // stops reading and the child blocks on writing, as it would on a full pipe. Linux reports the
  // end of the output with `EIO`, and macOS with an empty read; either ends the pump, which then
  // queues an empty chunk to say so.
  //
  // The pump never blocks indefinitely, in `read` or on the queue, so that it notices `stop`
  // promptly: a background job the child started may hold the terminal open long after the child
  // has exited, and the master can only be closed safely by the thread which reads it.
  def pump
    ( pid:    Int,
      master: Descriptor,
      chunks: juc.BlockingQueue[Array[Byte]^{}],
      stop:   Atomic[Boolean] )
  :   Thread =

    Thread.ofPlatform().nn.daemon().nn.name("guillotine-pty-pump-"+pid).nn.start: () =>
      val buffer: Ptr[Byte] = cstdlib.malloc(BufferSize.toCSize)
      // A `struct pollfd`: the descriptor, then the requested and returned events as shorts.
      val descriptor: Ptr[Byte] = cstdlib.malloc(8.toCSize)
      !descriptor.asInstanceOf[Ptr[CInt]] = master.fd
      !(descriptor + 4).asInstanceOf[Ptr[CShort]] = Readable

      @tailrec
      def enqueue(bytes: Array[Byte]^{}): Unit =
        if !stop() && !chunks.offer(bytes, Patience.toLong, juc.TimeUnit.MILLISECONDS)
        then enqueue(bytes)

      @tailrec
      def recur(): Unit = if !stop() then
        val ready = libc.poll(descriptor, 1.toULong, Patience)

        if ready == 0 || ready < 0 && errorNumber == Interrupted then recur()
        else if ready > 0 then
          val count = read(master.fd, buffer, BufferSize)

          if count > 0 then
            enqueue(frozen(buffer, count))
            recur()

      try
        recur()
        enqueue(Array.empty[Byte])
      catch case _: InterruptedException => ()
      finally
        master.close()
        cstdlib.free(buffer)
        cstdlib.free(descriptor)

    . nn

  // The child's exit status, decoded as the JDK decodes it: its exit code if it exited, and 128
  // plus the signal number if a signal ended it.
  def await(pid: Int): Int =
    val status: Ptr[CInt] = stackalloc[CInt]()

    @tailrec
    def recur(): Int =
      val result = libc.waitpid(pid, status, 0)

      if result < 0 && errorNumber == Interrupted then recur()
      else if result < 0 then 0x80 + Kill
      else
        val value = !status
        if (value & 0x7f) == 0 then (value >> 8) & 0xff else 0x80 + (value & 0x7f)

    recur()

  def resize(fd: Int, width: Int, height: Int): Unit =
    val size: Ptr[CShort] = stackalloc[CShort](4)
    size(0) = height.toShort
    size(1) = width.toShort
    ioctl.ioctl(fd, SetWindowSize.toSize, size.asInstanceOf[Ptr[Byte]])

  private def flags(termios: Ptr[Byte], offset: Int): Long =
    if macOs then !(termios + offset).asInstanceOf[Ptr[Long]]
    else (!(termios + offset).asInstanceOf[Ptr[CInt]]).toLong & 0xffffffffL

  private def flags(termios: Ptr[Byte], offset: Int, value: Long): Unit =
    if macOs then !(termios + offset).asInstanceOf[Ptr[Long]] = value
    else !(termios + offset).asInstanceOf[Ptr[CInt]] = value.toInt

  private def switch(word: Long, flag: Long, on: Boolean): Long =
    if on then word | flag else word & ~flag

  // The line discipline of the terminal `fd` is a side of, as the switches `Termios` names.
  def mode(fd: Int): Termios =
    val termios: Ptr[Byte] = stackalloc[Byte](TermiosSize)
    check("tcgetattr")(libc.tcgetattr(fd, termios))
    val input = flags(termios, InputFlags)
    val output = flags(termios, OutputFlags)
    val local = flags(termios, LocalFlags)

    Termios
      ( echo            = (local & Echo) != 0,
        canonical       = (local & Canonical) != 0,
        signals         = (local & Signals) != 0,
        extensions      = (local & Extensions) != 0,
        translateReturn = (input & TranslateReturn) != 0,
        flowControl     = (input & FlowControl) != 0,
        postprocessing  = (output & Postprocess) != 0 )

  // Sets the switches `Termios` names, leaving the rest of the discipline as it is. Without
  // canonical input, a read returns as soon as one byte has arrived, which is what `raw` means.
  def mode(fd: Int, mode: Termios): Unit =
    val termios: Ptr[Byte] = stackalloc[Byte](TermiosSize)
    check("tcgetattr")(libc.tcgetattr(fd, termios))

    val local =
      switch(switch(switch(switch(flags(termios, LocalFlags), Echo, mode.echo),
          Canonical, mode.canonical), Signals, mode.signals), Extensions, mode.extensions)

    val input =
      switch(switch(flags(termios, InputFlags), TranslateReturn, mode.translateReturn),
          FlowControl, mode.flowControl)

    val output =
      switch(switch(flags(termios, OutputFlags), Postprocess, mode.postprocessing),
          NewlineReturn, mode.postprocessing)

    flags(termios, LocalFlags, local)
    flags(termios, InputFlags, input)
    flags(termios, OutputFlags, output)

    if !mode.canonical then
      termios(Characters + MinimumIndex) = 1.toByte
      termios(Characters + TimeoutIndex) = 0.toByte

    check("tcsetattr")(libc.tcsetattr(fd, Now, termios))

  // A null-terminated array of C strings, allocated in `zone`.
  private def strings(values: java.util.List[String])(using zone: Zone): Ptr[CString] =
    val array: Ptr[CString] = alloc[CString](values.size + 1)

    @tailrec
    def recur(index: Int): Unit = if index < values.size then
      array(index) = toCString(values.get(index).nn)
      recur(index + 1)

    recur(0)
    array(values.size) = null.asInstanceOf[CString]
    array

  // Starts the command `builder` describes — its arguments, environment and working directory —
  // on a fresh pseudo-terminal of the given size and, if one is given, line discipline.
  def apply(builder: ProcessBuilder, width: Int, height: Int, termios: Optional[Termios])
  :   PtyProcess =

    val flags = ReadWrite | NoControllingTerminal
    val master = check("posix_openpt")(libc.posix_openpt(flags))

    try
      fcntl.fcntl(master, SetDescriptorFlags, DescriptorCloseOnExec)
      check("grantpt")(libc.grantpt(master))
      check("unlockpt")(libc.unlockpt(master))
      val name: Ptr[Byte] = stackalloc[Byte](1024)
      succeed("ptsname_r")(libc.ptsname_r(master, name, 1024.toCSize))

      // macOS will not size the terminal until its slave has been opened once, so it is opened
      // here too, and closed again once the child has opened its own.
      val slave = check("open")(fcntl.open(name, flags | CloseOnExec, 0.toUInt))

      try
        resize(master, width, height)
        termios.let(mode(slave, _))
        val pid = spawn(builder, fromCString(name))
        settle(master, pid)
        new PtyProcess(pid, Descriptor(master))
      finally close(slave)

    catch case error: ji.IOException =>
      close(master)
      throw error

  // On macOS, `posix_spawn` performs its file actions before `POSIX_SPAWN_SETSID` takes effect, so
  // the slave, though opened as the child's standard input, is opened before the child leads a
  // session, and never becomes its controlling terminal; the command is started through
  // `/bin/bash`, which opens the slave by its path once it leads the session, closes it again, and
  // replaces itself with the command (see the JVM version). Linux needs none of this.
  private def command(builder: ProcessBuilder, slave: String): java.util.List[String] =
    val command = java.util.ArrayList[String](builder.command().nn)
    command.set(0, executable(builder))

    if !macOs then command else
      val trampoline = java.util.ArrayList[String]()
      trampoline.add("/bin/bash")
      trampoline.add("-c")
      trampoline.add("exec 3<>\"$0\" 3>&-; exec \"$@\"")
      trampoline.add(slave)
      trampoline.addAll(command)
      trampoline

  // The executable a command names, found on the `PATH` as `posix_spawnp` would find it, resolved
  // before spawning so that a missing command fails to start, as it does on pipes.
  private def executable(builder: ProcessBuilder): String =
    val name = builder.command().nn.get(0).nn

    def runnable(path: String): Boolean =
      val file = ji.File(path)
      file.isFile && file.canExecute

    if name.contains("/") then
      if runnable(name) then name else throw ji.IOException("cannot execute "+name)
    else
      val search = builder.environment().nn.get("PATH")
      val directories = (if search == null then "" else search).split(":").nn

      @tailrec
      def recur(index: Int): String =
        if index >= directories.length then throw ji.IOException(name+" was not found")
        else
          val directory = directories(index).nn
          val candidate = directory+"/"+name
          if directory.nonEmpty && runnable(candidate) then candidate else recur(index + 1)

      recur(0)

  // On macOS the trampoline makes the terminal the child's controlling terminal a moment after the
  // child starts; the terminal is not lent out until the child is its foreground group or a second
  // has passed (the JVM version also notices an early exit, through `waitid`'s `WNOWAIT`, which
  // posixlib does not expose; an exiting child costs at most the second here).
  private def settle(master: Int, pid: Int): Unit =
    val deadline = System.nanoTime + 1_000_000_000L

    @tailrec
    def recur(): Unit =
      if libc.tcgetpgrp(master) != pid && System.nanoTime < deadline then
        Thread.sleep(1L)
        recur()

    if macOs then recur()

  private def spawn(builder: ProcessBuilder, slave: String): Int = Zone.acquire: zone =>
    given Zone = zone
    val command = this.command(builder, slave)
    val variables = java.util.ArrayList[String]()

    builder.environment().nn.forEach: (name, value) => variables.add(name.nn+"="+value.nn)

    val actions: Ptr[Byte] = alloc[Byte](ActionsSize)
    val attributes: Ptr[Byte] = alloc[Byte](AttributesSize)
    val noSignals: Ptr[Byte] = alloc[Byte](SignalSetSize)
    val allSignals: Ptr[Byte] = alloc[Byte](SignalSetSize)
    libc.sigfillset(allSignals)

    succeed("posix_spawn_file_actions_init")(libc.posix_spawn_file_actions_init(actions))

    try
      succeed("posix_spawnattr_init")(libc.posix_spawnattr_init(attributes))

      try
        val slavePath = toCString(slave)

        succeed("addopen"):
          libc.posix_spawn_file_actions_addopen(actions, 0, slavePath, ReadWrite, 0)

        succeed("adddup2")(libc.posix_spawn_file_actions_adddup2(actions, 0, 1))
        succeed("adddup2")(libc.posix_spawn_file_actions_adddup2(actions, 0, 2))
        val directory = builder.directory()

        if directory != null then
          val path = toCString(directory.getPath.nn)
          succeed("addchdir_np")(libc.posix_spawn_file_actions_addchdir_np(actions, path))

        succeed("setflags")(libc.posix_spawnattr_setflags(attributes, SpawnFlags))
        succeed("setsigmask")(libc.posix_spawnattr_setsigmask(attributes, noSignals))
        succeed("setsigdefault")(libc.posix_spawnattr_setsigdefault(attributes, allSignals))

        val pid: Ptr[CInt] = alloc[CInt]()
        val path = toCString(command.get(0).nn)
        val arguments = strings(command)
        val environment = strings(variables)

        succeed("posix_spawn"):
          libc.posix_spawn(pid, path, actions, attributes, arguments, environment)

        !pid

      finally libc.posix_spawnattr_destroy(attributes)
    finally libc.posix_spawn_file_actions_destroy(actions)

// A child process running on a pseudo-terminal, presented as a `java.lang.Process` so that `Job`
// drives it exactly as it drives a process on pipes. Its output and its input are both the master
// side of the terminal, and it has no separate error stream: the child's standard error is the
// terminal too.
private[guillotine] class PtyProcess private (processId: Int, master: PtyProcess.Descriptor)
extends java.lang.Process:
  private val exit: juc.CompletableFuture[Integer] = juc.CompletableFuture[Integer]().nn
  private val chunks = juc.ArrayBlockingQueue[Array[Byte]^{}](PtyProcess.Chunks)
  private val stop: Atomic[Boolean] = Atomic(false)
  private val pump: Thread = PtyProcess.pump(processId, master, chunks, stop)

  // `waitpid` blocks, so it runs on a thread of its own rather than on whichever thread first asks
  // for the exit status; reaping the child at once also means it never lingers as a zombie.
  Thread.ofPlatform().nn.daemon().nn.name("guillotine-pty-reaper-"+processId).nn.start: () =>
    exit.complete(Integer.valueOf(PtyProcess.await(processId)))

  private lazy val input: ji.InputStream = new ji.InputStream:
    // A JDK class cannot extend `Stateful`, so its state is held in atomic cells rather than
    // `var`s; it is only ever read and written behind this stream's lock.
    private val chunk: Atomic.Ref[Array[Byte]^{}] = Atomic.Ref(Array.empty[Byte])
    private val position: Atomic[Int] = Atomic(0)
    private val ended: Atomic[Boolean] = Atomic(false)

    override def read(): Int =
      val bytes = scala.Array[Byte](0)
      if read(bytes, 0, 1) < 0 then -1 else bytes(0) & 0xff

    override def read(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Int =
      synchronized:
        if !ended() && length > 0 && position() == chunk().length then
          val next = chunks.take().asInstanceOf[Array[Byte]^{}]
          chunk() = next
          position() = 0
          ended() = next.length == 0

        if length == 0 then 0 else if ended() then -1 else
          val current = chunk()
          val count = Math.min(length, current.length - position())
          System.arraycopy(current.asInstanceOf[AnyRef], position(), bytes, offset, count)
          position() = position() + count
          count

    override def available(): Int = synchronized(chunk().length - position())

  private lazy val output: ji.OutputStream = new ji.OutputStream:
    private val buffer: Ptr[Byte] = cstdlib.malloc(PtyProcess.BufferSize.toCSize)

    override def write(byte: Int): Unit = write(scala.Array[Byte](byte.toByte), 0, 1)

    override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
      synchronized:
        if length > 0 then
          val chunk = Math.min(length, PtyProcess.BufferSize)
          PtyProcess.copyIn(bytes.nn, offset, buffer, 0, chunk)
          PtyProcess.write(master, buffer, chunk)
          write(bytes, offset + chunk, length - chunk)

    // A terminal has no end of input of its own: closing the master would hang up on the child.
    // A cooked-mode read is ended by typing the end-of-file character, `^D`, instead.
    override def close(): Unit = ()

  // Ends the session with the terminal: a child still running is hung up on, as closing a real
  // terminal would, and killed if it has not exited within a second; then the pump is stopped, and
  // it closes the master.
  def close(): Unit =
    if isAlive() then
      PtyProcess.signal(processId, PtyProcess.HangUp)

      if !waitFor(1L, juc.TimeUnit.SECONDS) then
        PtyProcess.signal(processId, PtyProcess.Kill)
        waitFor()

    stop() = true

    try pump.join() catch case _: InterruptedException => Thread.currentThread().nn.interrupt()

  def resize(width: Int, height: Int): Unit =
    if !master.closed then PtyProcess.resize(master.fd, width, height)

  def mode: Termios = PtyProcess.mode(master.fd)
  def mode_=(termios: Termios): Unit = PtyProcess.mode(master.fd, termios)

  def getInputStream(): ji.InputStream = input
  def getOutputStream(): ji.OutputStream = output
  def getErrorStream(): ji.InputStream = ji.InputStream.nullInputStream().nn
  def waitFor(): Int = exit.get().nn.intValue

  override def waitFor(timeout: Long, unit: juc.TimeUnit | Null): Boolean =
    try
      exit.get(timeout, unit)
      true
    catch case _: juc.TimeoutException => false

  def exitValue(): Int =
    if exit.isDone then exit.get().nn.intValue
    else throw IllegalThreadStateException("the process has not exited")

  override def isAlive(): Boolean = !exit.isDone
  override def pid(): Long = processId.toLong
  override def supportsNormalTermination(): Boolean = true
  def destroy(): Unit = if isAlive() then PtyProcess.signal(processId, PtyProcess.Terminate)

  override def destroyForcibly(): java.lang.Process =
    if isAlive() then PtyProcess.signal(processId, PtyProcess.Kill)
    this
