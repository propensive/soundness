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
import java.lang.foreign as jlf
import java.lang.invoke as jli
import java.util.concurrent as juc

import scala.annotation.tailrec
import scala.language.experimental.pureFunctions

import rudiments.*

// The handful of POSIX calls a pseudo-terminal needs, bound through the foreign function API. The
// constants which differ between platforms are chosen for macOS or for Linux (glibc) at runtime.
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

  // How long, in milliseconds, the pump waits for output before checking whether it should stop.
  private val Patience: Int = 100
  private val Readable: Short = 1 // `POLLIN`

  private val macOs: Boolean = System.getProperty("os.name").nn.startsWith("Mac")

  private val ReadWrite: Int = 2
  private val NoControllingTerminal: Int = if macOs then 0x20000 else 0x100
  private val CloseOnExec: Int = if macOs then 0x1000000 else 0x80000
  private val SetDescriptorFlags: Int = 2
  private val DescriptorCloseOnExec: Int = 1
  private val SetWindowSize: Long = if macOs then 0x80087467L else 0x5414L
  private val Interrupted: Int = 4

  // `POSIX_SPAWN_SETSID` makes the child a session leader, so the first terminal it opens — the
  // slave, as its standard input — becomes its controlling terminal. macOS can also close every
  // descriptor the child is not explicitly given; on Linux, the JDK's own descriptors are opened
  // close-on-exec, and so are the ones opened here. Every signal is unblocked and has its default
  // disposition in the child, whatever the JVM's thread had.
  private val SpawnFlags: Short =
    val common = 0x04 | 0x08 // `POSIX_SPAWN_SETSIGDEF`, `POSIX_SPAWN_SETSIGMASK`
    (if macOs then common | 0x400 | 0x4000 else common | 0x80).toShort

  private val int16 = jlf.ValueLayout.JAVA_SHORT.nn
  private val int32 = jlf.ValueLayout.JAVA_INT.nn
  private val int64 = jlf.ValueLayout.JAVA_LONG.nn
  private val pointer = jlf.ValueLayout.ADDRESS.nn

  private val linker = jlf.Linker.nativeLinker().nn
  private val errno = jlf.Linker.Option.captureCallState("errno").nn
  private val variadic = jlf.Linker.Option.firstVariadicArg(2).nn
  val stateLayout: jlf.StructLayout = jlf.Linker.Option.captureStateLayout().nn

  private val errnoHandle: jli.VarHandle =
    stateLayout.varHandle(jlf.MemoryLayout.PathElement.groupElement("errno")).nn

  private def symbol(name: String): jlf.MemorySegment =
    linker.defaultLookup().nn.find(name).nn.get().nn

  private def bind(name: String, descriptor: jlf.FunctionDescriptor): jli.MethodHandle =
    linker.downcallHandle(symbol(name), descriptor).nn

  private def bindErrno(name: String, descriptor: jlf.FunctionDescriptor): jli.MethodHandle =
    linker.downcallHandle(symbol(name), descriptor, errno).nn

  private def ints: jlf.FunctionDescriptor = jlf.FunctionDescriptor.of(int32, int32).nn
  private def pointers: jlf.FunctionDescriptor = jlf.FunctionDescriptor.of(int32, pointer).nn
  private def twoPointers = jlf.FunctionDescriptor.of(int32, pointer, pointer).nn
  private def transfer = jlf.FunctionDescriptor.of(int64, int32, pointer, int64).nn
  private def naming = jlf.FunctionDescriptor.of(int32, int32, pointer, int64).nn
  private def waiting = jlf.FunctionDescriptor.of(int32, int32, pointer, int32).nn

  private def spawning: jlf.FunctionDescriptor =
    jlf.FunctionDescriptor.of(int32, pointer, pointer, pointer, pointer, pointer, pointer).nn

  private lazy val openpt = bindErrno("posix_openpt", ints)
  private lazy val grantpt = bindErrno("grantpt", ints)
  private lazy val unlockpt = bindErrno("unlockpt", ints)
  private lazy val closeFd = bind("close", ints)
  private lazy val ptsname = bind("ptsname_r", naming)
  private lazy val readFd = bindErrno("read", transfer)
  private lazy val writeFd = bindErrno("write", transfer)
  private lazy val waitpid = bindErrno("waitpid", waiting)
  private lazy val kill = bind("kill", jlf.FunctionDescriptor.of(int32, int32, int32).nn)

  // `nfds_t` is thirty-two bits wide on macOS, but a narrower argument is read from the low bits
  // of the register a sixty-four-bit one fills.
  private lazy val poll =
    bindErrno("poll", jlf.FunctionDescriptor.of(int32, pointer, int64, int32).nn)

  private lazy val sigfillset = bind("sigfillset", pointers)
  private lazy val actionsInit = bind("posix_spawn_file_actions_init", pointers)
  private lazy val actionsDestroy = bind("posix_spawn_file_actions_destroy", pointers)
  private lazy val addChdir = bind("posix_spawn_file_actions_addchdir_np", twoPointers)
  private lazy val attributesInit = bind("posix_spawnattr_init", pointers)
  private lazy val attributesDestroy = bind("posix_spawnattr_destroy", pointers)
  private lazy val setSignalMask = bind("posix_spawnattr_setsigmask", twoPointers)
  private lazy val setSignalDefaults = bind("posix_spawnattr_setsigdefault", twoPointers)
  private lazy val spawnPath = bind("posix_spawn", spawning)
  private lazy val spawnSearch = bind("posix_spawnp", spawning)

  private lazy val open =
    val descriptor = jlf.FunctionDescriptor.of(int32, pointer, int32).nn
    linker.downcallHandle(symbol("open"), descriptor, errno, variadic).nn

  private lazy val fcntl =
    val descriptor = jlf.FunctionDescriptor.of(int32, int32, int32, int32).nn
    linker.downcallHandle(symbol("fcntl"), descriptor, variadic).nn

  private lazy val ioctl =
    val descriptor = jlf.FunctionDescriptor.of(int32, int32, int64, pointer).nn
    linker.downcallHandle(symbol("ioctl"), descriptor, errno, variadic).nn

  // `mode_t` is sixteen bits wide on macOS, but an argument narrower than a register is passed
  // widened, and the mode given here is always zero.
  private lazy val addOpen =
    val descriptor = jlf.FunctionDescriptor.of(int32, pointer, int32, pointer, int32, int32).nn
    bind("posix_spawn_file_actions_addopen", descriptor)

  private lazy val addDup2 =
    val descriptor = jlf.FunctionDescriptor.of(int32, pointer, int32, int32).nn
    bind("posix_spawn_file_actions_adddup2", descriptor)

  private lazy val setFlags =
    bind("posix_spawnattr_setflags", jlf.FunctionDescriptor.of(int32, pointer, int16).nn)

  private def errorNumber(state: jlf.MemorySegment): Int = (errnoHandle.get(state, 0L): Int)

  // Failures are `java.io.IOException`s, as the `java.io` streams of a `java.lang.Process` must
  // report them; `Pseudoterminal.fork` turns one from spawning into an `Exec.Error`.
  //
  // A call returning `-1` and setting `errno` on failure.
  private def check(call: String, state: jlf.MemorySegment)(result: Int): Int =
    import scala.unsafeExceptions.canThrowAny

    if result < 0 then throw ji.IOException(call+" failed with errno "+errorNumber(state))
    else result

  // A call returning an error number itself, and zero on success.
  private def succeed(call: String)(result: Int): Unit =
    import scala.unsafeExceptions.canThrowAny
    if result != 0 then throw ji.IOException(call+" failed with error "+result)

  def close(fd: Int): Unit = (closeFd.invokeExact(fd): Int)
  def signal(pid: Int, signal: Int): Unit = (kill.invokeExact(pid, signal): Int)

  @tailrec
  def read(fd: Int, buffer: jlf.MemorySegment, length: Long, state: jlf.MemorySegment): Long =
    val count = (readFd.invokeExact(state, fd, buffer, length): Long)

    if count < 0 && errorNumber(state) == Interrupted then read(fd, buffer, length, state)
    else count

  @tailrec
  def write(master: Descriptor, buffer: jlf.MemorySegment, length: Long, state: jlf.MemorySegment)
  :   Unit =

    import scala.unsafeExceptions.canThrowAny
    if master.closed then throw ji.IOException("the terminal has been closed")

    if length > 0 then
      val count = (writeFd.invokeExact(state, master.fd, buffer, length): Long)

      if count >= 0 then write(master, buffer.asSlice(count).nn, length - count, state)
      else if errorNumber(state) == Interrupted then write(master, buffer, length, state)
      else throw ji.IOException("writing to the terminal failed with errno "+errorNumber(state))

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
      chunks: juc.BlockingQueue[scala.IArray[Byte]],
      stop:   Atomic[Boolean] )
  :   Thread =

    Thread.ofPlatform().nn.daemon().nn.name("guillotine-pty-pump-"+pid).nn.start: () =>
      val arena = jlf.Arena.ofConfined().nn
      val buffer = arena.allocate(BufferSize.toLong).nn
      val state = arena.allocate(stateLayout).nn
      val descriptor = arena.allocate(8L).nn
      descriptor.set(int32, 0L, master.fd)
      descriptor.set(int16, 4L, Readable)

      @tailrec
      def enqueue(bytes: scala.IArray[Byte]): Unit =
        if !stop() && !chunks.offer(bytes, Patience.toLong, juc.TimeUnit.MILLISECONDS)
        then enqueue(bytes)

      @tailrec
      def recur(): Unit = if !stop() then
        val ready = (poll.invokeExact(state, descriptor, 1L, Patience): Int)

        if ready == 0 || ready < 0 && errorNumber(state) == Interrupted then recur()
        else if ready > 0 then
          val count = read(master.fd, buffer, BufferSize.toLong, state)

          if count > 0 then
            val bytes = buffer.asSlice(0L, count).nn.toArray(jlf.ValueLayout.JAVA_BYTE)
            enqueue(bytes.asInstanceOf[scala.IArray[Byte]])
            recur()

      try
        recur()
        enqueue(scala.IArray[Byte]())
      catch case _: InterruptedException => ()
      finally
        master.close()
        arena.close()

    . nn

  // The child's exit status, decoded as the JDK decodes it: its exit code if it exited, and 128
  // plus the signal number if a signal ended it.
  def await(pid: Int): Int =
    val arena = jlf.Arena.ofConfined().nn

    try
      val state = arena.allocate(stateLayout).nn
      val status = arena.allocate(int32).nn

      @tailrec
      def recur(): Int =
        val result = (waitpid.invokeExact(state, pid, status, 0): Int)

        if result < 0 && errorNumber(state) == Interrupted then recur()
        else if result < 0 then 0x80 + Kill
        else
          val value = status.get(int32, 0L)
          if (value & 0x7f) == 0 then (value >> 8) & 0xff else 0x80 + (value & 0x7f)

      recur()
    finally arena.close()

  def resize(fd: Int, width: Int, height: Int): Unit =
    val arena = jlf.Arena.ofConfined().nn

    try
      val state = arena.allocate(stateLayout).nn
      (ioctl.invokeExact(state, fd, SetWindowSize, windowSize(arena, width, height)): Int)
    finally arena.close()

  private def windowSize(arena: jlf.Arena, width: Int, height: Int): jlf.MemorySegment =
    val size = arena.allocate(8L).nn
    size.set(int16, 0L, height.toShort)
    size.set(int16, 2L, width.toShort)
    size

  // A null-terminated array of C strings.
  private def strings(arena: jlf.Arena, values: java.util.List[String]): jlf.MemorySegment =
    val array = arena.allocate(pointer, values.size + 1L).nn

    @tailrec
    def recur(index: Int): Unit = if index < values.size then
      array.setAtIndex(pointer, index.toLong, arena.allocateFrom(values.get(index).nn).nn)
      recur(index + 1)

    recur(0)
    array

  // Starts the command `builder` describes — its arguments, environment and working directory —
  // on a fresh pseudo-terminal of the given size. A command named without a directory, which
  // `Command.builder` could not find on the environment's `PATH`, is left to `posix_spawnp`.
  def apply(builder: ProcessBuilder, width: Int, height: Int): PtyProcess =
    import scala.unsafeExceptions.canThrowAny
    val arena = jlf.Arena.ofConfined().nn

    try
      val state = arena.allocate(stateLayout).nn
      val flags = ReadWrite | NoControllingTerminal
      val master = check("posix_openpt", state)((openpt.invokeExact(state, flags): Int))

      try
        (fcntl.invokeExact(master, SetDescriptorFlags, DescriptorCloseOnExec): Int)
        check("grantpt", state)((grantpt.invokeExact(state, master): Int))
        check("unlockpt", state)((unlockpt.invokeExact(state, master): Int))
        val name = arena.allocate(1024L).nn
        succeed("ptsname_r")((ptsname.invokeExact(master, name, 1024L): Int))

        // macOS will not size the terminal until its slave has been opened once, so it is opened
        // here too, and closed again once the child has opened its own.
        val slave = check("open", state)((open.invokeExact(state, name, flags | CloseOnExec): Int))
        val size = windowSize(arena, width, height)

        try
          check("ioctl", state)((ioctl.invokeExact(state, master, SetWindowSize, size): Int))
          new PtyProcess(spawn(arena, builder, name), Descriptor(master))
        finally close(slave)

      catch case error: ji.IOException =>
        close(master)
        throw error

    finally arena.close()

  private def spawn(arena: jlf.Arena, builder: ProcessBuilder, slave: jlf.MemorySegment): Int =
    val command = builder.command().nn
    val variables = java.util.ArrayList[String]()

    builder.environment().nn.forEach: (name, value) => variables.add(name.nn+"="+value.nn)

    val actions = arena.allocate(1024L).nn
    val attributes = arena.allocate(1024L).nn
    val noSignals = arena.allocate(128L).nn
    val allSignals = arena.allocate(128L).nn
    (sigfillset.invokeExact(allSignals): Int)

    succeed("posix_spawn_file_actions_init")((actionsInit.invokeExact(actions): Int))

    try
      succeed("posix_spawnattr_init")((attributesInit.invokeExact(attributes): Int))

      try
        succeed("addopen")((addOpen.invokeExact(actions, 0, slave, ReadWrite, 0): Int))
        succeed("adddup2")((addDup2.invokeExact(actions, 0, 1): Int))
        succeed("adddup2")((addDup2.invokeExact(actions, 0, 2): Int))
        val directory = builder.directory()

        if directory != null then
          val path = arena.allocateFrom(directory.getPath.nn).nn
          succeed("addchdir_np")((addChdir.invokeExact(actions, path): Int))

        succeed("setflags")((setFlags.invokeExact(attributes, SpawnFlags): Int))
        succeed("setsigmask")((setSignalMask.invokeExact(attributes, noSignals): Int))
        succeed("setsigdefault")((setSignalDefaults.invokeExact(attributes, allSignals): Int))

        val pid = arena.allocate(int32).nn
        val path = arena.allocateFrom(command.get(0).nn).nn
        val arguments = strings(arena, command)
        val environment = strings(arena, variables)
        val spawn = if command.get(0).nn.contains("/") then spawnPath else spawnSearch

        succeed("posix_spawn"):
          (spawn.invokeExact(pid, path, actions, attributes, arguments, environment): Int)

        pid.get(int32, 0L)

      finally (attributesDestroy.invokeExact(attributes): Int)
    finally (actionsDestroy.invokeExact(actions): Int)

// A child process running on a pseudo-terminal, presented as a `java.lang.Process` so that `Job`
// drives it exactly as it drives a process on pipes. Its output and its input are both the master
// side of the terminal, and it has no separate error stream: the child's standard error is the
// terminal too.
private[guillotine] class PtyProcess private (processId: Int, master: PtyProcess.Descriptor)
extends java.lang.Process:
  private val exit: juc.CompletableFuture[Integer] = juc.CompletableFuture[Integer]().nn
  private val chunks = juc.ArrayBlockingQueue[scala.IArray[Byte]](PtyProcess.Chunks)
  private val stop: Atomic[Boolean] = Atomic(false)
  private val pump: Thread = PtyProcess.pump(processId, master, chunks, stop)

  // `waitpid` blocks, so it runs on a thread of its own rather than on whichever thread first asks
  // for the exit status; reaping the child at once also means it never lingers as a zombie.
  Thread.ofPlatform().nn.daemon().nn.name("guillotine-pty-reaper-"+processId).nn.start: () =>
    exit.complete(Integer.valueOf(PtyProcess.await(processId)))

  private lazy val input: ji.InputStream = new ji.InputStream:
    // A JDK class cannot extend `Stateful`, so its state is held in atomic cells rather than
    // `var`s; it is only ever read and written behind this stream's lock.
    private val chunk: Atomic.Ref[scala.IArray[Byte]] = Atomic.Ref(scala.IArray[Byte]())
    private val position: Atomic[Int] = Atomic(0)
    private val ended: Atomic[Boolean] = Atomic(false)

    override def read(): Int =
      val bytes = scala.Array[Byte](0)
      if read(bytes, 0, 1) < 0 then -1 else bytes(0) & 0xff

    override def read(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Int =
      synchronized:
        if !ended() && length > 0 && position() == chunk().length then
          val next = chunks.take().asInstanceOf[scala.IArray[Byte]]
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
    private val buffer = jlf.Arena.ofAuto().nn.allocate(PtyProcess.BufferSize.toLong).nn
    private val state = jlf.Arena.ofAuto().nn.allocate(PtyProcess.stateLayout).nn

    override def write(byte: Int): Unit = write(scala.Array[Byte](byte.toByte), 0, 1)

    override def write(bytes: scala.Array[Byte] | Null, offset: Int, length: Int): Unit =
      synchronized:
        if length > 0 then
          val chunk = Math.min(length, PtyProcess.BufferSize)
          jlf.MemorySegment.copy(bytes.nn, offset, buffer, jlf.ValueLayout.JAVA_BYTE, 0L, chunk)
          PtyProcess.write(master, buffer, chunk.toLong, state)
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
