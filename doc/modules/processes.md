## Processes

### About

Shell commands and subprocesses are run through the `sh"…"` [interpolator]:

- The command is parsed at [compiletime], so an unbalanced quote does not compile.
- A value which is substituted into it is escaped according to where it sits, so a filename
  with a space in it cannot break the command apart.
- The output is turned into whatever type is asked for: its lines as a list, the whole of it
  as text, the raw bytes as a stream, or the exit status.

Commands pipe together. A command is run and waited for, or is forked into a background job.

### On subprocesses

Running a subprocess is where injection bugs breed. Build the command as one string, and a
substituted value can smuggle in extra arguments, or a character which means something to the
shell. Build it as an array to be safe, and the convenience is gone. Either way, the output
comes back as a raw stream, to be decoded, buffered and closed by hand, and the exit status is
an integer which someone must remember to check.

Here the structure of the command is parsed at compiletime, so quoting is understood before
the program runs. Each substituted value is escaped for its exact position: a value placed
where a single argument is expected stays one argument, however it is spelled. The output is
interpreted by the type requested, so reading lines, bytes or an exit status is a matter of
naming the type, not of parsing a stream. A process whose result type and ways of failing are
in its signature has an [honest signature](../philosophy/honest-signatures.md), for something
which is usually driven by side effect.

Everything comes from the `soundness` package. A working directory and an error [strategy] are
in scope, and the last import lets the samples print their results:

```scala
import soundness.*

import environments.javaBaseEnvironment
import errorDiagnostics.stackTracesDiagnostics
import logging.silentLogging
import strategies.throwUnsafely
import workingDirectories.javaBaseWorkingDirectory
import stdios.javaLangSystemStdio, termcapDefinitions.xterm256Termcap
```

### Defining a command

`sh"…"` builds a `Command`. It splits the text at whitespace, and honors quotes as a shell
would:

```scala fuel
run
##
Out.println(sh"ls -la".inspect)
Out.println(sh"echo 'Hello world'".inspect)
```

A command whose quotes do not balance does not compile:

```scala fuel
compile
errors show
##
sh"echo 'Hello world"
```

### Running a command

`exec` runs the command, and interprets its output as the type named:

```scala fuel
run
##
Out.println(sh"echo hello".exec[Text]().trim)
Out.println(sh"printf 'a\nb\nc\n'".exec[List[Text]]().inspect)
Out.println(sh"false".exec[Exit]().inspect)
```

The type does more than name a conversion. It decides how the result is *interpreted*.
Commands disagree about which stream carries the important output, and about what a nonzero
status means. For some it is a failure, and for others it is one of several ordinary answers.
The choice of type settles both. It also decides *when* the call returns:

```tabular
column  Type
column  Gives
column  Returns
row  `Text`  the whole of standard output  when the process finishes
row  `List[Text]`  the lines of standard output  when the process finishes
row  `Chain[Text]`  the lines of standard output, lazily  at once, reading each line as it arrives
row  `Stderr`  the whole of standard error  when the process finishes
row  `Data`  the raw bytes of standard output  when the process finishes
row  `Exit`  the exit status  when the process finishes
row  `Unit`  nothing: the output is discarded  when the process finishes
row  a path type  the trimmed output, read as a path  when the process finishes
```

A `Computable` instance defines each of these. A type which is not covered gains one by saying
how to interpret a process which has finished, or which is running. That is often done by
mapping an existing instance, since `Computable` is a functor.

### An implied output type

Many commands have an obvious type of output, and the common ones are known. Applying a
command directly, without naming a type, uses that default. `echo` gives text, and `ls` gives
a stream of lines:

```scala fuel
run
##
Out.println(sh"echo hi"().trim)
```

### Substitution and escaping

A value which is substituted into a command is escaped for its position. Where an argument is
expected:

```tabular
column  A substituted
column  Becomes
row  value  exactly one argument, no matter its contents
row  list  one argument for each element
row  path  one argument, its text
row  command  one argument, escaped, as described below
```

A filename which contains a space stays one argument, and a list becomes several:

```scala
val file = t"a file.txt"
val flags = List(t"-l", t"-a")
```

```scala fuel
run
##
Out.println(sh"cat $file".inspect)
Out.println(sh"ls $flags".inspect)
```

Any type which describes how it becomes an argument of a command can be substituted, so a
value from a program's own domain, such as a [path](paths.md), is placed in a command without
first being turned into a string by hand. [Interpolation](interpolation.md#substitutions)
describes the mechanism.

### Piping

Commands are piped with `|`. The output of one becomes the input of the next, and the pipeline
is run, and read, exactly as a single command is:

```scala fuel
run
##
Out.println((sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Text]().trim)
```

Pipelines flatten, and do not nest. A pipeline of three stages is one pipeline of three
commands, however it was assembled, and a pipeline which is piped into a command extends it:

```scala fuel
run
##
Out.println((sh"echo a" | sh"cat" | sh"wc").commands.length.show)
```

### Commands inside commands

A command which is substituted into another is escaped as a single argument. That is what
invoking a shell requires, and what command strings built by hand get wrong:

```scala
val inner = sh"echo 'Hello world'"
```

```scala fuel
run
##
Out.println(sh"sh -c '$inner'".exec[Text]().trim)
Out.println(Command(t"echo", t"a b").escape)
```

An argument which contains a space, a quote or a character special to the shell is therefore
carried through untouched. There is no point at which it becomes a string which a shell might
split again.

### Failure

A command which cannot be run at all, because its binary is missing, or is a directory, or is
not executable, raises an `Exec.Error`. The error carries the command which failed, so it
names what was attempted, instead of reporting an error number from the operating system:

```scala fuel
run
##
Out.println(capture[Exec.Error](sh"no-such-binary".exec[Text]()).command.arguments.head)
```

A command which runs, and *fails*, is a different matter. Its exit status is part of its
result, and what a nonzero status means is the caller's decision. Asking for an `Exit` gives
the status itself. Asking for a `Text`, or a stream, gives the output, with the status
available beside it.

### Forking

`exec` blocks until the command finishes. `fork` starts the command without waiting, and
returns a `Job`, which can be awaited for its result, inspected, or stopped:

```scala fuel
run
##
val job = sh"sleep 5".fork[Unit]()

Out.println(job.alive.inspect)
job.kill()
```

```tabular
column  Member of a `Job`
column  Does
row  `alive`  says whether the process is still running
row  `abort()`  asks the process to stop
row  `kill()`  insists that it stop
row  `await()`  blocks for the result
row  `pid`  gives the process's `Pid`
row  `process`  gives the operating system's process itself
```

`pid` and `process` are for the occasions where a signal must be sent, or a tree of processes
inspected. A `Pid` is a distinct type, not a number, so it cannot be confused with an exit
code or a file descriptor, and it is written and parsed in a form of its own.

### Detaching

A process which should outlive the one that starts it, such as a server or a
[daemon](daemons.md), is *detached*. `detach` starts it reading from the null device, and
writing nowhere, or appending to a log whose parent directories are made. It gives the
process's pid, and holds nothing else of it.

A forked child inherits pipes which nobody drains, and a server which writes to them
eventually blocks. A detached process cannot. Its environment is the
[`Environment`](environment.md#overriding-for-a-block) in scope, so `emptyEnvironment` with
`variables(…)` gives it exactly the variables named:

```scala fuel
typecheck
##
import pathInterfaces.pathOnLinux
import filesystemBackends.javaBaseFilesystem

val directory: Path on Linux = (% / "var" / "log").on[Linux]
val pid: Pid = sh"server --port 8080".detach(log = directory / "server.log")
```

So a command is started in one of three ways or, through `pty`, on a terminal:

```tabular
column  Method
column  Waits
column  Answers
row  `exec`  until the command finishes  its result, as the type named
row  `fork`  only until it has started  a `Job`, to await, inspect or stop
row  `detach`  only until it has started  its `Pid`, holding nothing else of it
row  `pty`  not at all  a description of the command on a pseudo-terminal, to `exec` or open a `session` on
```

### On a terminal

A command's standard streams are normally pipes, and many programs behave differently when
they notice. A shell prints no prompt, `stty` fails, and a line editor never starts. `pty`
runs the command on a fresh pseudo-terminal of the given size instead. The command then finds
a terminal on its input, output and error, as it would if a person had typed it. Here `stty`
reports the size which was asked for, as 30 rows and 100 columns:

```scala fuel
typecheck
##
val size: Text = sh"stty size".pty(100, 30).exec[Text]().trim
```

For an interactive program, a `session` lends the running command to a block, as a `Job`
which can also be resized, and ends the command when the block does. If the program is still
running, it is hung up on, as it would be by closing a real terminal.

The session's output is everything which the program writes to the terminal, standard error
included, with its escape sequences, which is what a
[terminal emulator](terminal-emulator.md) would receive. Anything streamable can be typed at
it, with `writeTo`. A terminal has no end of input of its own, so a program which reads until
the end is sent the end-of-file character, `^D`, instead of having its input closed:

```scala fuel
typecheck
##
sh"sh -c 'read line; stty size'".pty().session: terminal ?=>
  terminal.resize(132, 50)
  t"\r".writeTo(terminal)
  terminal.text().trim
```

Resizing sends the program the signal `SIGWINCH`, writing `\r` presses Return, and the text
which comes back is `50 132`.

The value which `pty` returns is not itself a terminal. It describes how to run the command,
and each `session` or `exec` allocates a fresh terminal, and releases it again.

A terminal's *line discipline* says whether it echoes what is typed, gathers input a line at a
time, turns `^C` into a signal, and so on. It is a `Termios`, named after the POSIX structure,
with a switch for each of the flags which a program most often changes. `Termios.raw` turns
them all off, as a full-screen program does before it draws, and `Termios.cooked` turns them
all on. A `Termios` given to `pty` is set before the command starts, and the terminal of a
running command can be read and changed through its `Job`:

```scala fuel
typecheck
##
sh"head -c 2".pty(termios = Termios.raw).session: terminal ?=>
  t"hi".writeTo(terminal)
  val canonical: Boolean = terminal.mode.canonical
  terminal.mode = Termios.cooked
  terminal.text()
```

In raw mode the two characters are delivered at once, without a newline, and are not echoed,
so the text which comes back is just `hi`.

Pseudo-terminals are allocated through the JDK's foreign function API, on Linux and macOS. A
program which uses them should be run with `--enable-native-access=ALL-UNNAMED`, to keep the
JDK from warning about it.

### Related

- [Environment](environment.md) decides which variables a subprocess inherits.
- [Streams](streams.md) reads a command's output a part at a time.
- [Git](git.md), [Docker](docker.md) and [SSH](ssh.md) drive particular programs with typed
  APIs of their own.
- [Command-line interfaces](cli.md) covers the other side: a program which is itself run from
  a shell.
- [ACP](acp.md), [debugging](debugging.md), [logging](logging.md), [LSP](lsp.md), [staging](staging.md) and [terminal](terminal.md) launch and control processes.
