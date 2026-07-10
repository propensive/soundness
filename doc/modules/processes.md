## Processes

### About

Soundness runs shell commands and subprocesses through the `sh"…"` interpolator. The command is
parsed as the code compiles, so an unbalanced quote is a compile error; a value substituted into it
is escaped according to where it sits, so a filename with a space cannot break the command apart;
and the output is turned into whatever type is asked for — the lines as a list, the whole output as
text, the raw bytes as a stream, or the exit status. Commands pipe together, and a command runs
synchronously or forks into a background job.

### On subprocesses

Running a subprocess is where injection bugs breed. Build the command as one string and an
interpolated value can smuggle in extra arguments or a shell metacharacter; build it as an array to
be safe and the convenience is gone. Either way the output comes back as a raw stream to be decoded,
buffered and closed by hand, and the exit status is an integer to remember to check.

Soundness parses the command structure at compiletime, so quoting is understood before the program
runs, and each substituted value is escaped for its exact position — a value spliced where a single
argument is expected stays one argument, however it is spelled. The output is interpreted by the
type requested, so reading lines, bytes or an exit status is a matter of naming the type rather than
parsing a stream. Everything comes from the `soundness` package, with a working directory and error
strategy in scope:

```scala
import soundness.*
import workingDirectories.defaultWorkingDirectory
import logging.silentLogging
import strategies.throwUnsafely
```

### Defining a command

`sh"…"` builds a `Command`, splitting on whitespace and honouring quotes as a shell would:

```scala
sh"ls -la"   // Command(t"ls", t"-la")
```

### Running a command

`exec` runs the command and interprets its output as the type named. The whole output reads as
`Text`, the lines as a `List[Text]` or a lazy `Stream[Text]`, and the exit status as an `Exit`:

```scala
sh"echo hello".exec[Text]().trim              // t"hello"
sh"printf 'a\nb\nc\n'".exec[List[Text]]()     // List(t"a", t"b", t"c")
sh"false".exec[Exit]()                        // Exit.Fail(1)
```

### Substitution and escaping

A value substituted into a command is escaped for its position. Spliced where an argument is
expected, it becomes exactly one argument no matter its contents; a list becomes one argument each;
and a path is substituted as its text:

```scala
val file = t"a file.txt"
sh"cat $file"   // Command(t"cat", t"a file.txt") — one argument, not two

val flags = List(t"-l", t"-a")
sh"ls $flags"   // Command(t"ls", t"-l", t"-a")
```

Any type that describes how it becomes a command argument can be substituted, so a domain value is
spliced without first turning it into a string by hand.

### Piping

Commands pipe with `|`, the output of one becoming the input of the next, and the pipeline is run
and read exactly as a single command:

```scala
(sh"echo 'Hello world'" | sh"sed s/e/a/g").exec[Text]().trim   // t"Hallo world"
```

### Forking

`fork` starts the command without waiting, returning a `Job` that can be awaited for its result,
inspected, or stopped:

```scala
val job = sh"sleep 5".fork[Unit]()
job.alive        // true
job.abort()      // ask it to stop
job.await()      // block for the result
```

### An implied output type

Many commands have an obvious output type, and Soundness knows the common ones. Applying a command
directly, without naming a type, uses that default — `echo` yields text, `ls` a stream of lines:

```scala
sh"echo hi"().trim   // t"hi"
```
