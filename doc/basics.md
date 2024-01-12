### Commands

Shell commands are created using the `sh""` interpolator, which will interpret (at compiletime) a
command and its arguments, correctly interpreting single- and double-quoted arguments and escaped
characters. Unclosed quotes will result in a compile error.

Substitutions of values of a variety of different types may be made into an `sh` interpolator, and
may be read as either "single-argument" (such as `String` or `Int`) or "multi-argument" (such as
`List[Int]` or `Set[String]`).

Multi-argument substitutions will be interpreted as multiple arguments to the shell command unless
they are enclosed within quotes, in which case they will be interpreted as a space-separated
string.

Substitutions should normally be surrounded by spaces, otherwise they will be prepended or appended
to adjacent arguments.

#### Piping

Two commands may be combined using the pipe operator (`|`), for example,
```scala
sh"cat /home/work/file" | sh"grep $query" | sh"wc -l"
```
which is equivalent to the single shell command, `cat /home/work/file | grep $query | wc -l`, with
the appropriate substitution of `query` being made.

While this expression will seem very familiar from a shell-command perspective, it may also be
written in function application style as,
```scala
sh"wc -l"(sh"grep $query"(sh"cat /home/work/file"))
```
and the two versions are equivalent.

#### Substitutions

Substitutions of a variety of different types may be made into an interpolated `sh` command. Any
type for which a `gossamer.Show` typeclass exists will be inserted as a single parameter, and
any sequence of one of these types will be inserted as multiple arguments. A `Command` instance
may also be substituted into another, for example,
```scala
val echo = sh"echo Hello World"
sh"sh -c '$echo'"
```
where the quotes are required aronud `'$echo'` so that the command is passed to `sh -c` as a
single argument, rather than multiple arguments (of which only the first would be used).

#### Environment

Execution requires an `Env` instance specifying a map of environment variables and a working
directory as a `String`, and should be specified as a contextual value, for example,
```scala
given Env(Map("PATH" -> "/usr/bin:/usr/sbin"), "/home/work")
```
however it is common to use the `enclosing` environment. That is, to pass the environment in which
the JVM was started to its subprocess, ensuring that processes started by Guillotine behave as they
would if started directly from the shell. There may, however, be security implications when doing
this, so it must be explicitly enabled with:
```scala
given Env = envs.enclosing
```

### Execution

Two methods are provided for starting execution of a process: `fork` and `exec`, both taking a type
parameter which determines the type of the return value, and may also affect how execution is
handled.

```scala
val result: String = sh"echo Hello World".exec[String]()
```

The `exec` method will return a value synchronously, when that value is ready. This may happen only
when the process completes execution, if the entire output is caputured, for example if the
return-type is `String`, or may happen earlier if a streaming return type, such as
`LazyList[String]`, is specified.

The `fork` method always starts the process asynchronously, and returns an instance of `Process[T]`,
where `T` is the specified return type.

```scala
val process: Process[String] = sh"locate lostfile".fork[String]()
```

`Process` implements a few useful methods for working with a running process:

 - `await()` which waits until the process completes, and returns its result of type `T`
 - `abort()` which stopes execution, by delegating to Java's `Process#destroy`
 - `kill()` which stopes execution, by delegating to Java's `Process#destroyForcibly`
 - `pid` which returns a `Pid` instance representing the OS-dependent process ID
 - `stdout` and `stderr` methods for directly accessing the process's output streams; these methods
   both take an integer parameter limiting the number of bytes that may be read from the stream,
   defaulting to `10MB`
 - `stdin(in)` which accepts a stream of bytes (`LazyList[IArray[Byte]]`) as standard input to the
   process

The synchronous `exec[T]()` method is always equivalent to `fork[T]().await()`.

### Result interpretation

Different shell processes may behave differently in how their results should be interpreted. Those
differences include the interpretation of the exit status—where different nonzero codes may be
interpreted as different types of failure—and which stream contains the important output, `STDOUT`
or `STDERR`.

How these differences are interpreted is determined by the choice of return type: nonzero return
types may be presented as thrown exceptions, or interpreted as a different sort of "success".
Furthermore, the return type will determine whether the result may be return before the shell
process terminates, or whether (in the case of a streaming response) it may be returned earlier.

The `Executor[T]` typeclass provides support for producing different return types. Executors for
the following types are provided:
- `String`, which interprets the response using the system encoding and returns a value after the
  process terminates
- `LazyList[String]`, which provides a stream of lines of text (without the newline character)
- `LazyList[IArray[Byte]]`, which returns a stream of byte arrays
- `Unit`, to be used when the result is not important
- `ExitStatus`, an enumeration of `Ok` or `Fail(status)` where `status` is a nonzero positive
  integer

Custom executors may be provided by implementing the `Executor` trait with the single abstract
method,
```scala
def interpret(process: java.lang.Process): T
```
or by mapping across an existing `Executor`, for example,
```scala
given Executor[Int] = summon[Executor[String]].map(_.toInt)
```
since all `Executor`s are functors.



