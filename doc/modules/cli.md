## Command-Line Applications

### About

A command-line application is an entry point that hands the program its arguments, an
interpretation of those arguments as subcommands, flags and operands, and tab
completions for every shell. The unusual part is that the completions are derived from
the same code that runs the command, so they cannot drift away from what the program
actually does.

### On the command line

The arguments a shell passes are a flat list of text, and every command-line program
must recover the structure — which words are subcommands, which are flags, which values
belong to which — before any real work begins. Completion scripts then re-implement that
same interpretation, by hand, per shell, and drift from the program they describe: the
new flag exists, but the completions have never heard of it.

Soundness turns the argument list into typed values — a subcommand matched by name, a
flag read as an `Int`, an operand decoded to a path — while recording enough about each
argument to complete it. Pressing `tab` runs the program's own argument-handling code in
a mode that produces suggestions instead of effects, so a flag that the program checks
for becomes a flag the program can suggest, and the completions agree with the program
by construction.

The sections below start with a minimal entry point and build up through subcommands and
flags to completions. Everything comes from the `soundness` package, together with a
choice of argument interpreter and a few contextual values the entry point needs:

```scala
import soundness.*
import executives.directExecutive
import interpreters.posixInterpreter
import backstops.genericErrorMessageBackstop
import systems.javaSystem
```

### An entry point

The `application` method wraps a `@main` method, giving its body the program's context
and expecting an `Exit` status in return:

```scala
@main
def mytool(args: IArray[Text]): Unit = application(args):
  Out.println(t"Hello, world!")
  Exit.Ok
```

Packaged as an executable JAR whose `Main-Class` is `mytool`, this runs from the shell.
`Exit.Ok` reports success; `Exit.Fail(1)` — or any positive code — reports failure, and
the status must be returned, not left implicit.

An application can equally be written as a class extending `Application`, which supplies
the `main` method and asks only for an `invoke`:

```scala
object MyTool extends Application:
  def invoke(using Cli): Exit =
    Out.println(t"Hello, world!")
    Exit.Ok
```

### Arguments

Within the body, `arguments` is the list of arguments the program was given, each an
`Argument`. Applying an argument yields its `Text`:

```scala
application(args):
  arguments.each: argument =>
    Out.println(argument())
  Exit.Ok
```

An `Argument` is more than its text: it remembers its position on the command line,
which is what later lets a completion know which argument it is completing.

### Subcommands

A `Subcommand` names a mode the program can run in, with a description for help and
completions. It is also an extractor, so the first argument is matched against it in an
ordinary pattern:

```scala
val Build = Subcommand(t"build", e"compile the project")
val Test = Subcommand(t"test", e"run the tests")

application(args):
  arguments match
    case Build() :: _ => Out.println(t"building…");  Exit.Ok
    case Test() :: _  => Out.println(t"testing…");   Exit.Ok
    case _            => Out.println(t"unknown command"); Exit.Fail(1)
```

The description is written with the `e"…"` interpolator, which carries ANSI styling for
shells that show it; a plain `Text` description works too. Subcommands nest: matching a
subcommand and then matching again on the remaining arguments gives a tree of commands,
each with its own flags.

### Flags

A `Flag` is an argument beginning with `-` or `--`. Declaring one and applying it reads
its value from the command line, returning `Optional` because the flag may be absent. A
flag's type is whatever the value should decode to — any type that can be read from text
works without further ceremony:

```scala
case Build() :: _ =>
  val jobs: Optional[Int] = Flag[Int](t"jobs", aliases = List('j'))()
  Out.println(t"building with ${jobs.or(1)} jobs")
  Exit.Ok
```

`aliases` gives short forms, so `--jobs 4`, `-j 4`, and `--jobs=4` are read alike.
`repeatable = true` allows a flag to appear more than once. A flag that takes no value —
an on-or-off option — is a `Switch`:

```scala
val verbose = Switch(t"verbose", aliases = List('v'), description = t"print detail")
```

### Exit status and errors

The body returns an `Exit`: `Exit.Ok`, or `Exit.Fail(code)` for a failure. An error that
escapes the body is caught by the *backstop* in scope, which turns it into an exit
status rather than a stack trace — `genericErrorMessageBackstop` prints a short message
and exits non-zero, while `stackTraceBackstop` prints the full trace, and
`silentBackstop` prints nothing. Choosing a backstop is a matter of which one is
imported.

### Tab completions

A completion script in a shell usually re-implements the command's own logic, by hand,
and falls out of step with it. Soundness takes the program itself as the only source of
truth: the same code that interprets the arguments produces the completions, so they
agree by construction.

Turning completions on replaces the default executive with the completions executive:

```scala
import executives.completions
```

Under this executive the body returns an `Execution`, and the only way to build one is an
`execute` block. The code *before* `execute` — the prelude — runs both when the command
is invoked and when completions are requested; the `execute` block runs *only* on a real
invocation. So the prelude does the argument handling, and the side effects live inside
`execute`:

```scala
@main
def mytool(args: IArray[Text]): Unit = application(args):
  arguments match
    case Build() :: _ =>
      val jobs = Flag[Int](t"jobs", aliases = List('j'))()
      execute:
        Out.println(t"building with ${jobs.or(1)} jobs")
        Exit.Ok

    case _ =>
      execute(Exit.Fail(1))
```

Because matching `Build` and declaring the `--jobs` flag happen in the prelude, they run
when the user presses `tab`: after `mytool ` the shell offers `build`, and after
`mytool build ` it offers `--jobs`. The structure guarantees this — an `Execution` cannot
be produced without an `execute` block, and the prelude that registers a completion is
exactly the prelude that reads the value, so completion and execution can never
disagree. The completions executive also withholds standard output from the prelude,
since there is nowhere to send it while completing; `Out.println` is available only
inside `execute`.

### Completing values

A flag completes its own values when its type can describe them. A type is told how to
suggest values with a `Discoverable` instance, and how to read a chosen value with an
`Interpretable` instance:

```scala
case class Mode(name: Text)

given Mode is Discoverable = _ => List(t"debug", t"release").map(Suggestion(_))
given Mode is Interpretable =
  case argument :: Nil => Mode(argument())
  case _               => Mode(t"debug")

val mode = Flag[Mode](t"mode", description = t"build mode")()
```

With these in scope, `mytool build --mode ` offers `debug` and `release`. A `Suggestion`
may carry a description, shown by shells that support it, and may be marked `incomplete`
to complete a value one segment at a time, as for a file path.

### Installing completions

A completed application ships with a generic, identical completion script for each shell
— Bash, Zsh, Fish, and PowerShell — that does nothing but forward the current command
line to the application in completions mode. The built-in administration command writes
these scripts into place:

```sh
mytool '{admin}' install
```

After installation each shell completes the command through the application itself, so
new subcommands and flags need no change to any script.

### Fast startup

A command invoked for completion must start quickly, or the shell feels sluggish.
Running the application as a daemon removes the JVM's startup cost from every
invocation: the program stays resident and each command, including each completion
request, is dispatched to the running process. The same body that `application` runs is
run by `daemon`, so an application gains fast startup by changing how it is launched, not
how it is written.
