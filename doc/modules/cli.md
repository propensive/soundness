## Command-Line Applications

### About

A command-line application has three parts: an entry point, which hands the program its
arguments, an interpretation of those arguments as subcommands, flags and operands, and tab
completions for every shell.

The unusual part is that the completions are derived from the same code which runs the
command, so they cannot drift away from what the program actually does. Neither can the help
text, the manpage, or the list of exit statuses. All of them are read from the program, not
written beside it.

### On the command line

The arguments which a shell passes are a flat list of text. Every command-line program must
recover their structure before any real work begins: which words are subcommands, which are
flags, and which values belong to which. Completion scripts then implement that same
interpretation again, by hand, for each shell, and they drift from the program they
describe. The new flag exists, but the completions have never heard of it.

Here the list of arguments is turned into typed values: a subcommand matched by name, a flag
read as an `Int`, an operand [decoded](decoding.md) to a path. Enough is recorded about each
argument to complete it. Pressing `tab` runs the program's own code for handling arguments,
in a mode which produces suggestions instead of effects. A flag which the program checks for
therefore becomes a flag which the program can suggest, and the completions agree with the
program by construction.

Every parameter, and how it is parsed, is declared where the command is defined, which is
the [declarative context](../philosophy/declarative-context.md) style, applied to the
command line.

The sections below start with a minimal entry point, and build up through subcommands and
flags to completions, help and manpages. Everything comes from the `soundness` package,
together with a few choices which the entry point needs:

```scala
import soundness.*

import backstops.genericErrorMessageBackstop
import executives.directExecutive
import interpreters.posixInterpreter
import systems.javaBaseSystem
import strategies.throwUnsafely
```

```tabular
column  Import
column  Decides
row  `interpreters.posixInterpreter`  how the flat list of arguments is read as flags and operands
row  `executives.directExecutive`  what the body of the entry point returns; [Tab completions](#tab-completions) uses another
row  `backstops.genericErrorMessageBackstop`  what is printed when an error escapes the program
row  `systems.javaBaseSystem`  where [system properties](environment.md#system-properties) come from
row  `strategies.throwUnsafely`  that an error is thrown as an exception
```

The samples on this page define whole programs, so they are compiled but not run.

### An entry point

The `application` method wraps the entry point of a program. It gives its body the program's
context, and expects an `Exit` status in return. It takes the arguments as a list of text.

The same program can equally be an object which extends `Application`. That abstract class
supplies the `main` method, which converts the JVM's arguments, and asks only for an
`invoke`:

```scala fuel
alternative Function
  code
      def mytool(args: List[Text]): Unit = application(args):
        Out.println(t"Hello, world!")
        Exit.Ok
alternative `Application`
  code
      object MyTool extends Application:
        def invoke(using Cli): Exit =
          Out.println(t"Hello, world!")
          Exit.Ok
##
```

Packaged as an executable JAR whose `Main-Class` is `mytool` or `MyTool`, this runs from a
shell. `Exit.Ok` reports success. `Exit.Fail(1)`, or any positive code, reports failure. The
status must be returned, and is never left implicit. [Packaging](packaging.md) builds the
JAR.

### Arguments

Within the body, `arguments` is the list of arguments which the program was given. Each is
an `Argument`, and applying one gives its `Text`:

```scala
def echo(args: List[Text]): Unit = application(args):
  arguments.each: argument =>
    Out.println(argument())
  Exit.Ok
```

An `Argument` is more than its text. It remembers its position on the command line, which is
what later lets a completion know which argument it is completing. Positions are numbered
from `0`, for the first argument after the command, and not from `1`, as in most scripting
languages.

Two words are often used interchangeably, and are worth separating. *Arguments* are the flat
sequence of textual values which the shell hands over. *Parameters* are what they mean, once
they have been interpreted. The arguments of `grep -rA4 pattern` are `-rA4` and `pattern`.
Its parameters are "search recursively", "show four lines of trailing context" and "search
for `pattern`".

The interpreter in scope is what turns the first into the second, which is why it is chosen
by import, and not fixed. The package `interpreters` has three:

```tabular
column  Interpreter
column  Reads
row  `posixInterpreter`  `--long` and `-s` flags
row  `posixClusteringInterpreter`  `--long` and `-s` flags, and `-rA4` as `-r -A 4`, with a cluster's last letter taking the value
row  `simpleInterpreter`  every argument as an operand, and no flags
```

### Subcommands

A `Subcommand` names a mode which the program can run in, with a description for help and
completions. It is also an extractor, so the first argument is matched against it in an
ordinary pattern:

```scala
val Build = Subcommand(t"build", e"compile the project")
val Test = Subcommand(t"test", e"run the tests")

def tool1(args: List[Text]): Unit = application(args):
  arguments match
    case Build() :: _ => Out.println(t"building");  Exit.Ok
    case Test() :: _  => Out.println(t"testing");   Exit.Ok
    case _            => Out.println(t"unknown command"); Exit.Fail(1)
```

The description is written with the `e"…"` interpolator, which carries
[styling](terminal.md#styled-text) for shells which show it. A plain `Text` description
works too.

Subcommands nest. Matching a subcommand, and then matching again on the remaining arguments,
gives a tree of commands, each with its own flags. A subcommand may be `hidden = true`, which
keeps it out of help and completions. Subcommands are gathered under headings in help with a
`CommandGroup`:

```scala
val admin = CommandGroup(t"Admin commands", t"Commands for user administration.")
val UserAdd = Subcommand(t"useradd", e"add a user account", group = admin)
```

### Flags

A `Flag` is an argument which begins with `-` or `--`. Declaring one, and applying it, reads
its value from the command line. The result is [`Optional`](optional-values.md), because the
flag may be absent. A flag's type is whatever its value should be decoded to, and any type
which can be read from text works without further ceremony:

```scala
def tool2(args: List[Text]): Unit = application(args):
  arguments match
    case Build() :: _ =>
      val jobs: Optional[Int] = Flag[Int](t"jobs", aliases = List('j'))()
      Out.println(t"building with ${jobs.or(1)} jobs")
      Exit.Ok

    case _ =>
      Exit.Fail(1)
```

A flag is declared with these parameters:

```tabular
column  Parameter
column  Default
column  Meaning
row  `name`  required  the long form, as `--name`, or a `Char` for a short one
row  `aliases`  `Nil`  other forms, such as a short `-j`
row  `description`  `Unset`  the text shown in help and beside completions
row  `repeatable`  `false`  whether the flag may appear more than once
row  `secret`  `false`  whether to leave the flag out of completions and help
```

`aliases` gives short forms, so `--jobs 4`, `-j 4` and `--jobs=4` are read alike.

The occurrences of a repeatable flag are read together, through a `List` type. `--tag a
--tag b` is read as `List(t"a", t"b")`, with each operand interpreted as the type of the
elements. An `Optional` type reads an operand which may be malformed as `Unset`, instead of
raising an error. Counting a `List` walks it, so `size` asks for the
[`linearSize` acknowledgment](collections.md#positions-and-access):

```scala
import dysasymptotics.linearSize

def tool3(args: List[Text]): Unit = application(args):
  val tags: Optional[List[Text]] = Flag[List[Text]](t"tag", repeatable = true)()
  val port: Optional[Int] = Flag[Optional[Int]](t"port")()
  Out.println(t"${tags.or(Nil).size} tags on port ${port.or(80)}")
  Exit.Ok
```

A flag which takes no value, and is simply on or off, is a `Switch`:

```scala
val verbose = Switch(t"verbose", aliases = List('v'), description = t"print detail")
```

### Settings

Many options should be settable not only on the command line, but also by an
[environment variable](environment.md), a system property or a configuration file, with a
fixed order of precedence. A `Setting` declares such an option once, by a canonical name in
camel case, and each source derives its own name for it by convention. Declaring an
application's identity, as a `Configurator.Prefix`, activates the standard cascade:

```scala
given prefix: Configurator.Prefix = Configurator.Prefix(t"myapp")

val LogLevel = Setting[Text](t"logLevel", description = t"logging verbosity")
val Home = Setting[Text](t"home", variable = t"MYAPP_HOME")
```

Reading `LogLevel()` inside the body consults its sources in order, and the first which has
a value wins:

```tabular
column  Source
column  Named, for `logLevel`
row  the command-line flag  `--log-level`
row  the environment variable given as `variable`, if any  verbatim, as `MYAPP_HOME` for `Home`
row  the system property  `myapp.log.level`
row  the environment variable  `MYAPP_LOG_LEVEL`
```

The result is `Optional`, so a default is given in the usual way, as
`LogLevel().or(t"info")`. `variable` is for a setting whose environment variable does not
follow the convention. Reading a setting registers its flag for tab completion, exactly as
reading the flag directly would.

Each source is a `Configurator`, which has a single abstract method, from a setting's
canonical name to an optional textual value. The cascade is just composition with `++`, in
which the left side takes priority. No format of configuration file is prescribed, but any
source can join the cascade as a function of one line. Defining a `Configurator` given
replaces the standard cascade, though the command-line flag always takes priority:

```scala
def fromFile(name: Text): Optional[Text] = Unset

given configurator: (Environment, System) => Configurator =
  Configurator.properties ++ Configurator.environment ++ { name => fromFile(name) }
```

### Exit status and errors

The body returns an `Exit`: `Exit.Ok`, or `Exit.Fail(code)` for a failure. An
[error](errors.md) which escapes the body is caught by the *backstop* in scope, which turns
it into an exit status instead of a stack trace. A backstop is chosen by importing one from
the package `backstops`:

```tabular
column  Backstop
column  Prints
row  `genericErrorMessageBackstop`  a short, fixed message: `An unexpected error occurred.`
row  `exceptionMessageBackstop`  the exception's own message
row  `stackTraceBackstop`  the full stack trace
row  `silentBackstop`  nothing
```

Each exits with status 1 for an exception, and with 2 for any more serious error.

An exit code on its own says little to the user. A failure which the program anticipates is
better declared as a `Status`, which is an object with both the code and a description:

```scala
object CannotConnect extends Status(1, t"the server could not be reached")
object BadConfig extends Status(2, t"the configuration file was invalid")
```

A `Status` is returned wherever an `Exit` is. Under the completions executive, below, the
statuses which a command can return are discovered from its code, and appear in its help and
its manpage without being listed anywhere else.

### Tab completions

Tab completion is one of the most useful things which a command-line program offers. It lets
a user discover features, see which flags and values are available, and avoid mistakes in
typing altogether.

It is also, conventionally, maintained separately from the program itself. There is a
different script for each shell, often written by different people. Each approximates the
command's behavior by listing its subcommands and flags, and sometimes goes further, to keep
mutually exclusive flags consistent with each other. The shells do not even agree on what a
completion is: Bash offers the text of the completion alone, while Zsh and Fish can show a
description beside it. The work is admirable, and it is impossible to keep correct.

Here the program itself is the only source of truth. The same code which interprets the
arguments produces the completions, so they agree by construction. The scripts for each
shell shrink to nothing of consequence: they capture the incomplete command line, the
position of the cursor and the name of the shell, and hand them to the application in
completions mode.

Turning completions on replaces the default executive with the completions executive. It is
imported where the entry point is defined, so that it, and not the direct executive, is the
one in scope for that program:

```scala
import executives.completionsExecutive
```

Under this executive, the body returns an `Execution`, and the only way to build one is an
`execute` block. The code *before* `execute` is the *prelude*. It runs both when the command
is invoked and when completions are requested. The `execute` block runs *only* on a real
invocation. So the prelude handles the arguments, and the side effects live inside
`execute`:

```scala fuel
frame
  label The subcommands, under the direct executive
  code
      def tool1(args: List[Text]): Unit =
        import executives.directExecutive
        application(args):
          arguments match
            case Build() :: _ => Out.println(t"building");  Exit.Ok
            case Test() :: _  => Out.println(t"testing");   Exit.Ok
            case _            => Out.println(t"unknown command"); Exit.Fail(1)
frame
  label With a flag read in the prelude
  code
      def tool2(args: List[Text]): Unit =
        import executives.directExecutive
        application(args):
          arguments match
            case Build() :: _ =>
              val jobs: Optional[Int] = Flag[Int](t"jobs", aliases = List('j'))()
              Out.println(t"building with ${jobs.or(1)} jobs")
              Exit.Ok

            case _ =>
              Exit.Fail(1)
frame
  label Under the completions executive, the effects inside `execute`
  code
      def mytool2(args: List[Text]): Unit =
        import executives.completionsExecutive
        application(args):
          arguments match
            case Build() :: _ =>
              val jobs = Flag[Int](t"jobs", aliases = List('j'))()
              execute:
                Out.println(t"building with ${jobs().or(1)} jobs")
                Exit.Ok

            case _ =>
              execute(Exit.Fail(1))
##
```

Matching `Build`, and declaring the `--jobs` flag, happen in the prelude, so they run when
the user presses `tab`. After `mytool `, the shell offers `build`, and after `mytool build `,
it offers `--jobs`.

The structure guarantees this. An `Execution` cannot be produced without an `execute` block,
and the prelude which registers a completion is exactly the prelude which reads the value,
so completion and execution can never disagree. The completions executive also withholds
standard output from the prelude, since there is nowhere to send it while completing.
`Out.println` is available only inside `execute`.

#### Conditional completions

A flag which is read in the prelude gives a `Prospective` handle, not the value itself. Its
`present` and `absent` may be consulted at once, to decide what else to offer, but applying
it to get the value is possible only inside `execute`.

The consequences run deeper than "a flag which is read is a flag which is offered". The
reading may be conditional, and then so is the offering, on *precisely the same condition*.
Consider a command with a `-v` flag, which turns on output, and a `--verbosity` flag, which
is meaningful only beside it:

```scala
enum Verbosity:
  case Quiet, Normal, Loud

def tool3(args: List[Text]): Unit =
  import executives.completionsExecutive
  import enumIdentification.kebabCaseIdentifiable
  application(args):
    val verbose = Switch(t"verbose", aliases = List('v'))()
    val verbosity = if verbose.present then Flag[Verbosity](t"verbosity")() else Unset

    execute:
      Out.println(t"verbosity: ${verbosity.let(_()).or(Verbosity.Normal)}")
      Exit.Ok
```

A flag whose type is an [enumeration] reads its value by name, in the style which the
imported `enumIdentification` given chooses: here, `--verbosity loud`.

Pressing `tab` after a bare `-` runs this prelude. `-v` is checked for, and so becomes a
candidate. If it is not yet on the command line, it is offered. If it *is* already there,
the branch which checks for `--verbosity` runs too, so `--verbosity` becomes a candidate as
well, while `-v`, which makes sense only once, does not. No table of dependencies between
flags was written anywhere. The conditional structure of the code is the conditional
structure of the completions, however complex it becomes.

One caveat applies to `inline` code. `Flag#apply()` is a `transparent inline` method. Inside
another `inline def`, it is not expanded when that body is typed, so its result is not
narrowed to a `Prospective`, and `.present` on it would answer `true` for every flag. An
inline method should read a flag with `Flag#present`, anywhere, or with `Flag#value`, inside
`execute`. Both are plain methods, which read the flag directly.

### Required and validated flags

A flag which must be present is read with `require()` instead of `()`, and one whose value
must also decode correctly is read with `validate()`. In the prelude, each gives a
`Requisite` handle, whose value is unconditionally available inside `execute`, because a
requisite which is missing or malformed stops the block from running at all.

Every such flag is checked before the block starts. The user therefore learns of all of them
at once, with exit status 2, by the convention for errors of usage, and not of one each time
the program is run:

```scala
def tool4(args: List[Text]): Unit =
  import executives.completionsExecutive
  application(args):
    val home = Flag[Text](t"home", description = t"the home directory").require()
    val jobs = Flag[Int](t"jobs", description = t"parallel jobs").validate()

    execute:
      Out.println(t"building ${home()} with ${jobs()} jobs")
      Exit.Ok
```

Inside `execute`, the same calls give the value directly, and raise a `MissingFlagError` or
an `InvalidFlagError` immediately, for the backstop to report. A required flag is also marked
as required in the help text.

Each way of reading a flag gives something different in the prelude and inside `execute`:

```tabular
column  Read
column  In the prelude
column  Inside `execute`
row  `flag()`  a `Prospective`, whose `present` and `absent` answer at once  an `Optional` value
row  `flag.require()`  a `Requisite`, checked before `execute` runs  the value, or a `MissingFlagError`
row  `flag.validate()`  a `Requisite`, checked before `execute` runs  the value, or a `MissingFlagError` or `InvalidFlagError`
row  `flag.present`  a `Boolean`  a `Boolean`
row  `flag.value`  not available  an `Optional` value
```

### The executive, and effectful methods

The *executive* is what determines the return type of the body of an entry point, and the
[contextual values][contextual value] which are available inside it:

```tabular
column  Executive
column  Body returns
column  `Stdio` and `Effectful`
row  `directExecutive`  an `Exit`  throughout the body
row  `completionsExecutive`  an `Execution`  only inside `execute`
```

`directExecutive` never runs the body speculatively, so a flag which is read anywhere in its
body is its value. Choosing an executive by import is what lets the API of the entry point
stay uniform, while imposing really different requirements on the code inside it.

An `execute` block also supplies an erased `Effectful`. Nothing consumes it internally. Its
purpose is that a method may *demand* it:

```scala
def deleteEverything()(using Effectful): Unit = ()
```

Under the completions executive, an `Effectful` can be obtained only inside an `execute`
block. Such a method therefore cannot be called from the prelude, which is to say that it
cannot run while the user is merely pressing `tab`. Marking the operations which are truly
irreversible in this way makes it hard to invoke one by accident, from code which
completions will run. The `install` operation, which writes completion scripts into place,
requires it for exactly this reason.

### Completing values

A flag completes its own values, when its type can describe them. A type is told how to
suggest values with a `Discoverable` instance, and how to read a chosen value with an
`Interpretable` instance:

```scala
case class Mode(name: Text)

given modeDiscoverable: (Mode is Discoverable) = (_, _) => List(Suggestion(t"debug"), Suggestion(t"release"))

given modeInterpretable: (Mode is Interpretable) = new Interpretable:
  type Self = Mode
  def interpret(arguments: List[Argument]): Optional[Mode] = arguments match
    case argument :: _ => Mode(argument())
    case _             => Unset

def tool5(args: List[Text]): Unit =
  import executives.completionsExecutive
  application(args):
    val mode = Flag[Mode](t"mode", description = t"build mode")()
    execute(Exit.Ok)
```

With these in scope, `mytool build --mode ` offers `debug` and `release`. A `Suggestion` may
have a description, which is shown by the shells that support one. It may be marked
`incomplete`, to complete a value one segment at a time, as for a file path. A flag whose
type is `Pathname` completes [filesystem paths](paths.md) in this way with no further code,
and expands a leading `~` as the shell would.

### Help and manpages

The prelude describes the whole interface: the subcommands, the flags, their descriptions,
which flags are required, and which statuses each command can return. Help therefore needs
no separate source. The completions executive rebuilds the interface as a `Help` tree, by
running the prelude again with synthesized prefixes of arguments, and lays it out, aligned,
in the style of a manpage. The same tree is written as a
[manpage](https://en.wikipedia.org/wiki/Man_page) in roff, with `roff`, and
`Manpages.install` writes that into place.

Everything which is discovered is documented. A `Status` which is returned from an `execute`
block appears under *Exit status*, and an environment variable which is read in the prelude
appears under *Environment*. Descriptions which discovery cannot supply, such as what an
environment variable means, or an example invocation, are added through a `Manual`:

```scala
val manual = Manual
  ( environment  = List(Manual.EnvironmentVariable(t"MYAPP_HOME", t"the home directory")),
    examples     = List(Manual.Example(t"build with four jobs", t"mytool build --jobs 4")) )
```

Within a command, `explain` attaches a paragraph of local help to the section for the
current subcommand, so a deep subcommand documents itself where it is defined.

### Installing completions

A completed application ships with a generic, identical completion script for each shell:
Bash, Zsh, Fish and PowerShell. The script does nothing but forward the current command line
to the application, in completions mode. The built-in administration command writes these
scripts into place:

```sh
mytool '{admin}' install
```

After installation, each shell completes the command through the application itself, so new
subcommands and flags need no change to any script.

### Fast startup

A command which is invoked for completion must start quickly, or the shell feels sluggish.
Running the application as a [daemon](daemons.md) removes the JVM's startup cost from every
invocation. The program stays resident, and each command, including each request for
completions, is dispatched to the running process. The same body which `application` runs is
run by `daemon`, so an application gains fast startup by changing how it is launched, not
how it is written.

### Related

- [Daemons](daemons.md) keeps an application resident, for fast startup.
- [Environment](environment.md) reads the variables and properties which settings fall back
  to.
- [Decoding](decoding.md) reads the value of a flag as a type.
- [Terminal](terminal.md) styles output, and reads the keyboard interactively.
- [Packaging](packaging.md) builds the executable.
