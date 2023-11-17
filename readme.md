[<img alt="GitHub Workflow" src="https://img.shields.io/github/actions/workflow/status/propensive/exoskeleton/main.yml?style=for-the-badge" height="24">](https://github.com/propensive/exoskeleton/actions)
[<img src="https://img.shields.io/discord/633198088311537684?color=8899f7&label=DISCORD&style=for-the-badge" height="24">](https://discord.gg/7b6mpF6Qcf)
<img src="/doc/images/github.png" valign="middle">

# Exoskeleton

__Interfaces for interacting with the shell__

Exoskeleton is a Scala library to make it easy to write command-line applications, in particular those which
run as a daemon for fast startup (like [Nailgun](https://github.com/facebookarchive/nailgun)) or which require
argument parsing and programmatic tab-completions.

Exoskeleton's launcher script is written in under 100 lines of Bash, and can be used to transform any executable
JAR file whose main entry point is provided by Exoskeleton into a self-contained, portable executable.

A novel API for tab-completions is provided, making it trivial to write interactive command-line applications in
Scala. Tab completions are automatically provided for [`bash`](https://www.gnu.org/software/bash/),
[`zsh`](http://zsh.sourceforge.net/) and [`fish`](https://fishshell.com/).

## Features

- fast daemon process for near-instantaneous invocations
- safe concurrent invocations so that at most one daemon is running at any time
- portable launcher written in Bash
- facilitates single-file distributions of Scala executables
- POSIX-style parameter parsing
- unified programmatic tab-completions for `bash`, `zsh` and `fish`
- easy to specify tab-completions which are dependent on others
- functional API for application entry points
- simple stream access to POSIX interrupts (signals)
- support for messaging between clients
- allows easy bundling with a JDK or JRE for distribution as a single file
- easy installation of completion scripts for each shell


## Availability

Exoskeleton has not yet been published as a binary.

## Getting Started

Exoskeleton has a modular design, so you can use as much or as little of it as you like. The modules include:
 - `args` for parsing command-line arguments
 - `core` to provide an enhanced entry-point into an application (requires `args`)
 - `daemon` for running the application as a daemon using the launcher script (requires `core`)
 - `completions` to provide tab-completions for an application (requires `core`)

By breaking Exoskeleton up into smaller modules, unnecessary dependencies can be avoided for functionality that
is not required. In particular, handling tab-completions and running as a daemon can be chose independently.

## Application Entry Point

In its most trivial usage, Exoskeleton provides an `application` wrapper to implement a main method:

```scala
import exoskeleton.*
import anticipation.*, turbulence.*, rudiments.*, gossamer.*

@main
def myapp(args: IArray[Text]): Unit = application(args):
  Out.println(t"Hello world!")
  ExitStatus.Ok
```

Packaging this as a fat JAR, with its `Main-Class` specified as `myapp` will produce an executable JAR which
can be invoked with,
```bash
java -jar myapp.jar
```

The body of the `application` method makes certain contextual values available. For example, `arguments`
provides access to the application's arguments. Note that the return value is `ExitStatus.Ok`, though
different exit statuses can be specified with `ExitStatus.Fail(1)` or higher numbers; but the value must be
specified.

## Argument Parsing

We make the following distinction between _arguments_ and _parameters_: the word "arguments" is used to
describe a linear sequence of textual values, while we use "parameters" to describe the interpreted meaning of
arguments. So, for example, the arguments of `grep -rA4 pattern` would be [`-rA4`, `pattern`] while its
parameters would be some representation of {"search recursively", "4 lines of trailing content", "search for
pattern"}.

Exoskeleton interprets arguments as parameters. Different interpreters may be used, though most users will want
to use the POSIX parameters interpreter, `parameterInterpretation.posix` to get a `PosixParameters`. With
the contextual `parameterInterpretation.posix` in scope, the `parameters` method can be used to get an instance
of `PosixParameters` instance.

An instance of `PosixParameters` distinguishes between _flags_, which are arguments beginning with `-` or `--`,
and _operands_—all other arguments, and consists of the following:
1. a list of positional operands appearing before the first flag, often considered as "subcommands"
2. a map from flags to a list of operands (being all the arguments up to the next flag)
3. a list of unparsed positional arguments which appear after a `--` argument

While each argument is clearly a string, each is represented by the type `Argument` which encapsulates not just
the argument's `Text` value, but its position on the command line. (The first argument after the command is
numbered `0`, whereas in scripting languages it's typically `1`.) Retaining the argument number turns out to be
useful later, when providing completions.

## Running as a Daemon

Java is known for its slow startup time compared to other languages. While that has improved significantly in
more recent versions, waiting about a second is typical, even for a trivial "Hello world" application. This is
unacceptable for many purposes, in particular, providing dynamic tab-completions.

Furthermore, many programs run faster, the longer they have been running, thanks to the HotSpot just-in-time
compiler. This advantage returns to zero every time the application is started.

Exoskeleton makes it easy to write a program which runs as a daemon. The first time it is called, it starts the JVM as a background process which listens for socket connections on a free local port, and all subsequent
invocations connect to the running JVM through sockets.

A "launcher script" written in Bash handles this and a few other tasks:
1. connecting to a port if the daemon is already running
2. launching the daemon if it is not running
3. forwarding environment information (environment variables, working directory, etc.) to the daemon
4. switching to an appropriate TTY mode
5. handling standard input and output
6. forwarding interrupts received by the script to the daemon
7. returning the exit status when the request has been handled

Nailgun provides two launcher clients written in C and Python. Unfortunately, the former must be installed
separately, distributed as multiple binaries for different CPU architectures, or compiled with a C compiler
before use. And the latter has noticeably poor performance for each invocation.

Exoskeleton's launcher, which is written in Bash, is both portable and fast.

The daemon implementation is an ordinary method call which primarily specifies the code to be invoked for each
invocation, and would typically be the implementation of a main method,
```scala
@main
def myapp(): Unit = daemon:
  Out.println(t"Hello world")
  ExitStatus.Ok
```
or:
```scala
object MyApp:
  def main(args: IArray[String]): Unit = daemon:
    Out.println(t"Hello world")
    ExitStatus.Ok
```

When invoking this `main` method, the code inside the `daemon` block is not run immediately, but a server is
started which listens on a free port, and each time it receives a request on that port, _then_ will the code in
the block be executed. The context of a `daemon` block provides all the same context as an `application` block,
so it's very easy to convert a non-daemon application into a daemon.

Note that unlike an `application` block, no arguments need to be passed to the the `daemon` block, but the
`arguments` and `parameters` methods can be used to get the arguments or parameters for the current invocation.

The `Out` object from [Turbulence](https://github.com/propensive/turbulence/) (or `Err`) must be used for
producing output, since the default `println` method defined in Scala's `Predef` prints to the JVM's global
`System.out`, which corresponds to standard output for the _daemon_ process; not any of the clients. Unless
`System.out` is configured to do something else, its output will be lost.

### Packaging

The launcher script is designed to be invoke a self-contained "fat JAR" file with a `Main-Class` declaration in
its manifest. But furthermore, it can be bundled with the JAR as a single executable file.

This is possible thanks to a feature of JAR files (which are just ZIP files with a different extension): the
index of a ZIP file is at the end, and refers to data within the file as a (negative) offset from the end. This
means that a ZIP file can be prefixed with unrelated data, without affecting its contents or ability to be read.

So a new file consisting of the launcher script followed by the JAR file can be distributed as a single,
standalone, portable, executable file. The launcher script simply calls `java -jar` passing _its own path_ as
the parameter, since it is simultaneously both a script and a JAR file.

The executable can be packaged using,
```bash
cat launcher application.jar > application
chmod +x application
```

### Messaging between daemon clients

An application running as a daemon can, of course, have multiple clients connected simultaneously. Normally,
these would be independent, with no interaction between them.

However, there are times when it's useful for one client to communicate with the others, and a _bus_ is provided
to facilitate this. A bus allows one client to pass messages of some type to _all_ the other clients. It's not
possible to send messages just to one particular client, although any client could decide whether a particular
message is relevant to it.

To use the bus facility, the daemon should have a type parameter specified when it is invoked, for example:
```scala
@main
def myapp(): Unit = daemon[Text]:
  // body
```

This may be any `Matchable` type, and determines what values may be broadcast to other clients, and conversely,
what values may be received (and therefore should be handled).

A message may be sent to all other clients by calling `broadcast` from within a `daemon` block, for example,
```scala
enum BusMessage:
  case Hello, Goodbye

def myapp(): Unit = daemon[BusMessage]:
  broadcast(BusMessage.Hello)
  // do something
  broadcast(BusMessage.Goodbye)
```

If such messages are sent, then they ought to be received too! The contextual method, `bus`, provides a stream
of messages which can be processed while the client is running. However, while the clients may be doing
different things at different times, they are homogeneous, so in order to be useful, the same client
implementation should be both sending and receiving messages on the bus.

A typical implementation might multiplex the event stream from a
[Profanity](https://github.com/propensive/profanity/) terminal with the bus, and handle keypresses and messages
from other clients in an event loop.

## Tab-completions

### The State of Completions

The ability to press the `tab` key with the cursor at some position in an incomplete command line is an
extremely useful feature of modern command-line applications, and offers users a highly interactive experience,
allowing them to discover new features, preview possible flags and operand values and avoid typographic errors
by having values completed without typing.

Tab completions are widely available in Bash, ZSH and Fish, though the support available in each shell differs
greatly. The support in bash is most simplistic, with no more information than the completion text itself
provided to the user, whereas Fish an ZSH both allow a description to be associated with each completion value.

Each shell implements completions differently, so most command-line applications which provide completions do
so through a different script for each shell, often written by different users and potentially having subtle
differences from each other or from the command itself.

Completion scripts also differ in how dynamic their completions are. Most try to duplicate the behavior of the
application by hardcoding subcommands, flags and some completion values, or try to approximate operand values
by calling other shell commands. Many go further to ensure that flags remain self consistent; for example, if
the `-t` flag and the `-x` flags cannot be used together, then `-x` would not be suggested as a completion of
`-` if `-t` already appears on the command line.

This work is admirable, but it's difficult to maintain, and liable to mistakes or compromises.

### Exoskeleton's Completions

Exoskeleton takes the view that the application itself should be the only source of truth for the completions
of arguments to a command, and that to the greatest extent possible, the same code that interprets a command
when it is invoked should be used to evaluate what completions should be provided.

This makes it possible for the complex logic of determining completions to be moved out of scripts written in
Bash, and have it evaluated entirely in the application itself—as long as the application has access to all the
information that would have been available to the completions script.

This greatly simplifies the functionality of completions scripts for Bash, ZSH and Fish. Exoskeleton provides a
_generic_ completion script for each, which is greatly simplified. It's sufficient for each script to capture
the current (incomplete) arguments of the command line, the position of the cursor, and the type of shell, and
to invoke the application in "completions mode", passing it this information so that it can produce output which
can provide the completions for just the _current_ argument.

Within the Scala code, a further innovation is employed to reuse as much logic as possible from interpreting the
arguments in producing the completions, while making an important distinction between the part of the program
which runs when completions are requested, and the part which runs when the user presses `enter` and the command
is invoked.

An application with tab-completions must delimit the side-effecting part of the program in a special `execute`
block which will be run for an invocation, but not for tab-completions. However, code which processes the
parameters will be executed in both cases. This ensures that requesting completions will not cause unwanted
side-effects, but offers an opportunity to have completions inferred from the "prelude" code which runs in both
modes. This is described in detail in the next section.

In the prelude, the act of checking for a flag is sufficient to infer that flag as a possible completion for
any "free" argument. Checking the value of a parameter may, of course, be performed only conditionally in a
branch which is dependent on other parameters, and this would cause that parameter to be proposed only
conditionally too, on _precisely the same condition_!

For example, imagine a command which has a `-v` flag to make it produce output, and a `--verbosity` flag which
can be used _only_ if `-v` is present. The prelude would check for `-v`, and if it's present, would check for
`--verbosity`. The whole expression could return a value such as `Maybe[Verbosity]` which would determine the
verbosity level for the rest of the program.

If a user pressed `tab` after a single `-` character at the end of the command line, it would invoke the
prelude, checking first for `-v`, and (as a side-effect) adding it to the list of flag completions. If there is
no `-v` already on the command-line, then it would be proposed for the completion of `-`. But if there is a
`-v` already present, the branch which checks for `--verbosity` would also run, and (as a side-effect) it would
be added to the set of known flags, and would be proposed as a completion of `-`. (Since it only makes sense
for `-v` to appear once on the command-line, it would _not_ be proposed as a completion in this instance.)

This simple example illustrates how the exact same logic could be used in both scenarios. But that logic can be
as complex as necessary, and depend on any number of variables. But in all cases, completions will be
guaranteed to mirror execution.

### Using Tab Completions

A standalone application or daemon will not process tab completions by default, but they can be "turned on" by
importing the contextual value:
```scala
import executives.completions
```

Having this in scope when `application` of `daemon` is invoked will replace the default _executive_ with the
_completions executive_. The role of the executive is to determine the return type of that invocation's
execution block, as well as the types of contextual values that are available within the block.

This distinction is important for keeping the simple æsthetics of the entry-point API, while imposing necessary
requirements on the structure of the implementation.

Executives are powerful idea, but in practice, only one difference is crucial: with the standard executive, the return value must be an `ExitStatus` instance, while for the completions executive, the return value must be an
instance of `Execution`, which can be constructed with a delimited `execute` block; and an `execute` block must
return an `ExitStatus`.

Here is an illustration of the difference in code. Compare the standard executive,

```scala
@main
def myapp(): Unit = daemon:
  // side-effecting code goes here
  ExitStatus.Ok
```
with the completions executive:
```scala
import executives.completions

@main
def myapp(): Unit = daemon:
  // parameter processing code goes here
  execute:
    // side-effecting code goes here
    ExitStatus.Ok
```
Because the only way to construct an `Execution` is with an `execute` block, and an `execute` block can only be
written by returning an `ExitStatus` value, the structure of a completions application (standalone or daemon)
will always follow this structure. It's not easy to forget the execution block or an exit status because doing
so will produce a complie error.

The completions executive also applies some restrictions in the "prelude" code which do not exist for the
standard executive: standard output is not permitted, except inside an `execute` block. Under the completions
executive, a `Stdio` instance, which is required by calls to `Out.println` and `Err.println`, is not made
available in the context of `daemon` or `application`, since there is nowhere for output to be sent during
evaluation of completions. The `execute` block provides an `Stdio`, though.

One further contextual value is provided in an `execute` block: an instance of `Effectful`. This type is defined
in Exoskeleton, but is not used anywhere. But any user-defined method can require it as a `using` parameter, and
it will ensure that that method can _only_ be called from within an `execute` block, since the only way to
obtain an `Effectful` instance is from within an `execute` block. This makes it very difficult to invoke an
"effectful" method elsewhere, by accident.

## Status

Exoskeleton is classified as __fledgling__. For reference, Scala One projects are
categorized into one of the following five stability levels:

- _embryonic_: for experimental or demonstrative purposes only, without any guarantees of longevity
- _fledgling_: of proven utility, seeking contributions, but liable to significant redesigns
- _maturescent_: major design decisions broady settled, seeking probatory adoption and refinement
- _dependable_: production-ready, subject to controlled ongoing maintenance and enhancement; tagged as version `1.0.0` or later
- _adamantine_: proven, reliable and production-ready, with no further breaking changes ever anticipated

Projects at any stability level, even _embryonic_ projects, are still ready to
be used, but caution should be taken if there is a mismatch between the
project's stability level and the importance of your own project.

Exoskeleton is designed to be _small_. Its entire source code currently consists
of 416 lines of code.

## Building

Exoskeleton can be built on Linux or Mac OS with [Fury](/propensive/fury), however
the approach to building is currently in a state of flux, and is likely to
change.

## Contributing

Contributors to Exoskeleton are welcome and encouraged. New contributors may like to look for issues marked
<a href="https://github.com/propensive/exoskeleton/labels/beginner">beginner</a>.

We suggest that all contributors read the [Contributing Guide](/contributing.md) to make the process of
contributing to Exoskeleton easier.

Please __do not__ contact project maintainers privately with questions unless
there is a good reason to keep them private. While it can be tempting to
repsond to such questions, private answers cannot be shared with a wider
audience, and it can result in duplication of effort.

## Author

Exoskeleton was designed and developed by Jon Pretty, and commercial support and training is available from
[Propensive O&Uuml;](https://propensive.com/).



## Name

Exoskeleton is a library for interacting with shells, which are their exterior skeletons—or Exoskeletons.

In general, Scala One project names are always chosen with some rationale, however it is usually
frivolous. Each name is chosen for more for its _uniqueness_ and _intrigue_ than its concision or
catchiness, and there is no bias towards names with positive or "nice" meanings—since many of the
libraries perform some quite unpleasant tasks.

Names should be English words, though many are obscure or archaic, and it should be noted how
willingly English adopts foreign words. Names are generally of Greek or Latin origin, and have
often arrived in English via a romance language.

## Logo

The logo shows a simplistic and imaginary arthropod, with a pair of wings on each side; and an exoskeleton.

## License

Exoskeleton is copyright &copy; 2023 Jon Pretty & Propensive O&Uuml;, and is made available under the
[Apache 2.0 License](/license.md).
