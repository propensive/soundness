## Staging

### About

Scala's quoted code, `'{…}`, normally runs in the next stage of the *same* program. Here the
idea is generalized, so that quoted code runs in a different *environment*: a fresh JVM, an
isolated classloader, or in principle any machine which can receive compiled code.

A `dispatch` block quotes ordinary Scala, splices in values from the enclosing scope, and
returns the result. The code is compiled, shipped and executed elsewhere, and its answer is
carried back. Values cross the boundary as [classloader-neutral data](generic-data.md) or as
[JSON](json.md).

### On multi-stage programs

Code which is compiled together is normally run together, in one environment: a single JVM, or
a browser under Scala.js. A distributed application breaks that assumption by its nature. The
code for each environment is compiled separately, and stays separate all the way from source to
execution, in different JVMs, in a browser, or on different machines. Between any two of those
environments there is a contract.

The compiler does not enforce that contract. Other tools can check it, but they check it from
outside, and rarely with the guarantees which Scala gives within one compilation. So the
contract can be broken, and the breakage appears at [runtime].

Running code somewhere else usually means building an artifact, arranging its execution, and
inventing a serialization boundary: a project's worth of infrastructure for what is, in
concept, one expression. Scala's staging machinery already knows how to compile a quoted
expression at runtime, and how to splice values into it. What it fixes is *where* the result
runs. Compiling both sides *together*, and delimiting them with quotes and splices, puts the
contract back under the compiler's control. Remote code is written beside the local code which
calls it, checked with it, marshaled for it, and maintained in step with it. This is
[safety by construction](../philosophy/safety-by-construction.md), extended across a process
boundary.

### Quotes, splices and phases

The syntax is Scala's own, and it is worth restating what it means. In a macro:

```scala fuel
typecheck
##
import scala.quoted.*

def say(user: String)(using Quotes): Expr[Unit] =
  val name: Expr[String] = Expr(user)
  '{ Out.println("Hello "+${name}) }
```

`'{…}` and `${…}` mark *phase shifts*. Code inside quotes runs one phase later than the code
around it, and code inside a splice runs one phase earlier. Above, the definition and the use of
`name` are in the same phase, which the *phase consistency principle* requires. `Out.println`
and the string literal belong to the next phase. An `Expr[String]` is the abstract notion of an
expression which will be a `String` in that next phase.

For a macro, "the next phase" is a later compilation. Nothing about quotes requires that
reading. The next phase can be any place where code runs *elsewhere*: another JVM, a cloud
machine, a browser. Quotes and splices nest and interleave at the granularity of an expression,
and the compiler checks consistency *across* the phases. Putting the remote code in another file
gives neither.

Here the "where" is a value. A `Rig` says how code is deployed and invoked, and `dispatch` does
the rest: compilation, caching for each call site, and marshaling of captured values in and
results out. Everything comes from the `soundness` package:

```scala
import scala.quoted.*
import soundness.*
import environments.javaBaseEnvironment
import workingDirectories.javaBaseWorkingDirectory
import classloaders.threadContextClassloader
import embeddings.automaticEmbedding
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
```

The samples on this page compile and run code in other processes, so they are compiled but not
run.

### Dispatching code

`dispatch` on a rig takes a quoted block. Values from the enclosing scope are spliced in with
`${…}`, and carried across the boundary automatically:

```scala
case class Job(name: Text, size: Long)

def runElsewhere(job: Job): Job = Jvm.dispatch:
  '{
     val name = ${job.name}
     Job(t"Processed: $name", ${job.size}*2)
   }
```

The block compiles once for each call site, and is cached. Each call marshals its spliced values
in and its result out. To the caller, `runElsewhere` is an ordinary function. Its body happens
to execute in another JVM.

Names used inside a quote resolve against the imports outside it, but only *static prefixes* do:
names which the compiler can resolve without a value. That restriction is not an inconvenience
to work around. It is the safety property. A reference to an object on the heap of the local
JVM has no meaning on the remote one, and phase consistency is exactly what stops such a
reference being written at all. Everything which crosses does so through a splice, deliberately
and visibly.

Three roles use this machinery, and they are usually three different people. Whoever implements
a `Rig` knows how to package, deploy and run code in some environment, such as a browser, a
container or a cloud VM, and knows nothing about what will be run. Whoever writes a `dispatch`
block knows what should run remotely, writes it in quotes and splices, and presents it to their
own callers as an ordinary method. Those callers never know that any of this is involved. Only
the first two roles ever see a quote.

### Rigs

Two rigs are provided. `Jvm` compiles the block and runs it in a fresh JVM subprocess, with full
isolation, at the cost of starting a process. `Isolation` runs it in the same process under an
isolated classloader, which is cheap and contained:

```scala fuel
typecheck
##
Isolation.dispatch:
  '{ t"computed in an isolated classloader" }
```

A new environment plugs in by implementing `Rig`, which says how the compiled code is *staged*
to its destination and how it is *invoked* there. The tooling of these libraries supplies more
examples:

```tabular
column  Rig
column  Runs the block
column  A dispatch returns
row  `Jvm`  in a fresh JVM subprocess  the output
row  `Isolation`  in the same process, under an isolated classloader  the output
row  `Enclave`  packaged as a standalone executable, for tests  an `Enclave.Launcher`
row  `Bench`, `Stress`, `Profile`  in a controlled JVM, measured as a [benchmark](../standards/benchmarking.md)  the output
```

### Writing a rig

`dispatch` does the hard parts before a rig sees anything. It extracts the classpath from the
running classloader, compiles the quoted code if it is not already compiled, captures the
spliced inputs, and encodes them into a single transportable value. One narrow job is left for
the rig: run some code somewhere, given that value, and bring back the value it produced.

A rig therefore declares a handful of members:

```tabular
column  Member
column  Declares
column  In `Jvm`
row  `Result[output]`  what a dispatch returns: the bare output, an `Optional`, a `Task`…  `output`
row  `Form`  the encoded form of the input and output  `Text`
row  `Target`  what staging produces  `LocalClasspath`
row  `Transport`  the form spliced values travel in  `Json`
row  `scalac`  the compiler to stage with  `Scalac[3.6]`
row  `stage`  how compiled output is *staged* to its destination  the classpath of the output
row  `invoke`  how it is *invoked* there  a `java` subprocess
```

The `Jvm` rig is the whole of it:

```scala
object Jvm extends Rig:
  type Result[output] = output
  type Form = Text
  type Target = LocalClasspath
  type Transport = Json

  def stage(out: Path on Linux): LocalClasspath = classpath(out)

  val scalac: Scalac[3.6, Universe.Classfile] = Scalac[3.6](List(scalacOptions.experimental))

  protected def invoke[output](stage: Stage[output, Form, Target]): output =
    stage.remote: input =>
      val cmd = sh"java -classpath ${stage.target()} superlunary.Executor $input"
      unsafely(cmd.exec[Text]())
```

`stage.remote` is the crux. It is handed a function from the encoded input to the encoded
output, and it takes care of everything on either side. Here that function shells out with a
[process](processes.md) to start a JVM, and captures its standard output. A rig for a browser or
a container differs only in that function, and in `stage`, which for a truly remote environment
must make the compiled classes *available* there, and not merely name their local paths.

### The boundary

What crosses the boundary must be serializable, and the transport is a `Stageable`. The provided
instances carry values as [Pojo](generic-data.md) trees or as JSON. A captured value with no
transportable form is rejected when the block compiles. The boundary is in the types, and is
not discovered at a distance.

### What is cached, and when it is not

Compiling a block is expensive, so the result is cached. The key of the cache matters. It is the
*fingerprint of the staged tree* together with the call site, and not the call site alone.

That distinction carries weight. One call site inside an inline method may expand to a different
tree at each use. Keying on the call site alone would silently rerun the first expansion for
every later one. Keying on the tree means that several distinct expansions from one site each
compile once, while calls which differ only in the data they transport share a single
compilation. That is what makes dispatching from a generic or inline context safe.

A spliced value is evaluated once for each dispatch, not once for each occurrence. `${job.name}`
used twice in a block refers to the same marshaled value, and `job.name` is not evaluated twice.

### Errors across the boundary

Code which runs elsewhere can fail, and the failure has to come back. An error raised in the
staged block is carried across and raised again at the call site. So `dispatch` reports the
failure of the remote computation, and not that the remote computation could not be reached. A
failure of the rig itself, such as a subprocess which would not start or a classloader which
could not be built, is distinct, since the two call for different responses.

### Related

- [Metaprogramming](metaprogramming.md) describes the compiletime tools which quotes rest on.
- [Compiler](compiler.md) describes the `Scalac` which a rig stages code with.
- [Processes](processes.md) describes the `sh"…"` command which the `Jvm` rig launches.
- [Generic data](generic-data.md) and [JSON](json.md) describe the forms in which values cross
  the boundary.
- [Testing](testing.md) uses the `Enclave` rig to package a program under test.
