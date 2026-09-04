## Staging

### About

Scala's quoted code — `'{…}` — normally runs in the next stage of the *same* program. Soundness
generalises the idea so that quoted code runs in a different *environment*: a fresh JVM, an
isolated classloader, or in principle any machine that can receive compiled code. A `dispatch`
block quotes ordinary Scala, splices in values from the enclosing scope, and returns the result —
compiled, shipped, executed elsewhere, and its answer carried back — with the values crossing the
boundary through [classloader-neutral data](generic-data.md) or [JSON](json.md).

### On multi-stage programs

Code compiled together is normally run together, in one environment: a single JVM, or a browser
under Scala.js. A distributed application breaks that assumption by construction. The code for
each environment is compiled separately and stays separate all the way from source to execution —
in different JVMs, in a browser, on different machines — and between any two of those environments
there is an inherent contract.

That contract is not enforced by the compiler. Other tools can check it, but they check it from
outside, and rarely with the guarantees Scala gives within a single compilation. So the contract
can be broken, and the breakage shows up at runtime.

Running code somewhere else usually means building an artifact, arranging its execution, and
inventing a serialization boundary — a project's worth of infrastructure for what is conceptually
one expression. Scala's staging machinery already knows how to compile a quoted expression at
runtime and how to splice values into it; what it fixes is *where* the result runs. Compiling both
sides *together*, and delimiting them with quotes and splices, puts the contract back under the
compiler's control: remote code is written beside the local code that calls it, checked with it,
marshalled for it, and maintained in lockstep with it.

### Quotes, splices and phases

The syntax is Scala's own, and it is worth restating what it means. In a macro:

```scala
def say(user: String)(using Quotes): Expr[Unit] =
  val name: Expr[String] = Expr(user)
  '{ Out.println("Hello "+${name}) }
```

`'{…}` and `${…}` mark *phase shifts*: code inside quotes runs one phase later than the code
around it, and code inside a splice one phase earlier. Above, the definition and the use of `name`
sit in the same phase — the *phase consistency principle* requires it — while `Out.println` and
the string literal belong to the next. An `Expr[String]` is the abstract notion of an expression
that will be a `String` in that next phase.

For a macro, "the next phase" is a later compilation. But nothing about quotes requires that
reading. The next phase can be any place code runs *elsewhere*: another JVM, a cloud machine, a
browser. What is gained by expressing it this way rather than by putting the remote code in
another file is that quotes and splices nest and interleave at expression granularity, and the
compiler checks consistency *across* the phases.

Soundness makes the "where" a value: a `Rig` says how code is deployed and invoked, and `dispatch`
does the rest — compilation, caching per call site, marshalling of captured values in and results
out. Everything comes from the `soundness` package:

```scala
import scala.quoted.*
import soundness.*
import embeddings.automatic
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
```

### Dispatching code

`dispatch` on a rig takes a quoted block. Values from the enclosing scope splice in with `${…}`,
carried across the boundary automatically:

```scala
case class Job(name: Text, size: Long)

def runElsewhere(job: Job): Job = Jvm.dispatch:
  '{
     val name = ${job.name}
     Job(t"Processed: $name", ${job.size}*2)
   }
```

The block compiles once per call site and is cached; each call marshals its spliced values in and
its result out. To the caller, `runElsewhere` is an ordinary function — its body just happens to
execute in another JVM.

Names used inside a quote resolve against the imports outside it, but only *static prefixes* do —
names the compiler can resolve without a value. That restriction is not an inconvenience to work
around; it is the safety property. A reference to an object on the local JVM's heap has no meaning
on the remote one, and phase consistency is exactly what stops such a reference from being written
in the first place. Everything that crosses does so through a splice, deliberately and visibly.

Three roles use this machinery, and they are usually three different people. Whoever implements a
`Rig` knows how to package, deploy and run code in some environment — a browser, a container, a
cloud VM — and knows nothing about what will be run. Whoever writes a `dispatch` block knows what
should run remotely, writes it in quotes and splices, and presents it to their own callers as an
ordinary method. And those callers are blissfully unaware that any of this is involved. Only the
first two roles ever see a quote.

### Rigs

Two rigs come provided. `Jvm` compiles the block and runs it in a fresh JVM subprocess — full
isolation, at process-startup cost — while `Isolation` runs it in the same process under an
isolated classloader, cheap and contained:

```scala
Isolation.dispatch:
  '{ t"computed in an isolated classloader" }
```

A new execution environment plugs in by implementing `Rig`: how the compiled code is *staged* to
its destination and how it is *invoked* there. Soundness's own tooling supplies two more examples —
the test enclave that packages a dispatched block as a standalone executable, and the
[benchmark](../standards/benchmarking.md) rig that measures staged code in a controlled JVM.

### Writing a rig

`dispatch` does the hard parts before a rig sees anything: it extracts the classpath from the
running classloader, compiles the quoted code if it is not already compiled, captures the spliced
inputs, and encodes them into a single transportable value. What is left for the rig is one
narrow job — run some code somewhere, given that value, and bring back the value it produced.

A rig therefore declares what its `Result` type is (the bare output, an `Optional`, a `Task` —
whatever suits the environment), which compiler to stage with, how compiled output is *staged* to
its destination, and how to *invoke* it. The `Jvm` rig is the whole of it:

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

`stage.remote` is the crux: hand it a function from the encoded input to the encoded output, and
it takes care of everything either side. Here that function shells out with
[Guillotine](processes.md) to start a JVM, and captures its standard output. A rig for a browser
or a container differs only in that function — and in `stage`, which for a genuinely remote
environment must make the compiled classes *available* there rather than merely naming their
local paths.

### The boundary

What crosses the boundary must be serializable, and the transport is a `Stageable`: the provided
instances carry values as [Pojo](generic-data.md) trees or as JSON. A captured value with no
transportable form is rejected when the block compiles — the boundary is in the types, not
discovered at a distance.

### What is cached, and when it is not

Compiling a block is expensive, so the result is cached — but the cache key matters. It is the
*fingerprint of the staged tree* together with the call site, not the call site alone.

That distinction is load-bearing. One call site inside an inline method may expand to a different
tree at each use, and keying on the call site alone would silently re-run the first expansion for
every subsequent one. Keying on the tree means several distinct expansions from one site each
compile once, while calls that differ only in the data they transport share a single compilation —
which is the behaviour that makes dispatching from a generic or inline context safe.

A spliced value is evaluated once per dispatch, not once per occurrence: `${job.name}` used twice
in a block refers to the same marshalled value rather than evaluating `job.name` twice.

### Errors across the boundary

Code running elsewhere can fail, and the failure has to come back. An error raised in the staged
block is carried across and re-raised at the call site, so `dispatch` reports the failure of the
remote computation rather than reporting that the remote computation could not be reached. A
failure of the rig itself — a subprocess that would not start, a classloader that could not be
built — is distinct, since the two call for different responses.
