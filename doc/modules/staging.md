## Staging

### About

Scala's quoted code — `'{…}` — normally runs in the next stage of the *same* program. Soundness
generalises the idea so that quoted code runs in a different *environment*: a fresh JVM, an
isolated classloader, or in principle any machine that can receive compiled code. A `dispatch`
block quotes ordinary Scala, splices in values from the enclosing scope, and returns the result —
compiled, shipped, executed elsewhere, and its answer carried back — with the values crossing the
boundary through [classloader-neutral data](generic-data.md) or [JSON](json.md).

### On multi-stage programs

Running code somewhere else usually means building an artifact, arranging its execution, and
inventing a serialization boundary — a project's worth of infrastructure for what is conceptually
one expression. Scala's staging machinery already knows how to compile a quoted expression at
runtime and how to splice values into it; what it fixes is *where* the result runs.

Soundness makes the "where" a value: a `Rig` says how code is deployed and invoked, and `dispatch`
does the rest — compilation, caching per call site, marshalling of captured values in and results
out. Everything comes from the `soundness` package:

```scala
import scala.quoted.*
import soundness.*
import embeddings.automatic
import strategies.throwUnsafely
import systems.javaSystem
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

### The boundary

What crosses the boundary must be serializable, and the transport is a `Stageable`: the provided
instances carry values as [Pojo](generic-data.md) trees or as JSON. A captured value with no
transportable form is rejected when the block compiles — the boundary is in the types, not
discovered at a distance.
