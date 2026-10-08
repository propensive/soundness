// P18: a stateful registry whose slots hold context-function handlers in a typed box
// (`Slot[Handler] | Null`) rather than an erased `AnyRef | Null` — the shape exegesis's
// `Lsp.Registry` and espionage's `Acp.Registry` keep behind 75 `@untrackedCaptures`. The claim
// being tested: "a union mentioning a context-function type freshens its capture sets at every
// adaptation", which was the reason for erasing. The box keeps the handler from being applied
// on adaptation; the question is whether the typed field, the assignment through an exclusive
// registry, and the read-back-and-invoke all pass. They do not: the `^` on the handler's
// *parameter* becomes a per-instance `registry.any` once it sits in a field type, so no handler
// (whose parameter is `Workspace^{any}`) can be assigned, and no workspace can be passed to one
// read back. The erased rim is a checker limit (`[field-fresh-param]`), not a leftover.
//EXPECT: capability `any` cannot flow into capture set \{registry\.any\}
//EXPECT: capability `workspace` cannot flow into capture set \{registry\.any\}
import language.experimental.captureChecking
import language.experimental.separationChecking

trait Workspace extends caps.ExclusiveCapability:
  def name: String

type Handler = (workspace: Workspace^) ?=> Unit
type Focused[result] = (workspace: Workspace^, label: String) ?=> result

case class Slot[handler](value: handler)

class Registry extends caps.ExclusiveCapability, caps.Stateful:
  var ready: Slot[Handler] | Null = null
  var opened: Slot[Focused[Int]] | Null = null

def ready(handler: Handler)(using registry: Registry^): Unit =
  registry.ready = Slot[Handler](handler)

def opened(handler: Focused[Int])(using registry: Registry^): Unit =
  registry.opened = Slot[Focused[Int]](handler)

def invoke(registry: Registry^, workspace: Workspace^): Int =
  val slot = registry.ready
  if slot != null then slot.value(using workspace)
  val focused = registry.opened
  if focused != null then focused.value(using workspace, "x") else 0

@main def run(): Unit =
  val registry = Registry()
  ready { println(summon[Workspace^].name) }(using registry)
  opened { summon[String].length }(using registry)
  val workspace = new Workspace { def name = "w" }
  println(invoke(registry, workspace))
