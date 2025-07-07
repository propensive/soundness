import language.experimental.captureChecking
import scala.caps

object internal:
  opaque type Tagged[+value, tag] = value

  object Tagged:
    inline def apply[tag](value: Any): Tagged[value.type, tag] = value

  extension [value, tag](tagged: Tagged[value, tag]) inline def apply(): value =
    tagged.asInstanceOf[value]

infix type aka[subject, label <: String] = internal.Tagged[subject, label]

extension (any: Any)
  inline def aka[label <: String]: any.type aka label = internal.Tagged[label](any)

trait Tactic extends caps.ExclusiveCapability

trait TC[T]:
  def go(t: T): Int

given tc: (t: Tactic) => ((TC[Int])^{t}) = i => 42

inline def use[R](inline lambda: (TC[Int] aka "contextual") ?=> R): R =
  val inst = compiletime.summonInline[TC[Int]^]
  lambda(using inst.aka["contextual"])

def caller(using t: Tactic): Int =
  use { ctx ?=> ctx().go(3) }
