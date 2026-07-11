// P4: repo idioms next to a stateful capability — a pure opaque transport (Credit <: Long,
// the case-2 regression watch), a `tracked val` constructor param, and a `cap^` capture
// parameter (the Cursor shape). KEY FINDING: the class extends ExclusiveCapability +
// Stateful, NOT Mutable — Mutable implies Unscoped (a classifier), and an Unscoped
// capability may not capture the non-Unscoped `load` thunk (see p4-mutable-unscoped.neg).
// Exclusive/read-only discipline is retained without Unscoped.
//FLAGS: -language:experimental.modularity
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.{ExclusiveCapability, Stateful}

object internal:
  opaque type Credit <: Long = Long
  object Credit:
    def apply(count: Long): Credit = count
  extension (credit: Credit) def count: Long = credit

import internal.*

trait Sizing:
  def block: Int

class Cursor[cap^](load: () ->{cap} Int, tracked val sizing: Sizing)
extends ExclusiveCapability, Stateful:
  private var consumed: Credit = Credit(0)
  def total: Long = consumed.count
  update def advance(): Int =
    val loaded = load()
    consumed = Credit(consumed.count + loaded)
    loaded

object StandardSizing extends Sizing:
  def block: Int = 4096

def drive(): Long =
  val cursor = Cursor({ () => 64 }, StandardSizing)
  cursor.advance()
  cursor.total
