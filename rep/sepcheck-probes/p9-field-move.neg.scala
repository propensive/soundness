// P9 negative (design limit, confirmed both rows): a tracked capability cannot be MOVED
// out of a field. Reading the field (`val slab = current`) widens to a fresh `any` whose
// hidden set covers the enclosing instance, so the re-mint assignment and every later
// `this`-rooted access is a separation failure — there is no take/replace primitive.
// Consequence: the Conduit publish hand-off and Cursor buffer adoption keep ONE audited
// Unsafe swap point each; `consume` enforcement applies to values threaded through call
// chains (factories, combinators), not to field-held state. Also proven en route:
// `consume` on an UNTRACKED abstract-Storage param is vacuous (no use-after rejection) —
// never write it there.
//EXPECT: Separation failure
//EXPECT: hidden
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.{ExclusiveCapability, Mutable, Stateful}
import caps.unsafe.untrackedCaptures

trait Addressable:
  type Storage
  def allocate(size: Int): Storage

final class Slab[storage](val storage: storage) extends Mutable

class Block(val storage: AnyRef, val size: Int)

final class MiniConduit(addressable0: Addressable) extends ExclusiveCapability, Stateful:
  @untrackedCaptures private var mark0: Int = 0
  private var current: Slab[addressable0.Storage]^ = Slab(addressable0.allocate(16))

  private update def relinquish(consume slab: Slab[addressable0.Storage]^, size: Int): Block =
    Block(slab.storage.asInstanceOf[AnyRef], size)

  update def publish(): Block =
    val slab = current
    current = Slab(addressable0.allocate(16))
    val block = relinquish(slab, mark0)
    mark0 = 0
    block
