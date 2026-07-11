// P5 negative (the highest-risk finding, confirmed): Array-as-Mutable does NOT compose
// through an abstract type member — the Addressable.Storage shape the kernel is generic
// in. A bare abstract `Storage` param means PURE (not read-only), fresh allocate results
// cannot flow into the bare abstract view, and an exclusive `Storage^` field cannot be
// reassigned ("not visible from any"). Kernel implication: Addressable's primitives keep
// untracked storage (or go per-medium concrete); stage-level exclusivity carries the
// safety instead.
//EXPECT: error
import language.experimental.captureChecking
import language.experimental.separationChecking

import caps.{ExclusiveCapability, Stateful}

trait Addressable:
  type Storage
  def allocate(size: Int): Storage^
  def transfer(from: Storage, fromIndex: Int, to: Storage^, toIndex: Int, count: Int): Unit

object ByteAddressable extends Addressable:
  type Storage = Array[Byte]
  def allocate(size: Int): Storage^ = new Array[Byte](size)
  def transfer(from: Storage, fromIndex: Int, to: Storage^, toIndex: Int, count: Int): Unit =
    System.arraycopy(from, fromIndex, to, toIndex, count)

class Buffer(val addressable: Addressable) extends ExclusiveCapability, Stateful:
  private var storage: addressable.Storage^ = addressable.allocate(16)
  update def grow(): Unit =
    val bigger = addressable.allocate(32)
    addressable.transfer(storage, 0, bigger, 0, 16)
    storage = bigger
