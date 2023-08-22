package fluorescent

import gastronomy.*
import cardinality.*
import rudiments.*

import scala.collection.mutable as scm

class BloomSet
    [ElementType: Digestible, HashType <: HashScheme[?]: HashFunction]
    (approximateSize: Int, targetErrorRate: 0.0 ~ 1.0):
  private val bits: scm.BitSet = scm.BitSet()
  val bitSize: Int = (-1.44*approximateSize*math.log(targetErrorRate.double)).toInt
  val hashCount: Int = ((bitSize.toDouble/approximateSize.toDouble)*math.log(2.0) + 0.5).toInt
  
  private def hash(value: ElementType): BigInt = BigInt(value.digest[HashType].bytes.mutable(using Unsafe)).abs
  
  private def additions(value: ElementType, bitSet: scm.BitSet): Unit =
    @tailrec
    def recur(hash: BigInt, count: Int): Unit =
      if count < hashCount then
        bitSet((hash%bitSize).toInt) = true
        recur(hash/bitSize, count + 1)

    recur(hash(value), 0)
    
  def add(value: ElementType): Unit =
    val bitSet = scm.BitSet()
    additions(value, bitSet)
    synchronized(bits |= bitSet)

  def addAll(elements: Iterable[ElementType]): Unit =
    val bitSet = scm.BitSet()
    elements.foreach(additions(_, bitSet))
    synchronized(bits |= bitSet)

  def mayContain(value: ElementType): Boolean =
    val bitSet = scm.BitSet()
    additions(value, bitSet)
    synchronized(bitSet.subsetOf(bits))

