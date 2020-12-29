/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Rudiments, version 0.1.0. Copyright 2020 Jon Pretty, Propensive OÜ.                                       ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/

package rudiments

import scala.collection._, mutable.{Builder, ArrayBuffer}

import language.implicitConversions

object `package` {
  @inline implicit def traversableStringExtensions(strings: Traversable[String]): TraversableStringExtensions =
    TraversableStringExtensions(strings)
  
  @inline implicit def globalExtensions[T](value: T): GlobalExtensions[T] = GlobalExtensions(value)
  @inline implicit def stringExtensions(string: String): StringExtensions = StringExtensions(string)
}


case class GlobalExtensions[T](value: T) extends AnyVal {
  @inline final def only[S](pf: PartialFunction[T, S]): Option[S] = Some(value).collect(pf)
  @inline final def unit: Unit = ()
  @inline final def swallow[S]: S => T = { _ => value }
  @inline final def pass(fn: T => Any): T = { fn(value); value }
}

case class StringExtensions(value: String) extends AnyVal {
  @inline final def populated: Option[String] = if(value.isEmpty) None else Some(value)
}

object Bytes {
  def apply(array: Array[Byte]): Bytes = new Bytes(array.clone)
}

final case class Bytes private[rudiments] (private val array: Array[Byte])
    extends IndexedSeqOptimized[Byte, Bytes] {
  override def equals(that: Any): Boolean = that match {
    case that: Bytes => java.util.Arrays.equals(array, that.array)
    case _           => false
  }

  override def hashCode: Int = java.util.Arrays.hashCode(array)
  def seq: IndexedSeq[Byte] = array
  def length: Int = array.length
  override def toString: String = s"[${size}B]"
  def apply(index: Int): Byte = array(index)

  def newBuilder: Builder[Byte, Bytes] = new Builder[Byte, Bytes] {
    private val underlying = ArrayBuffer.empty[Byte]
    
    def +=(elem: Byte): this.type = {
      underlying += elem
      this
    }

    def clear(): Unit = underlying.clear()
    def result(): Bytes = Bytes(underlying.toArray)
  }
}

case class TraversableStringExtensions(values: Traversable[String]) extends AnyVal {
  def join: String = values.mkString
  def join(separator: String): String = values.mkString(separator)
  def join(left: String, separator: String, right: String): String = values.mkString(left, separator, right)
}
