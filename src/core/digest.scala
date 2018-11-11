/*
  Gastronomy, version 1.0.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except
  in compliance with the License. You may obtain a copy of the License at

      http://www.apache.org/licenses/LICENSE-2.0

  Unless required  by applicable  law or  agreed to  in writing,  software  distributed  under the
  License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either
  express  or  implied.  See  the  License for  the specific  language  governing  permissions and
  limitations under the License.
                                                                                                  */
package gastronomy

import magnolia._

import scala.collection._

import java.security._
import java.util.Arrays

import language.experimental.macros

object `package` {
  implicit class DigestExtension[T](val value: T) extends AnyVal {
    def digest[A <: HashScheme]
              (implicit hashFunction: HashFunction[A], hashable: Hashable[T])
              : Digest =
      Digester(hashable.digest(_, value)).apply(hashFunction)
  }
}

object HashFunction {
  implicit val md5: HashFunction[Md5] = HashFunction("MD5")
  implicit val sha256: HashFunction[Sha256] = HashFunction("SHA-256")
  implicit val sha1: HashFunction[Sha1] = HashFunction("SHA1")
}

final case class HashFunction[A <: HashScheme](name: String) extends AnyVal {
  def init: DigestAccumulator = new DigestAccumulator(MessageDigest.getInstance(name))
}

trait HashScheme
trait Md5 extends HashScheme
trait Sha256 extends HashScheme
trait Sha1 extends HashScheme

final case class Digest(bytes: Bytes) extends AnyVal {
  override def toString: String = ByteEncoder.base64.encode(bytes)
  def encoded[ES <: EncodingScheme: ByteEncoder]: String = bytes.encoded[ES]
}

object Hashable {
  type Typeclass[T] = Hashable[T]

  def combine[T](caseClass: CaseClass[Hashable, T]): Hashable[T] =
    (acc, value) => caseClass.parameters.foldLeft(acc) { (acc, param) =>
      param.typeclass.digest(acc, param.dereference(value))
    }

  def dispatch[T](sealedTrait: SealedTrait[Hashable, T]): Hashable[T] =
    (acc, value) => sealedTrait.dispatch(value) { subtype =>
      val acc2 = int.digest(acc, sealedTrait.subtypes.indexOf(subtype))
      subtype.typeclass.digest(acc2, subtype.cast(value))
    }
    
  implicit def traversable[T: Hashable]: Hashable[Traversable[T]] = 
    (acc, xs) => xs.foldLeft(acc)(implicitly[Hashable[T]].digest)

  implicit val int: Hashable[Int] =
    (acc, n) => acc.append(Array((n >> 24).toByte, (n >> 16).toByte, (n >> 8).toByte, n.toByte))
  
  implicit val long: Hashable[Long] =
    (acc, n) => acc.append(Array((n >> 56).toByte, (n >> 48).toByte, (n >> 40).toByte,
        (n >> 32).toByte, (n >> 24).toByte, (n >> 16).toByte, (n >> 8).toByte, n.toByte))
  
  implicit val double: Hashable[Double] =
    (acc, n) => long.digest(acc, java.lang.Double.doubleToRawLongBits(n))
  
  implicit val float: Hashable[Float] =
    (acc, n) => int.digest(acc, java.lang.Float.floatToRawIntBits(n))
  
  implicit val boolean: Hashable[Boolean] =
    (acc, n) => acc.append(Array(if(n) 1.toByte else 0.toByte))
  
  implicit val byte: Hashable[Byte] = (acc, n) => acc.append(Array(n))
  implicit val short: Hashable[Short] = (acc, n) => acc.append(Array((n >> 8).toByte, n.toByte))
  implicit val char: Hashable[Char] = (acc, n) => acc.append(Array((n >> 8).toByte, n.toByte))
  implicit val string: Hashable[String] = (acc, s) => acc.append(s.getBytes("UTF-8"))
  implicit def gen[T]: Hashable[T] = macro Magnolia.gen[T]
}

trait Hashable[T] { def digest(acc: DigestAccumulator, value: T): DigestAccumulator }

final case class Digester(run: DigestAccumulator => DigestAccumulator) {
  def apply[A <: HashScheme: HashFunction]: Digest = run(implicitly[HashFunction[A]].init).digest()
  
  def digest[T: Hashable](value: T): Digester =
    Digester(run.andThen(implicitly[Hashable[T]].digest(_, value)))
}

final case class DigestAccumulator(private val messageDigest: MessageDigest) {
  def append(bytes: Array[Byte]): DigestAccumulator = {
    messageDigest.update(bytes)
    new DigestAccumulator(messageDigest)
  }
  
  def digest(): Digest = Digest(Bytes(messageDigest.digest()))
}

object Bytes {
  def apply[Coll[T] <: Seq[T]]
           (bytes: Coll[Byte])
           (implicit cbf: generic.CanBuildFrom[Coll[Byte], Byte, Array[Byte]]) =
    new Bytes(bytes.to[Array])
  
  def apply(bytes: Array[Byte]): Bytes = new Bytes(bytes.clone)
}

final class Bytes private[gastronomy] (private[gastronomy] val array: Array[Byte]) {
  def length: Int = array.size
  def size: Int = length
  override def toString: String = s"[${size}B]"
  override def hashCode: Int = Arrays.hashCode(array)
 
  def apply(index: Int): Byte = array(index)

  def ++(that: Bytes): Bytes = {
    val newArray = new Array[Byte](size + that.size)
    System.arraycopy(array, 0, newArray, 0, array.size)
    System.arraycopy(that.array, 0, newArray, array.size, that.array.size)
    new Bytes(newArray)
  }

  override def equals(that: Any): Boolean = that match {
    case that: Bytes => Arrays.equals(array, that.array)
    case _ => false
  }
  
  def to[Coll[T]](implicit cbf: generic.CanBuildFrom[Array[Byte], Byte, Coll[Byte]]): Coll[Byte] =
    array.to[Coll]

  def encoded[ES <: EncodingScheme: ByteEncoder]: String = implicitly[ByteEncoder[ES]].encode(this)
}

trait EncodingScheme
trait Base64 extends EncodingScheme
trait Base32 extends EncodingScheme
trait Hex extends EncodingScheme
trait Binary extends EncodingScheme

object ByteEncoder {
  implicit val hex: ByteEncoder[Hex] =
    bytes => javax.xml.bind.DatatypeConverter.printHexBinary(bytes.array)
  
  implicit val base64: ByteEncoder[Base64] =
    bytes => java.util.Base64.getEncoder.encodeToString(bytes.array)
}

trait ByteEncoder[ES <: EncodingScheme] { def encode(bytes: Bytes): String }

