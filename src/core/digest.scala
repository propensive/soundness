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

import scala.collection.mutable._
import java.security._
import magnolia._

import language.experimental.macros

object `package` {
  implicit class DigestExtension[T](val value: T) extends AnyVal {
    def digest[A <: Hash](implicit algorithm: Algorithm[A], digestible: Digestible[T]): Digest =
      Digester(digestible.digest(_, value)).apply(algorithm)
  }
}

object Algorithm {
  implicit val Md5: Algorithm[Md5] = Algorithm("MD5")
  implicit val Sha256: Algorithm[Sha256] = Algorithm("SHA-256")
  implicit val Sha1: Algorithm[Sha1] = Algorithm("SHA1")
}

final case class Algorithm[A <: Hash](name: String) extends AnyVal {
  def init: Accumulator = new Accumulator(MessageDigest.getInstance(name))
}

trait Hash
trait Md5 extends Hash
trait Sha256 extends Hash
trait Sha1 extends Hash

final case class Digest(value: String) extends AnyVal { override def toString: String = value }

object Digestible {
  type Typeclass[T] = Digestible[T]

  def combine[T](caseClass: CaseClass[Digestible, T]): Digestible[T] =
    (acc, value) => caseClass.parameters.foldLeft(acc) { (acc, param) =>
      param.typeclass.digest(acc, param.dereference(value))
    }

  def dispatch[T](sealedTrait: SealedTrait[Digestible, T]): Digestible[T] =
    (acc, value) => sealedTrait.dispatch(value) { subtype =>
      val acc2 = int.digest(acc, sealedTrait.subtypes.indexOf(subtype))
      subtype.typeclass.digest(acc2, subtype.cast(value))
    }
    
  implicit def traversable[T: Digestible]: Digestible[Traversable[T]] = 
    (acc, xs) => xs.foldLeft(acc)(implicitly[Digestible[T]].digest)

  implicit val int: Digestible[Int] =
    (acc, n) => acc.append(Array((n >> 24).toByte, (n >> 16).toByte, (n >> 8).toByte, n.toByte))
  
  implicit val long: Digestible[Long] =
    (acc, n) => acc.append(Array((n >> 56).toByte, (n >> 48).toByte, (n >> 40).toByte,
        (n >> 32).toByte, (n >> 24).toByte, (n >> 16).toByte, (n >> 8).toByte, n.toByte))
  
  implicit val double: Digestible[Double] =
    (acc, n) => long.digest(acc, java.lang.Double.doubleToRawLongBits(n))
  
  implicit val float: Digestible[Float] = (acc, n) => int.digest(acc, java.lang.Float.floatToRawIntBits(n))
  implicit val byte: Digestible[Byte] = (acc, n) => acc.append(Array(n))
  implicit val boolean: Digestible[Boolean] = (acc, n) => acc.append(Array(if(n) 1.toByte else 0.toByte))
  implicit val short: Digestible[Short] = (acc, n) => acc.append(Array((n >> 8).toByte, n.toByte))
  implicit val char: Digestible[Char] = (acc, n) => acc.append(Array((n >> 8).toByte, n.toByte))
  implicit val string: Digestible[String] = (acc, s) => acc.append(s.getBytes("UTF-8"))
  implicit def gen[T]: Digestible[T] = macro Magnolia.gen[T]
}

trait Digestible[T] { def digest(acc: Accumulator, value: T): Accumulator }

case class Digester(run: Accumulator => Accumulator) {
  def apply[A <: Hash](implicit algorithm: Algorithm[A]): Digest = run(algorithm.init).digest()
  def digest[T: Digestible](value: T): Digester = Digester(run.andThen(implicitly[Digestible[T]].digest(_, value)))
}

case class Accumulator(private val messageDigest: MessageDigest) {
  def append(bytes: Array[Byte]): Accumulator = {
    messageDigest.update(bytes)
    new Accumulator(messageDigest)
  }
  
  def digest(): Digest = Digest(java.util.Base64.getEncoder.encodeToString(messageDigest.digest()))
}
