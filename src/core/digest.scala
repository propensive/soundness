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
  implicit case object Md5 extends Algorithm[Md5]("MD5")
  implicit case object Sha256 extends Algorithm[Sha256]("SHA-256")
  implicit case object Sha1 extends Algorithm[Sha1]("SHA1")
}

abstract class Algorithm[A <: Hash](val name: String) {
  def init: MessageDigest = MessageDigest.getInstance(name)
}

trait Hash
trait Md5 extends Hash
trait Sha256 extends Hash
trait Sha1 extends Hash

case class Digest(value: String) extends AnyVal { override def toString: String = value }

object Digestible {
  type Typeclass[T] = Digestible[T]

  def combine[T](caseClass: CaseClass[Digestible, T]): Digestible[T] =
    (md, value) => caseClass.parameters.foldLeft(md) { (md, param) =>
      param.typeclass.digest(md, param.dereference(value))
    }

  def dispatch[T](sealedTrait: SealedTrait[Digestible, T]): Digestible[T] =
    (md, value) => sealedTrait.dispatch(value) { subtype =>
      val md2 = int.digest(md, sealedTrait.subtypes.indexOf(subtype))
      subtype.typeclass.digest(md2, subtype.cast(value))
    }
    
  private def append[T](toArray: T => Array[Byte]): Digestible[T] = { (md, value) =>
    md.update(toArray(value))
    md
  }
  
  implicit def traversable[T: Digestible]: Digestible[Traversable[T]] = { (md, xs) =>
    xs.foldLeft(md)(implicitly[Digestible[T]].digest)
  }

  implicit val int: Digestible[Int] = append { n =>
    Array((n >> 24).toByte, (n >> 16).toByte, (n >> 8).toByte, n.toByte)
  }
  
  implicit val long: Digestible[Long] = append { n =>
    Array((n >> 56).toByte, (n >> 48).toByte, (n >> 40).toByte, (n >> 32).toByte, (n >> 24).toByte,
        (n >> 16).toByte, (n >> 8).toByte, n.toByte)
  }
  
  implicit val double: Digestible[Double] =
    (md, n) => long.digest(md, java.lang.Double.doubleToRawLongBits(n))
  
  implicit val float: Digestible[Float] = (md, n) => int.digest(md, java.lang.Float.floatToRawIntBits(n))
  implicit val byte: Digestible[Byte] = append(Array(_))
  implicit val string: Digestible[String] = append(_.getBytes("UTF-8"))
  implicit val short: Digestible[Short] = append { n => Array((n >> 8).toByte, n.toByte) }
  implicit val char: Digestible[Char] = append { n => Array((n >> 8).toByte, n.toByte) }
  implicit def gen[T]: Digestible[T] = macro Magnolia.gen[T]
}

trait Digestible[T] { def digest(md: MessageDigest, value: T): MessageDigest }

case class Digester(run: MessageDigest => MessageDigest) {
  def apply[A <: Hash](implicit algorithm: Algorithm[A]): Digest =
    Digest(java.util.Base64.getEncoder.encodeToString(run(algorithm.init).digest()))

  def digest[T: Digestible](value: T): Digester = Digester(run.andThen(implicitly[Digestible[T]].digest(_, value)))
}
