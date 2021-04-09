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

import magnolia.*

import scala.collection.*

import java.security.*
import java.util.Base64.getEncoder as Base64Encoder
import java.lang.{Double as JDouble, Float as JFloat}

sealed trait HashScheme
sealed trait Md5 extends HashScheme
sealed trait Sha256 extends HashScheme
sealed trait Sha1 extends HashScheme

given HashFunction[Md5] = HashFunction("MD5")
given HashFunction[Sha256] = HashFunction("SHA-256")
given HashFunction[Sha1] = HashFunction("SHA1")

extension [T](value: T)
  def digest[A <: HashScheme](using hashFunction: HashFunction[A], hashable: Hashable[T]): Digest =
    Digester(hashable.digest(_, value)).apply(hashFunction)

extension (bytes: IArray[Byte])
  def encoded[E <: EncodingScheme: ByteEncoder]: String = summon[ByteEncoder[E]].encode(bytes)

case class HashFunction[A <: HashScheme](name: String):
  def init: DigestAccumulator = DigestAccumulator(MessageDigest.getInstance(name))

case class Digest(bytes: IArray[Byte]):
  override def toString: String = summon[ByteEncoder[Base64]].encode(bytes)
  def encoded[ES <: EncodingScheme: ByteEncoder]: String = bytes.encoded[ES]

object Hashable extends Derivation[Hashable]:
  def join[T](caseClass: CaseClass[Hashable, T]): Hashable[T] =
    (acc, value) => caseClass.params.foldLeft(acc) { (acc, param) =>
      param.typeclass.digest(acc, param.dereference(value))
    }

  def split[T](sealedTrait: SealedTrait[Hashable, T]): Hashable[T] =
    (acc, value) => sealedTrait.split(value) { subtype =>
      val acc2 = summon[Hashable[Int]].digest(acc, sealedTrait.subtypes.indexOf(subtype))
      subtype.typeclass.digest(acc2, subtype.cast(value))
    }
    
  given[T: Hashable]: Hashable[Traversable[T]] = (acc, xs) => xs.foldLeft(acc)(summon[Hashable[T]].digest)
  given Hashable[Int] = (acc, n) => acc.append(IArray.from((24 to 0 by -8).map(n >> _).map(_.toByte).toArray))
  given Hashable[Long] = (acc, n) => acc.append(IArray.from((52 to 0 by -8).map(n >> _).map(_.toByte).toArray))
  given Hashable[Double] = (acc, n) => summon[Hashable[Long]].digest(acc, JDouble.doubleToRawLongBits(n))
  given Hashable[Float] = (acc, n) => summon[Hashable[Int]].digest(acc, JFloat.floatToRawIntBits(n))
  given Hashable[Boolean] = (acc, n) => acc.append(IArray(if n then 1.toByte else 0.toByte))
  given Hashable[Byte] = (acc, n) => acc.append(IArray(n))
  given Hashable[Short] = (acc, n) => acc.append(IArray((n >> 8).toByte, n.toByte))
  given Hashable[Char] = (acc, n) => acc.append(IArray((n >> 8).toByte, n.toByte))
  given Hashable[String] = (acc, s) => acc.append(IArray.from(s.getBytes("UTF-8")))
  given Hashable[IArray[Byte]] = _.append(_)
  given Hashable[Digest] = (acc, d) => acc.append(d.bytes)

trait Hashable[T]:
  def digest(acc: DigestAccumulator, value: T): DigestAccumulator

case class Digester(run: DigestAccumulator => DigestAccumulator):
  def apply[A <: HashScheme: HashFunction]: Digest = run(summon[HashFunction[A]].init).digest()
  def digest[T: Hashable](value: T): Digester = Digester(run.andThen(summon[Hashable[T]].digest(_, value)))

final case class DigestAccumulator(private val messageDigest: MessageDigest):
  def append(bytes: IArray[Byte]): DigestAccumulator =
    messageDigest.update(bytes.to(Array))
    DigestAccumulator(messageDigest)
  
  def digest(): Digest = Digest(IArray.from(messageDigest.digest()))

trait EncodingScheme
trait Base64 extends EncodingScheme
trait Base64Url extends EncodingScheme
trait Base32 extends EncodingScheme
trait Hex extends EncodingScheme
trait Binary extends EncodingScheme

object ByteEncoder:
  private val HexLookup: IArray[Byte] = IArray.from("0123456789ABCDEF".getBytes)

  given ByteEncoder[Hex] = bytes =>
    val array = new Array[Byte](bytes.length*2)
    bytes.indices.foreach { idx =>
      array(2*idx) = HexLookup((bytes(idx) >> 4) & 0xf)
      array(2*idx + 1) = HexLookup(bytes(idx) & 0xf)
    }
    
    String(array, "UTF-8")

  given ByteEncoder[Base64] = bytes => Base64Encoder.encodeToString(bytes.to(Array))
  
  given ByteEncoder[Base64Url] = bytes =>
    Base64Encoder.encodeToString(bytes.to(Array)).replace('+', '-').replace('/', '_').takeWhile(_ != '=')

trait ByteEncoder[ES <: EncodingScheme]:
  def encode(bytes: IArray[Byte]): String