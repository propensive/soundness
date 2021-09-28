/*
    Rudiments, version 0.6.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package rudiments

import scala.collection.IterableFactory
import scala.compiletime.*, ops.int.*
import scala.reflect.ClassTag

import java.util.regex.*
import java.io as ji

import language.dynamics

type Bytes = IArray[Byte]

case class TooMuchData() extends Exception(s"the amount of data in the stream exceeds the capacity")

extension (value: LazyList[Bytes])
  def slurp(maxSize: Int): Bytes throws TooMuchData =
    value.foldLeft(IArray[Byte]()) { (acc, next) =>
      if acc.length + next.length > maxSize then throw TooMuchData() else acc ++ next
    }

extension [T](value: T)
  def only[S](pf: PartialFunction[T, S]): Option[S] = Some(value).collect(pf)
  def unit: Unit = ()
  def twin: (T, T) = (value, value)
  def triple: (T, T, T) = (value, value, value)
  def unsafeMatchable: T & Matchable = value.asInstanceOf[T & Matchable]

extension (value: Bytes)
  def uString: String = String(value.to(Array), "UTF-8")
  def unsafeMutable: Array[Byte] = value.asInstanceOf[Array[Byte]]

extension (value: Array[Byte])
  def unsafeImmutable: IArray[Byte] = value.asInstanceOf[IArray[Byte]]

extension [K, V](map: Map[K, V])
  def upsert(key: K, operation: Option[V] => V) = map.updated(key, operation(map.get(key)))
  
  def collate(otherMap: Map[K, V])(merge: (V, V) => V): Map[K, V] =
    otherMap.foldLeft(map) { case (acc, (k, v)) => acc.updated(k, acc.get(k).fold(v)(merge(v, _))) }

class Recur[T](fn: => T => T):
  def apply(value: T): T = fn(value)

def fix[T](func: Recur[T] ?=> (T => T)): (T => T) = func(using Recur(fix(func)))
def recur[T: Recur](value: T): T = summon[Recur[T]](value)

extension (obj: Boolean.type) def unapply(str: String): Option[Boolean] =
  try Some(str.toBoolean) catch Exception => None

extension (obj: Byte.type) def unapply(str: String): Option[Byte] =
  try Some(str.toByte) catch Exception => None

extension (obj: Short.type) def unapply(str: String): Option[Short] =
  try Some(str.toShort) catch Exception => None

extension (obj: Int.type) def unapply(str: String): Option[Int] =
  try Some(str.toInt) catch Exception => None

extension (obj: Long.type) def unapply(str: String): Option[Long] =
  try Some(str.toLong) catch Exception => None

extension (obj: Float.type) def unapply(str: String): Option[Float] =
  try Some(str.toFloat) catch Exception => None

extension (obj: Char.type) def unapply(str: String): Option[Char] =
  if str.length == 1 then Some(str.head) else None

extension (obj: Double.type) def unapply(str: String): Option[Double] =
  try Some(str.toDouble) catch Exception => None

case class Property(name: String) extends Dynamic:
  def apply(): String throws KeyNotFound =
    Option(System.getProperty(name)).getOrElse(throw KeyNotFound(name)).nn
  
  def selectDynamic(key: String): Property = Property(s"$name.$key")
  def applyDynamic(key: String)(): String throws KeyNotFound = selectDynamic(key).apply()

object Sys extends Dynamic:
  def selectDynamic(key: String): Property = Property(key)
  def applyDynamic(key: String)(): String throws KeyNotFound = selectDynamic(key).apply()
  def bigEndian: Boolean = java.nio.ByteOrder.nativeOrder == java.nio.ByteOrder.BIG_ENDIAN

case class KeyNotFound(name: String) extends Exception(s"rudiments: key $name not found")

object Impossible:
  def apply(error: Exception): Impossible =
    Impossible(s"rudiments: an ${error.getClass.getName} exception was thrown when this was not "+
        s"believed to be possible; the error was '${error.getMessage}'")

case class Impossible(message: String) extends Error(message)

object Unset:
  override def toString: String = "<unset>"

type Maybe[T] = Unset.type | T

extension [T](opt: Maybe[T])
  def otherwise(value: => T): T = opt match
    case Unset => value
    case other => other.asInstanceOf[T]
  
  def option: Option[T] = opt match
    case Unset => None
    case other => Some(other.asInstanceOf[T])

extension (iarray: IArray.type)
  def init[T: ClassTag](size: Int)(fn: Array[T] => Unit): IArray[T] =
    val array = new Array[T](size)
    fn(array)
    array.asInstanceOf[IArray[T]]


extension [T](opt: Option[T]) def maybe: Unset.type | T = opt.getOrElse(Unset)

case class StreamCutError() extends Exception("rudiments: the stream was cut prematurely")

type DataStream = LazyList[IArray[Byte] throws StreamCutError]

object Util:
  def read(in: ji.InputStream, limit: Int): DataStream = in match
    case in: ji.BufferedInputStream =>
      def read(): DataStream =
        val avail = in.available
        if avail == 0 then LazyList()
        else
          val buf = new Array[Byte](in.available.min(limit))
          try
            val count = in.read(buf, 0, buf.length)
            if count < 0 then LazyList(buf.unsafeImmutable)
            else buf.unsafeImmutable #:: read()
          catch case error: ji.IOException => LazyList(throw StreamCutError())
      
      read()
    
    case in: ji.InputStream =>
      read(ji.BufferedInputStream(in), limit)
  
  def write(stream: LazyList[IArray[Byte]], out: ji.OutputStream): Unit =
    stream.map(_.unsafeMutable).foreach(out.write(_))