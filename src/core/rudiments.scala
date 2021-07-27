/*
    Rudiments, version 0.2.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

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

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

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

extension (value: Bytes)
  def string: String = String(value.to(Array), "UTF-8")
  def unsafeMutable: Array[Byte] = value.asInstanceOf[Array[Byte]]

extension (value: Any)
  def unsafeMatchable: Matchable = value.asInstanceOf[Matchable]

extension (value: Array[Byte])
  def unsafeImmutable: IArray[Byte] = value.asInstanceOf[IArray[Byte]]

extension (value: String)
  def populated: Option[String] = if value.isEmpty then None else Some(value)
  def cut(delimiter: String): IArray[String] = cut(delimiter, 0)
  
  def cut(delimiter: String, limit: Int): IArray[String] =
    IArray.from(value.split(Pattern.quote(delimiter), limit).nn.map(_.nn))
  
  def bytes: IArray[Byte] = IArray.from(value.getBytes("UTF-8").nn)
  def chars: IArray[Char] = IArray.from(value.toCharArray.nn)
  def urlEncode: String = URLEncoder.encode(value, "UTF-8").nn
  def urlDecode: String = URLDecoder.decode(value, "UTF-8").nn
  def lower: String = value.toLowerCase.nn
  def upper: String = value.toUpperCase.nn

  def padRight(length: Int, char: Char = ' '): String = 
    if value.length < length then value+char.toString*(length - value.length) else value
  
  def padLeft(length: Int, char: Char = ' '): String =
    if value.length < length then char.toString*(length - value.length)+value else value

extension (values: Iterable[String])
  def join: String = values.mkString
  def join(separator: String): String = values.mkString(separator)
  
  def join(left: String, separator: String, right: String): String =
    values.mkString(left, separator, right)
  
  def join(separator: String, last: String): String = values.size match
    case 0 => ""
    case 1 => values.head
    case _ => values.init.mkString(separator)+last+values.last

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

extension (ctx: StringContext) def str(strings: (String | Int | Char)*): String =
  ctx.parts.head + strings.zip(ctx.parts.tail).map(_.toString+_.toString).mkString

case class Property(name: String) extends Dynamic:
  def apply(): String throws KeyNotFound =
    Option(System.getProperty(name)).getOrElse(throw KeyNotFound(name)).nn
  
  def selectDynamic(key: String): Property = Property(s"$name.$key")
  def applyDynamic(key: String)(): String throws KeyNotFound = selectDynamic(key).apply()

object Sys extends Dynamic:
  def selectDynamic(key: String): Property = Property(key)
  def applyDynamic(key: String)(): String throws KeyNotFound = selectDynamic(key).apply()

case class KeyNotFound(name: String) extends Exception(str"rudiments: key $name not found")

def √(value: Double): Double = math.sqrt(value)

case class Impossible(message: String) extends Error(message)