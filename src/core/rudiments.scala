/*

    Rudiments, version 0.1.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
    compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the License is
    distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
    See the License for the specific language governing permissions and limitations under the License.

*/
package rudiments

extension [T](value: T)
  def only[S](pf: PartialFunction[T, S]): Option[S] = Some(value).collect(pf)
  def unit: Unit = ()
  def twin: (T, T) = (value, value)
  def triple: (T, T, T) = (value, value, value)

extension (value: String)
  def populated: Option[String] = if(value.isEmpty) None else Some(value)
  def cut(delimiter: String): IArray[String] = IArray.from(value.split(delimiter))
  def bytes: IArray[Byte] = IArray.from(value.getBytes("UTF-8"))
  def chars: IArray[Char] = IArray.from(value.toCharArray)

extension (values: Traversable[String])
  def join: String = values.mkString
  def join(separator: String): String = values.mkString(separator)
  def join(left: String, separator: String, right: String): String = values.mkString(left, separator, right)
  
  def join(separator: String, last: String): String = values.size match
    case 0 => ""
    case 1 => values.head
    case _ => values.init.mkString(separator)+last+values.last

class Recur[T](fn: => T => T):
  def apply(value: T): T = fn(value)

def fix[T](func: Recur[T] ?=> (T => T)): (T => T) = func(using Recur(fix(func)))
def recur[T: Recur](value: T): T = summon[Recur[T]](value)

extension (obj: Boolean.type)
  def unapply(str: String): Option[Boolean] = try Some(str.toBoolean) catch case e: Exception => None

extension (obj: Byte.type)
  def unapply(str: String): Option[Byte] = try Some(str.toByte) catch case e: Exception => None

extension (obj: Short.type)
  def unapply(str: String): Option[Short] = try Some(str.toShort) catch case e: Exception => None

extension (obj: Int.type)
  def unapply(str: String): Option[Int] = try Some(str.toInt) catch case e: Exception => None

extension (obj: Long.type)
  def unapply(str: String): Option[Long] = try Some(str.toLong) catch case e: Exception => None

extension (obj: Float.type)
  def unapply(str: String): Option[Float] = try Some(str.toFloat) catch case e: Exception => None

extension (obj: Char.type)
  def unapply(str: String): Option[Char] = if str.length == 1 then Some(str.head) else None

extension (obj: Double.type)
  def unapply(str: String): Option[Double] = try Some(str.toDouble) catch case e: Exception => None

extension (ctx: StringContext)
  def str(strings: String*): String = ctx.parts.head + strings.zip(ctx.parts.tail).map(_+_).mkString