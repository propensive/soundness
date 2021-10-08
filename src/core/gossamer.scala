/*
    Gossamer, version 0.5.0. Copyright 2020-21 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package gossamer

import rudiments.*
import contextual.*
import wisteria.*

import annotation.targetName

import java.util.regex.*
import java.net.{URLEncoder, URLDecoder}

opaque type Txt = String

extension (text: Txt)
  def s: String = text

extension (string: String)
  def text: Txt = string

  @targetName("add")
  infix def +(other: Txt): String = string+other
  
  def slice(index: Int): String = string.substring(index).nn
  def slice(from: Int, to: Int): String = string.substring(from, to).nn
  def length: Int = string.size
  
  def apply(idx: Int): Char throws OutOfRangeError =
    if idx >= 0 && idx < length then string.charAt(idx)
    else throw OutOfRangeError(idx, 0, length)
  
  def populated: Option[String] = if length == 0 then None else Some(string)
  def cut(delimiter: String): IArray[String] = cut(delimiter, 0)
  
  def cut(delimiter: String, limit: Int): IArray[String] =
    IArray.from(string.split(Pattern.quote(delimiter), limit).nn.map(_.nn))
  
  def bytes: IArray[Byte] = IArray.from(string.getBytes("UTF-8").nn)
  def chars: IArray[Char] = IArray.from(string.toCharArray.nn)
  def urlEncode: String = URLEncoder.encode(string, "UTF-8").nn
  def urlDecode: String = URLDecoder.decode(string, "UTF-8").nn
  def punycode: String = java.net.IDN.toASCII(string).nn
  def lower: String = string.toLowerCase.nn
  def upper: String = string.toUpperCase.nn

  def lspan(width: Int): String =
    if string.size < length then string+" "*(length - string.size) else string.take(width)
  
  def rspan(width: Int): String =
    if string.size < length then " "*(length - string.size)+string else string.takeRight(width)

  def padRight(length: Int, char: Char = ' '): String = 
    if string.size < length then string+char.toString*(length - string.size) else string
  
  def padLeft(length: Int, char: Char = ' '): String =
    if string.size < length then char.toString*(length - string.size)+string else string
  
  def editDistanceTo(other: String): Int =
    val m = string.size
    val n = other.size
    val old = new Array[Int](n + 1)
    val dist = new Array[Int](n + 1)

    for j <- 1 to n do old(j) = old(j - 1) + 1
    
    for i <- 1 to m do
      dist(0) = old(0) + 1

      for j <- 1 to n do
        dist(j) = (old(j - 1) + (if string.charAt(i - 1) == other.charAt(j - 1) then 0 else 1))
          .min(old(j) + 1).min(dist(j - 1) + 1)

      for j <- 0 to n do old(j) = dist(j)
    
    dist(n)

object Txt:
  def apply(str: String): Txt = str

object Joinable:
  given string: Joinable[String] = _.mkString

trait Joinable[T]:
  def join(elements: Iterable[T]): T

extension [T](values: Iterable[T])(using joinable: Joinable[T])
  def join: T = joinable.join(values)
  
  def join(separator: T): T =
    joinable.join(values.flatMap(Iterable(separator, _)).drop(1))
  
  def join(left: T, separator: T, right: T): T =
    Iterable(left, join(separator), right).join
  
  def join(separator: T, penultimate: T): T = values.size match
    case 0 => Iterable().join
    case 1 => values.head
    case _ => Iterable(values.init.join(separator), penultimate, values.last).join
  
  def join(left: T, separator: T, penultimate: T, right: T): T =
    Iterable(left, join(separator, penultimate), right).join

case class OutOfRangeError(idx: Int, from: Int, to: Int)
extends Exception(s"gossamer: the index $idx exceeds the range $from-$to")

object Interpolation:
  case class Input(string: String)

  given [T: Show]: Insertion[Input, T] = value => Input(summon[Show[T]].show(value).s)

  object Str extends Interpolator[Input, String, String]:
    def initial: String = ""
    def parse(state: String, next: String): String = state+next
    def skip(state: String): String = state
    def insert(state: String, input: Input): String = state+input.string
    def complete(state: String): String = state
  
  object Txt extends Interpolator[Input, String, Txt]:
    def initial: String = ""
    def parse(state: String, next: String): String = state+next
    def skip(state: String): String = state
    def insert(state: String, input: Input): String = state+input.string

    def complete(state: String): Txt =
      state.split("\\n\\s*\\n").nn.map(_.nn.replaceAll("\\s\\s*", " ").nn.trim.nn).mkString("\n").nn.text
