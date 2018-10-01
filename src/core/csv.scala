/*
  
  Caesura, version 0.1.0. Copyright 2018 Jon Pretty, Propensive Ltd.

  The primary distribution site is: https://propensive.com/

  Licensed under the Apache License, Version 2.0 (the "License"); you may not use
  this file except in compliance with the License. You may obtain a copy of the
  License at
  
      http://www.apache.org/licenses/LICENSE-2.0
 
  Unless required by applicable law or agreed to in writing, software
  distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
  WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
  License for the specific language governing permissions and limitations under
  the License.

*/
package caesura

import annotation._
import magnolia._

import language.experimental.macros

object Csv {
 
  def apply[T: Encoder](value: T): Row = implicitly[Encoder[T]].encode(value)

  def parse(line: String): Row = {
    @tailrec
    def parseLine(items: Vector[String], idx: Int, quoted: Boolean, start: Int, end: Int,
        join: Boolean): Vector[String] =
      if(line.length <= idx) {
        if(join) items.init :+ items.last+line.substring(start, if(end == -1) idx else end)
        else items :+ line.substring(start, if(end == -1) idx else end)
      } else (line(idx): @switch) match {
        case ',' =>
          if(quoted) parseLine(items, idx + 1, quoted, start, end, join)
          else {
            val elems = if(start == -1) items :+ "" else {
              val suffix = line.substring(start, if(end == -1) idx else end)
              if(join) items.init :+ items.last+suffix else items :+ suffix
            }
            
            parseLine(elems, idx + 1, false, idx + 1, -1, false)
          }

        case '"' =>
          if(quoted) parseLine(items, idx + 1, false, start, idx, join)
          else if(end != -1) {
            parseLine(items :+ line.substring(start, idx), idx + 1, true, idx + 1, -1, true)
          } else parseLine(items, idx + 1, true, idx + 1, -1, false)

        case ch  =>
          parseLine(items, idx + 1, quoted, start, end, join)
      }
    
    Row(parseLine(Vector(), 0, false, 0, -1, false): _*)
  }

  object Decoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = new Decoder[T] {
      def cols: Int = caseClass.parameters.map(_.typeclass.cols).sum
      def decode(row: Row): T = {

        @annotation.tailrec
        def parseParams(row: Row, typeclasses: Seq[Decoder[_]], params: Vector[Any]): T = {
          if(typeclasses.isEmpty) caseClass.rawConstruct(params)
          else {
            val typeclass = typeclasses.head
            val appended = params :+ typeclass.decode(Row(row.elems.take(typeclass.cols): _*))
            parseParams(Row(row.elems.drop(typeclass.cols): _*), typeclasses.tail, appended)
          }
        }

        val typeclasses = caseClass.parameters.map(_.typeclass)
        parseParams(row, typeclasses, Vector())
      }
    }

    implicit val string: Decoder[String] = Decoder(_.elems.head)
    implicit val int: Decoder[Int] = Decoder(_.elems.head.toInt)
    implicit val boolean: Decoder[Boolean] = Decoder(_.elems.head == "true")
    implicit val double: Decoder[Double] = Decoder(_.elems.head.toDouble)
    implicit val byte: Decoder[Byte] = Decoder(_.elems.head.toByte)
    implicit val short: Decoder[Short] = Decoder(_.elems.head.toShort)
    implicit val float: Decoder[Float] = Decoder(_.elems.head.toFloat)
    implicit val char: Decoder[Char] = Decoder(_.elems.head.head)

    def apply[T](fn: Row => T, len: Int = 1) = new Decoder[T] {
      def decode(elems: Row): T = fn(elems)
      def cols: Int = len
    }
    
    implicit def gen[T]: Decoder[T] = macro Magnolia.gen[T]
  }

  trait Decoder[T] {
    def decode(elems: Row): T
    def cols: Int
  }

  object Encoder {
    type Typeclass[T] = Encoder[T]

    def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = new Encoder[T] {
      def encode(value: T) = Row(caseClass.parameters.flatMap { param =>
        param.typeclass.encode(param.dereference(value)).elems
      }: _*)
    }

    implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]

    implicit val string: Encoder[String] = s => Row(s)
    implicit val int: Encoder[Int] = i => Row(i.toString)
    implicit val boolean: Encoder[Boolean] = b => Row(b.toString)
    implicit val byte: Encoder[Byte] = b => Row(b.toString)
    implicit val short: Encoder[Short] = s => Row(s.toString)
    implicit val float: Encoder[Float] = f => Row(f.toString)
    implicit val double: Encoder[Double] = d => Row(d.toString)
    implicit val char: Encoder[Char] = c => Row(c.toString)
  }

  trait Encoder[T] {
    def encode(value: T): Row
  }
}

case class Row(elems: String*) {
  def as[T: Csv.Decoder]: T = implicitly[Csv.Decoder[T]].decode(this)

  override def toString: String = elems.map { field =>
    field.replaceAll("\"", "\"\"")
  }.mkString("\"", "\",\"", "\"")
}
