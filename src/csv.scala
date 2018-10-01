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
    def parseLine(items: Vector[String], idx: Int, quoted: Boolean, start: Int, end: Int)
                 : Vector[String] =
      if(line.length <= idx) items :+ line.substring(start, if(end == -1) idx else end)
      else (line(idx): @switch) match {
        case ',' =>
          if(quoted) parseLine(items, idx + 1, quoted, start, end)
          else {
            val next = if(start == -1) "" else line.substring(start, if(end == -1) idx else end)
            parseLine(items :+ next, idx + 1, false, idx + 1, -1)
          }
        case '"' =>
          if(quoted) parseLine(items, idx + 1, false, start, idx)
          else parseLine(items, idx + 1, true, idx + 1, -1)
        case ch  =>
          parseLine(items, idx + 1, quoted, start, end)
      }
    
    Row(parseLine(Vector(), 0, false, 0, -1))
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
            val appended = params :+ typeclass.decode(Row(row.elems.take(typeclass.cols)))
            parseParams(Row(row.elems.drop(typeclass.cols)), typeclasses.tail, appended)
          }
        }

        val typeclasses = caseClass.parameters.map(_.typeclass)
        parseParams(row, typeclasses, Vector())
      }
    }

    implicit val string: Decoder[String] = Decoder(_.elems.head)
    implicit val int: Decoder[Int] = Decoder(_.elems.head.toInt)
    implicit val double: Decoder[Double] = Decoder(_.elems.head.toDouble)

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
      }.to[Vector])
    }

    implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]

    implicit val string: Encoder[String] = s => Row(Vector(s))
    implicit val int: Encoder[Int] = i => Row(Vector(i.toString))
    implicit val boolean: Encoder[Boolean] = b => Row(Vector(b.toString))
    implicit val byte: Encoder[Byte] = b => Row(Vector(b.toString))
    implicit val short: Encoder[Short] = s => Row(Vector(s.toString))
    implicit val float: Encoder[Float] = f => Row(Vector(f.toString))
    implicit val double: Encoder[Double] = d => Row(Vector(d.toString))
  }

  trait Encoder[T] {
    def encode(value: T): Row
  }
}

case class Row(elems: Vector[String]) {
  def as[T: Csv.Decoder]: T = implicitly[Csv.Decoder[T]].decode(this)

  override def toString: String = elems.map { field =>
    field.replaceAll("\"", "\"\"")
  }.mkString("\"", "\",\"", "\"")
}
