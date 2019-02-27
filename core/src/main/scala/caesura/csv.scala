package caesura

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

import magnolia._

import scala.annotation._
import scala.language.experimental.macros

trait Format {

  implicit val stringEncoder: Encoder[String] = s => Row(s)
  implicit val intEncoder: Encoder[Int] = i => Row(i.toString)
  implicit val booleanEncoder: Encoder[Boolean] = b => Row(b.toString)
  implicit val byteEncoder: Encoder[Byte] = b => Row(b.toString)
  implicit val shortEncoder: Encoder[Short] = s => Row(s.toString)
  implicit val floatEncoder: Encoder[Float] = f => Row(f.toString)
  implicit val doubleEncoder: Encoder[Double] = d => Row(d.toString)
  implicit val charEncoder: Encoder[Char] = c => Row(c.toString)

  implicit val stringDecoder: Decoder[String] = Decoder(_.elems.head)
  implicit val intDecoder: Decoder[Int] = Decoder(_.elems.head.toInt)
  implicit val booleanDecoder: Decoder[Boolean] = Decoder(_.elems.head == "true")
  implicit val doubleDecoder: Decoder[Double] = Decoder(_.elems.head.toDouble)
  implicit val byteDecoder: Decoder[Byte] = Decoder(_.elems.head.toByte)
  implicit val shortDecoder: Decoder[Short] = Decoder(_.elems.head.toShort)
  implicit val floatDecoder: Decoder[Float] = Decoder(_.elems.head.toFloat)
  implicit val charDecoder: Decoder[Char] = Decoder(_.elems.head.head)

  protected val separator: Char

  def apply[T: Encoder](value: T): Row = implicitly[Encoder[T]].encode(value)

  def parse(line: String): Row = {
    @tailrec
    def parseLine(items: Vector[String], idx: Int, quoted: Boolean, start: Int, end: Int,
                  join: Boolean): Vector[String] =
      if (line.length <= idx) {
        if (join) items.init :+ items.last + line.substring(start, if (end == -1) idx else end)
        else items :+ line.substring(start, if (end == -1) idx else end)
      } else (line(idx): @switch) match {
        case `separator` =>
          if (quoted) parseLine(items, idx + 1, quoted, start, end, join)
          else {
            val elems = if (start == -1) items :+ "" else {
              val suffix = line.substring(start, if (end == -1) idx else end)
              if (join) items.init :+ items.last + suffix else items :+ suffix
            }

            parseLine(elems, idx + 1, quoted = false, idx + 1, -1, join = false)
          }

        case '"' =>
          if (quoted) parseLine(items, idx + 1, quoted = false, start, idx, join = join)
          else if (end != -1) {
            parseLine(items :+ line.substring(start, idx), idx + 1, quoted = true, idx + 1, -1, join = true)
          } else parseLine(items, idx + 1, quoted = true, idx + 1, -1, join = false)

        case _ =>
          parseLine(items, idx + 1, quoted, start, end, join)
      }

    Row(parseLine(Vector(), 0, quoted = false, 0, -1, join = false): _*)
  }

  object Decoder {
    type Typeclass[T] = Decoder[T]

    def combine[T](caseClass: CaseClass[Decoder, T]): Decoder[T] = new Decoder[T] {
      def cols: Int = caseClass.parameters.map(_.typeclass.cols).sum

      def decode(row: Row): T = {

        @annotation.tailrec
        def parseParams(row: Row, typeclasses: Seq[Decoder[_]], params: Vector[Any]): T = {
          if (typeclasses.isEmpty) caseClass.rawConstruct(params)
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

    def apply[T](fn: Row => T, len: Int = 1): Decoder[T] = new Decoder[T] {
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

    def combine[T](caseClass: CaseClass[Encoder, T]): Encoder[T] = (value: T) =>
      Row(caseClass.parameters.flatMap {
        param => param.typeclass.encode(param.dereference(value)).elems
      }: _*)

    implicit def gen[T]: Encoder[T] = macro Magnolia.gen[T]
  }

  trait Encoder[T] {
    def encode(value: T): Row
  }

  case class Row(elems: String*) {
    def as[T: Decoder]: T = implicitly[Decoder[T]].decode(this)

    override def toString: String = elems
      .map {
        _.replaceAll("\"", "\"\"")
      }
      .mkString("\"", s""""$separator"""", "\"")
  }

}

object Csv extends Format {
  override protected val separator = ','
}

object Tsv extends Format {
  override protected val separator = '\t'
}
