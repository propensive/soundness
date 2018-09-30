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

import scala.io.Source

import language.experimental.macros

object Csv {
  
  def parse(line: String): CsvRow = {
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
    
    CsvRow(parseLine(Vector(), 0, false, 0, -1): _*)
  }

  def parseFile(file: String): Iterable[CsvRow] =
    Source.fromFile(file).getLines.toIterable.map(parse)

}

object CsvParser {
  type Typeclass[T] = CsvParser[T]

  def combine[T](caseClass: CaseClass[CsvParser, T]): CsvParser[T] = new CsvParser[T] {
    def cols: Int = caseClass.parameters.map(_.typeclass.cols).sum
    def parse(row: CsvRow): T = {

      @annotation.tailrec
      def parseParams(row: CsvRow, typeclasses: Seq[CsvParser[_]], params: Vector[Any]): T = {
        if(typeclasses.isEmpty) caseClass.rawConstruct(params)
        else {
          val typeclass = typeclasses.head
          val appended = params :+ typeclass.parse(CsvRow(row.elems.take(typeclass.cols): _*))
          parseParams(CsvRow(row.elems.drop(typeclass.cols): _*), typeclasses.tail, appended)
        }
      }

      val typeclasses = caseClass.parameters.map(_.typeclass)
      parseParams(row, typeclasses, Vector())
    }
  }

  implicit val string: CsvParser[String] = CsvParser(_.elems.head)
  implicit val int: CsvParser[Int] = CsvParser(_.elems.head.toInt)
  implicit val double: CsvParser[Double] = CsvParser(_.elems.head.toDouble)

  def apply[T](fn: CsvRow => T, len: Int = 1) = new CsvParser[T] {
    def parse(elems: CsvRow): T = fn(elems)
    def cols: Int = len
  }
  
  implicit def gen[T]: CsvParser[T] = macro Magnolia.gen[T]
}

trait CsvParser[T] {
  def parse(elems: CsvRow): T
  def cols: Int
}

case class CsvRow(elems: String*)
