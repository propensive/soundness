/*
    Probably, version [unreleased]. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package probably

import anticipation.*
import gossamer.*
import rudiments.*

import scala.collection.mutable.BitSet
import scala.io.*
import scala.runtime.coverage.*

import java.io.*

case class Coverage(path: Text, spec: IArray[Juncture], oldHits: Set[Int], hits: Set[Int]):
  lazy val structure: Map[Text, List[Surface]] =
    val index: Int = spec.lastIndexWhere(_.id == 0)
    spec.to(List).drop(index).groupBy(_.path).map: (path, junctures) =>
      path -> Surface.collapse(junctures.sortBy(-_.end).sortBy(_.start), Nil)

    . to(Map)

object Coverage:
  def apply(): Option[Coverage] = currentDir.map: dir =>
    val currentFile = Invoker.measurementFile(dir.s)
    val hits = measurements(currentFile)
    val dirFile = File(dir.s)

    if !dirFile.exists() then Coverage(dir, IArray(), Set(), Set())
    else
      val otherFiles = Option(dirFile.listFiles).map(_.nn).map(_.to(List).map(_.nn)).getOrElse(Nil)
      val measurementFiles = otherFiles.filter(_.getName.nn.startsWith("scoverage.measurements"))

      val allHits: Set[Int] = measurementFiles.flatMap(measurements(_)).to(Set)
      val oldHits: Set[Int] = allHits -- hits

      Coverage(dir, spec(dir), oldHits, hits)

  private def currentDir: Option[Text] =
    Option(System.getProperty("scalac.coverage")).map(_.nn).map(Text(_))

  private def spec(dir: Text): IArray[Juncture] =
    val file = java.io.File(java.io.File(dir.s), "scoverage.coverage")
    val lines = Source.fromFile(file).getLines.to(Stream).map(Text(_))

    def recur(lines: Stream[Text], junctures: List[Juncture] = Nil): List[Juncture] =
      lines match
        case As[Int](id) #:: path #:: _ #:: _ #:: _ #:: className #:: methodName #::
            As[Int](start) #:: As[Int](end) #:: As[Int](lineNo) #:: symbolName #:: treeName #::
            As[Boolean](branch) #:: _ #:: As[Boolean](ignored) #:: tail =>

          val juncture = Juncture(id, path, className, methodName, start, end, lineNo + 1,
              symbolName, treeName, branch, ignored, tail.takeWhile(!_.starts(t"\f")).to(List))

          recur(tail.dropWhile(!_.starts(t"\f")).tail, juncture :: junctures)

        case _ =>
          junctures.reverse

    IArray.from(recur(lines.dropWhile(_.starts(t"#"))))

  private def measurements(file: File): Set[Int] =
    val ids = BitSet()
    if !file.exists() then Set()
    else Source.fromFile(file).getLines.to(Stream).each: id =>
      ids(id.toInt) = true

    ids.to(Set)
