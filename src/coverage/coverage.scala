/*
    Probably, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

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

import rudiments.*
import gossamer.*
import digression.*

import scala.runtime.coverage.*
import scala.io.*
import scala.collection.mutable.BitSet

import java.io.*

object Surface:
  given Ordering[Surface] = Ordering.by[Surface, Int](_.start).orElseBy(-_.end)

case class Surface
    (id: Int, path: Text, className: Text, methodName: Text, start: Int, end: Int, lineNo: Int,
        symbolName: Text, treeName: Text, branch: Boolean, ignored: Boolean, code: List[Text]):
  def contains(right: Surface): Boolean =
    right.start >= start && right.end <= end && !(right.start == start && right.end == end)
  
  def shortCode: Text =
    val lines = code.flatMap(_.cut(t"\\n"))
    if lines.length > 1 then t"${lines.head}..." else lines.head
  
  def method: StackTrace.Method = StackTrace.Method(
    StackTrace.rewrite(className.s),
    StackTrace.rewrite(methodName.s, method = true),
  )

object SurfaceTree:
  def collapse(todo: List[Surface], done: List[SurfaceTree]): List[SurfaceTree] = todo match
    case Nil =>
      done.reverse
    
    case head :: tail =>
      val todo2 = tail.takeWhile(head.contains(_))
      collapse(tail.drop(todo2.length), SurfaceTree(head, collapse(todo2, Nil)) :: done)

case class SurfaceTree(surface: Surface, children: List[SurfaceTree]):
  def covered(hits: Set[Int]): Boolean =
    hits.contains(surface.id) && children.forall(_.covered(hits))
  
  def uncovered(hits: Set[Int]): SurfaceTree =
    SurfaceTree(surface, children.filter(!_.covered(hits)).map(_.uncovered(hits)))

case class CoverageResults(path: Text, spec: IArray[Surface], oldHits: Set[Int], hits: Set[Int]):
  lazy val structure: Map[Text, List[SurfaceTree]] =
    spec.drop(spec.lastIndexWhere(_.id == 0)).to(List).groupBy(_.path).map: (path, surfaces) =>
      path -> SurfaceTree.collapse(surfaces.sortBy(-_.end).sortBy(_.start), Nil)
    .to(Map)

object Coverage:
  def apply(): Option[CoverageResults] = currentDir.map: dir =>
    val currentFile = Invoker.measurementFile(dir.s)
    val hits = measurements(currentFile)
    val dirFile = File(dir.s)

    if !dirFile.exists() then CoverageResults(dir, IArray(), Set(), Set())
    else
      val otherFiles = Option(dirFile.listFiles).map(_.nn).map(_.to(List).map(_.nn)).getOrElse(Nil)
      val measurementFiles = otherFiles.filter(_.getName.nn.startsWith("scoverage.measurements"))
      
      val allHits: Set[Int] = measurementFiles.flatMap(measurements(_)).to(Set)
      val oldHits: Set[Int] = allHits -- hits
      
      CoverageResults(dir, spec(dir), oldHits, hits)

  private def currentDir: Option[Text] =
    Option(System.getProperty("scalac.coverage")).map(_.nn).map(Text(_))

  private def spec(dir: Text): IArray[Surface] =
    val file = java.io.File(java.io.File(dir.s), "scoverage.coverage")
    val lines = Source.fromFile(file).getLines.to(LazyList).map(Text(_))

    def recur(lines: LazyList[Text], surfaces: List[Surface] = Nil): List[Surface] =
      lines match
        case As[Int](id) #:: path #:: _ #:: _ #:: _ #:: className #:: methodName #::
            As[Int](start) #:: As[Int](end) #:: As[Int](lineNo) #:: symbolName #:: treeName #::
            As[Boolean](branch) #:: _ #:: As[Boolean](ignored) #:: tail =>
          
          val surface = Surface(id, path, className, methodName, start, end, lineNo + 1,
              symbolName, treeName, branch, ignored, tail.takeWhile(!_.starts(t"\f")).to(List))
          
          recur(tail.dropWhile(!_.starts(t"\f")).tail, surface :: surfaces)
        
        case _ =>
          surfaces.reverse
      
    IArray.from(recur(lines.dropWhile(_.starts(t"#"))))
  
  private def measurements(file: File): Set[Int] =
    val ids = BitSet()
    if !file.exists() then Set()
    else Source.fromFile(file).getLines.to(LazyList).foreach: id =>
      ids(id.toInt) = true
    
    ids.to(Set)

