                                                                                                  /*
┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓
┃                                                                                                  ┃
┃                                                   ╭───╮                                          ┃
┃                                                   │   │                                          ┃
┃                                                   │   │                                          ┃
┃   ╭───────╮╭─────────╮╭───╮ ╭───╮╭───╮╌────╮╭────╌┤   │╭───╮╌────╮╭────────╮╭───────╮╭───────╮   ┃
┃   │   ╭───╯│   ╭─╮   ││   │ │   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮   ││   ╭─╮  ││   ╭───╯│   ╭───╯   ┃
┃   │   ╰───╮│   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╰─╯  ││   ╰───╮│   ╰───╮   ┃
┃   ╰───╮   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   │ │   ││   ╭────╯╰───╮   │╰───╮   │   ┃
┃   ╭───╯   ││   ╰─╯   ││   ╰─╯   ││   │ │   ││   ╰─╯   ││   │ │   ││   ╰────╮╭───╯   │╭───╯   │   ┃
┃   ╰───────╯╰─────────╯╰────╌╰───╯╰───╯ ╰───╯╰────╌╰───╯╰───╯ ╰───╯╰────────╯╰───────╯╰───────╯   ┃
┃                                                                                                  ┃
┃    Soundness, version 0.64.0.                                                                    ┃
┃    © Copyright 2021-25 Jon Pretty, Propensive OÜ.                                                ┃
┃                                                                                                  ┃
┃    The primary distribution site is:                                                             ┃
┃                                                                                                  ┃
┃        https://soundness.dev/                                                                    ┃
┃                                                                                                  ┃
┃    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file     ┃
┃    except in compliance with the License. You may obtain a copy of the License at                ┃
┃                                                                                                  ┃
┃        https://www.apache.org/licenses/LICENSE-2.0                                               ┃
┃                                                                                                  ┃
┃    Unless required by applicable law or agreed to in writing,  software distributed under the    ┃
┃    License is distributed on an "AS IS" BASIS,  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,    ┃
┃    either express or implied. See the License for the specific language governing permissions    ┃
┃    and limitations under the License.                                                            ┃
┃                                                                                                  ┃
┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛
                                                                                                  */
package probably

import java.io.*

import scala.collection.mutable.BitSet
import scala.io.*

import anticipation.*
import denominative.dysasymptotics.linearSize
import distillate.*
import gossamer.*
import rudiments.*
import vacuous.*

object Coverage:
  def apply(): Option[Coverage] = currentDir.map: dir =>
    val currentFile = measurementFileIn(dir)
    val hits = measurements(currentFile)
    val dirFile = File(dir.s)

    if !dirFile.exists() then Coverage(dir, Array(), Set(), Set())
    else
      // `listFiles` hands back a mutable array, which captures the root capability.
      // Wrapping it in an `Option` instantiates that type variable with a capturing
      // type, which capture checking refuses; consume the array where it is produced
      // and let only the immutable list escape.
      val listed = dirFile.listFiles
      val otherFiles =
        if listed == null then Nil.stdlib else listed.nn.iterator.map(_.nn).toList

      val measurementFiles = otherFiles.filter(_.getName.nn.startsWith("scoverage.measurements"))

      val allHits: Set[Int] = Set.from(measurementFiles.iterator.flatMap(measurements(_).stdlib))
      val oldHits: Set[Int] = Set.of(allHits.stdlib -- hits.stdlib)

      Coverage(dir, spec(dir), oldHits, hits)

  private def currentDir: Option[Text] =
    Option(System.getProperty("scalac.coverage")).map(_.nn).map(Text(_))

  private def spec(dir: Text): Array[Juncture]^{} =
    val file = java.io.File(java.io.File(dir.s), "scoverage.coverage")
    val lines = Source.fromFile(file).getLines().to(Chain).map(Text(_))

    def recur(lines: Chain[Text], junctures: List[Juncture] = Nil): List[Juncture] =
      lines match
        case
          ( As.Int(id) #:: path #:: _ #:: _ #:: _ #:: className #:: methodName #::
            As.Int(start) #:: As.Int(end) #:: As.Int(lineNo) #:: symbolName #:: treeName #::
            As.Boolean(branch) #:: _ #:: As.Boolean(ignored) #:: tail ) =>

          val juncture = Juncture(id, path, className, methodName, start, end, lineNo + 1,
              symbolName, treeName, branch, ignored, List.from(tail.stdlib.takeWhile(!_.starts(t"\f"))))

          recur(Chain.of(tail.stdlib.dropWhile(!_.starts(t"\f")).tail), juncture :: junctures)

        case _ =>
          junctures.reverse

    Array.from(recur(Chain.of(lines.stdlib.dropWhile(_.starts(t"#")))).stdlib)

  private def measurements(file: File): Set[Int] =
    val ids = BitSet()

    if !file.exists() then Set()
    else Source.fromFile(file).getLines().to(Chain).each: id =>
      ids(id.toInt) = true

    Set.from(ids)

case class Coverage(path: Text, spec: Array[Juncture]^{}, oldHits: Set[Int], hits: Set[Int]):
  lazy val structure: Map[Text, List[Surface]] =
    val index: Int = spec.readable.lastIndexWhere(_.id == 0)

    spec.to[List].skip(index).group(_.path).map: junctures =>
      Surface.collapse(junctures.sort(-_.end).sort(_.start), Nil)
