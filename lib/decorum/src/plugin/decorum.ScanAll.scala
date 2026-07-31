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
package decorum

import java.nio.file.{Files, Path as JPath}

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

object ScanAll:
  def main(args: scala.Array[String]): Unit =
    val libRoot     = JPath.of(args.head)
    val ruleFilter  = if args.length > 1 then Some(args(1)) else None
    val all         = mutable.ArrayBuffer[Violation]()

    val files = Files.walk(libRoot).nn.iterator.nn.asScala.filter: path =>
      val s = path.toString
      s.endsWith(".scala") && s.contains("/src/") && !s.contains("/src/test")

    . toList

    // Mechanical-fix mode for SN-247: emit, for every flagged site, the raw
    // byte region `[start, end)` and the one-line rendering the checker
    // measured — a driver substitutes exactly that string, so the joined
    // form is byte-for-byte what was width-checked.
    if ruleFilter == Some("--fix-247") then
      files.foreach: path =>
        val text           = Files.readString(path).nn
        val (tree, source) = Parsing.parse(path.toString, text)

        Necessity.extract(tree, source, text).foreach: site =>
          println(s"${path}\t${site.start}\t${site.end}\t${site.rendering}")

      return

    files.foreach: path =>
      val s    = path.toString
      val text = Files.readString(path).nn
      Checker.check(s, Checker.expectedModule(s), text).foreach(all += _)

    val filtered = ruleFilter match
      case Some(r) => all.filter(_.rule == r).toList
      case None    => all.toList

    filtered.foreach: v =>
      val short = v.file.split("/lib/").nn.map(_.nn) match
        case parts if parts.length >= 2 => "lib/"+parts(1)
        case _                          => v.file

      println(s"${short}:${v.line}:${v.column}  [${v.rule}] ${v.message}")
