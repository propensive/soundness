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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package beneficence

import java.io.{BufferedReader, BufferedWriter, File, FileInputStream, FileOutputStream,
    InputStreamReader, OutputStreamWriter}
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, StandardCopyOption}

import scala.collection.mutable

import dotty.tools.dotc.core.Contexts.*

case class Entry(fqn: String, sourceFile: String)

object GivensWriter:
  private val SourcePrefix = "# source: "

  def merge
    ( collected:         mutable.LinkedHashMap[String, mutable.Buffer[Entry]],
      recompiledSources: Set[String] )
    ( using Context )
  :     Unit =

    val outputRoot = outputRootOrNull
    if outputRoot == null then return  // JAR output: skip; future work to support

    val givensDir = new File(new File(outputRoot, "META-INF"), "givens")
    if !givensDir.exists then givensDir.mkdirs(): @annotation.nowarn

    collected.foreach: (typeclassFqn, entries) =>
      mergeOne(new File(givensDir, typeclassFqn), entries, recompiledSources)

  def mergeSuites
    ( collected:         mutable.Buffer[Entry],
      recompiledSources: Set[String] )
    ( using Context )
  :     Unit =

    // Mirrors the givens path: when a compilation registers no entries we
    // skip rewriting altogether, leaving any prior file untouched.
    if collected.isEmpty then return

    val outputRoot = outputRootOrNull
    if outputRoot == null then return

    val servicesDir = new File(new File(outputRoot, "META-INF"), "services")
    if !servicesDir.exists then servicesDir.mkdirs(): @annotation.nowarn

    mergeOne(new File(servicesDir, "probably.Suite"), collected, recompiledSources)

  private def outputRootOrNull(using Context): File | Null =
    ctx.settings.outputDir.value.file

  private def mergeOne
    ( target:            File,
      entries:           collection.Seq[Entry],
      recompiledSources: Set[String] )
  :     Unit =

    val current = if target.exists then read(target) else Map.empty[String, List[String]]
    val pruned  = current.filterNot((source, _) => recompiledSources.contains(source))

    val merged: List[(String, List[String])] =
      val byNew: Map[String, List[String]] =
        entries.groupBy(_.sourceFile).view.mapValues(_.map(_.fqn).distinct.toList).toMap

      val keptOrder: List[String] = pruned.keys.toList
      val newOrder: List[String]  = byNew.keys.toList.filterNot(pruned.contains)

      (keptOrder ++ newOrder).map: source =>
        source -> pruned.getOrElse(source, byNew.getOrElse(source, Nil))

    write(target, merged)

  private def read(file: File): Map[String, List[String]] =
    val builder = mutable.LinkedHashMap.empty[String, mutable.ListBuffer[String]]
    var currentSource: String | Null = null

    val reader = new BufferedReader
      ( new InputStreamReader
             ( new FileInputStream(file), StandardCharsets.UTF_8 ) )
    try
      var line: String | Null = reader.readLine()
      while line != null do
        val l: String = line.trim.nn
        if l.startsWith(SourcePrefix) then
          val source = l.substring(SourcePrefix.length).nn.trim.nn
          currentSource = source
          builder.getOrElseUpdate(source, mutable.ListBuffer())
        else if l.isEmpty || l.startsWith("#") then
          ()
        else
          val src = currentSource
          if src != null then builder.getOrElseUpdate(src, mutable.ListBuffer()) += l
        line = reader.readLine()
    finally reader.close()

    builder.view.mapValues(_.toList).toMap

  private def write(file: File, blocks: List[(String, List[String])]): Unit =
    val parent = file.getParentFile
    if parent != null && !parent.exists then parent.mkdirs(): @annotation.nowarn

    val tmp = new File(file.getParentFile, file.getName.nn + ".tmp")
    val writer = new BufferedWriter
      ( new OutputStreamWriter
             ( new FileOutputStream(tmp), StandardCharsets.UTF_8 ) )
    try
      var first = true
      blocks.foreach: (source, givens) =>
        if givens.nonEmpty then
          if !first then writer.newLine()
          writer.write(SourcePrefix + source)
          writer.newLine()
          givens.foreach: fqn =>
            writer.write(fqn)
            writer.newLine()
          first = false
    finally writer.close()

    Files.move
     ( tmp.toPath,
       file.toPath,
       StandardCopyOption.REPLACE_EXISTING,
       StandardCopyOption.ATOMIC_MOVE )
    ()
