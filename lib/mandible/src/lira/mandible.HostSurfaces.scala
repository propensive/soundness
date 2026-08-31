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
package mandible

import scala.collection.immutable.List as SList
import scala.jdk.CollectionConverters.*

import anticipation.*
import contingency.*
import fulminate.*
import gossamer.*
import reliquary.*
import rudiments.*
import vacuous.*

// Harvesters for host-contract surfaces (`jsig.md` §6): readers that turn the artifacts host
// vendors already publish into the content trees a `.lira` host contract carries.
//
// `CtSym` reads the JDK's own `ct.sym` — the compile-time symbol table `--release` uses —
// which holds the signature surface of every release back to JDK 8, so one modern JDK yields
// the whole `jdk` lineage. `HostArchive` reads a stub jar (`android.jar` per API level, a
// `scalajs-javalib` artifact) into the same shape.
object CtSym:

  // The running JDK's own `ct.sym`, where one exists.
  def location(): Optional[Text] =
    val home = java.lang.System.getProperty("java.home").nn
    val path = java.nio.file.Paths.get(home, "lib", "ct.sym").nn
    if java.nio.file.Files.exists(path) then Text(path.toString) else Unset

  // `ct.sym` entry names begin with a run of release codes — `8`–`9` for JDK 8 and 9, then
  // `A` = 10, `B` = 11, … — naming every release the entry's content is identical in.
  private def decode(char: Char): Optional[Int] =
    if char >= '5' && char <= '9' then char - '0'
    else if char >= 'A' && char <= 'Z' then char - 'A' + 10
    else Unset

  private def code(release: Int): Char =
    if release < 10 then ('0' + release).toChar else ('A' + release - 10).toChar

  // The releases the symbol table carries, ascending.
  def releases(path: Text): List[Int] =
    val zip = java.util.zip.ZipFile(path.s)

    try
      val found = scala.collection.mutable.SortedSet[Int]()

      zip.entries.nn.asScala.foreach: entry =>
        val name = entry.nn.getName.nn
        val slash = name.indexOf('/')

        if slash > 0 then name.substring(0, slash).nn.foreach: char =>
          decode(char).let { release => found += release }

      found.toList.to(List)

    finally zip.close()

  // The signature surface of one release, partitioned by platform module (hosts.md §3,
  // "Granularity"): one (module, content) pair per platform module present in the release,
  // with both the release codes and the module segment stripped from the paths, so the
  // `java.base` contract's tree reads `java/lang/Object.sig`. This is the RECOMMENDED harvest
  // for a modularized platform; `surface` below is the union view.
  def modules(path: Text, release: Int)
  :   List[(Text, List[(TreePath, Data)])] raises Lira.Error =

    val grouped = scala.collection.mutable.LinkedHashMap
        [String, scala.collection.mutable.ListBuffer[(TreePath, Data)]]()

    surface(path, release).each: (tree, data) =>
      val name = tree.text.s
      val slash = name.indexOf('/')

      if slash > 0 then
        val module = name.substring(0, slash).nn
        val inner = name.substring(slash + 1).nn
        grouped.getOrElseUpdate(module, scala.collection.mutable.ListBuffer())
          += ((TreePath(Text(inner)), data))


    List.from:
      grouped.toList.sortBy(_(0)).map: (module, entries) =>
        (Text(module), entries.toList.to(List))

  // The signature surface of one release: every `.sig` entry present in it, with the release
  // codes stripped from the path, so the tree reads `java.base/java/lang/Object.sig`.
  // `module-info.sig` entries are omitted — module descriptors are not consumer surface. A
  // `prefix`, where given, keeps a harvest to the paths beneath it; contracts SHOULD be
  // harvested whole (`jsig.md` §4), and the filter exists for tooling and tests.
  def surface(path: Text, release: Int, prefix: Optional[Text] = Unset)
  :   List[(TreePath, Data)] raises Lira.Error =

    val zip = java.util.zip.ZipFile(path.s)
    val marker = code(release)

    try
      val entries = scala.collection.mutable.ListBuffer[(TreePath, Data)]()

      zip.entries.nn.asScala.foreach: entry =>
        val name = entry.nn.getName.nn
        val slash = name.indexOf('/')

        if slash > 0 && name.substring(0, slash).nn.indexOf(marker.toInt) >= 0 && name.endsWith(".sig")
        && !name.endsWith("module-info.sig")
        then
          val inner = name.substring(slash + 1).nn

          if prefix.let { p => inner.startsWith(p.s) }.or(true) then
            val stream = zip.getInputStream(entry).nn
            val bytes = stream.readAllBytes().nn
            stream.close()
            entries += ((TreePath(Text(inner)), Array.unsafeFrozen(bytes)))

      entries.toList.to(List)

    finally zip.close()

// A directory of carrier files — the `.wit` of a WASI world, the `.d.ts` of a JavaScript
// runtime's builtins — read into the content tree a host contract carries. A single file is
// admitted as a one-entry tree. Entries are sorted by path: a filesystem's traversal order is
// not deterministic, and a harvest of the same bytes must build the same tree.
object HostTree:

  def surface(path: Text, suffix: Text): List[(TreePath, Data)] raises Lira.Error =
    val root = java.nio.file.Paths.get(path.s).nn
    val entries = scala.collection.mutable.ListBuffer[(TreePath, Data)]()

    if java.nio.file.Files.isDirectory(root) then
      val stream = java.nio.file.Files.walk(root).nn

      try
        stream.iterator.nn.asScala.foreach: item =>
          if java.nio.file.Files.isRegularFile(item) && item.toString.endsWith(suffix.s) then
            val relative = root.relativize(item).nn.toString.replace('\\', '/').nn
            val bytes = java.nio.file.Files.readAllBytes(item).nn
            entries += ((TreePath(Text(relative)), Array.unsafeFrozen(bytes)))

      finally stream.close()

    else if java.nio.file.Files.isRegularFile(root) && root.toString.endsWith(suffix.s) then
      val bytes = java.nio.file.Files.readAllBytes(root).nn
      entries += ((TreePath(Text(root.getFileName.nn.toString)), Array.unsafeFrozen(bytes)))

    List.from(entries.toList.sortBy(_(0).text.s))

// A stub jar — `android.jar` for one API level, or any signature-bearing archive — read into
// the content tree a host contract carries.
object HostArchive:

  def surface(path: Text, prefix: Optional[Text] = Unset): List[(TreePath, Data)] raises
      Lira.Error =

    val zip = java.util.zip.ZipFile(path.s)

    try
      val entries = scala.collection.mutable.ListBuffer[(TreePath, Data)]()

      zip.entries.nn.asScala.foreach: entry =>
        val name = entry.nn.getName.nn

        if name.endsWith(".class") && !name.endsWith("module-info.class")
        && prefix.let { p => name.startsWith(p.s) }.or(true)
        then
          val stream = zip.getInputStream(entry).nn
          val bytes = stream.readAllBytes().nn
          stream.close()
          entries += ((TreePath(Text(name)), Array.unsafeFrozen(bytes)))

      entries.toList.to(List)

    finally zip.close()
