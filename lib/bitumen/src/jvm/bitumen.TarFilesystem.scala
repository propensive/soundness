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
package bitumen


import java.nio.file as jnf

import scala.collection.mutable as scm

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import hieroglyph.*, codepages.asciiCodepage
import hypotenuse.*, arithmeticOptions.uncheckedOverflow
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*

import filesystemBackends.javaBaseFilesystem

// Writes a `Tarfile` out to a directory tree: the inverse of `directory.archive[Tar]()`, still
// through `java.nio`, since setting a mode has no backend primitive yet.
private[bitumen] object TarFilesystem:
  // `created` holds the directories this extraction has made so far, so that the parent of each
  // entry is created only once: `createDirectories` stats every component of the chain, which a
  // tarball of many files in one directory would otherwise repeat for every file. The set must be
  // exact, since a skipped `mkdir` would fail the write that follows, and it must not outlive the
  // extraction, since the disk may change between runs.
  def applyEntry[plane <: Posix: Filesystem]
    ( root: Path on plane, entry: Tar.Entry, created: scm.HashSet[jnf.Path] )
    ( using CreateNonexistentParents on plane,
            OverwritePreexisting on plane,
            Tactic[Io.Error],
            Tactic[Tar.Error] )
  :   Unit =

    (entry: @scala.unchecked) match
      case f: Tar.Entry.File =>
        val path = absolutize(root, f.path)
        createParent(path.javaPath, created)
        val bytes: scala.Array[Byte] = Array.unsafeJvm(f.data.memoize)
        jnf.Files.write(path.javaPath, bytes)
        applyPermissions(path.javaPath, f.mode)
        applyTimestamps(path.javaPath, f.mtime)

      case d: Tar.Entry.Directory =>
        val path = absolutize(root, d.path)
        jnf.Files.createDirectories(path.javaPath)
        created += path.javaPath
        applyPermissions(path.javaPath, d.mode)

      case s: Tar.Entry.Symlink =>
        val path = absolutize(root, s.path)
        createParent(path.javaPath, created)

        if jnf.Files.exists(path.javaPath, jnf.LinkOption.NOFOLLOW_LINKS) then
          jnf.Files.delete(path.javaPath)

        jnf.Files.createSymbolicLink(path.javaPath, jnf.Path.of(s.target.s))

      case l: Tar.Entry.Link =>
        val path = absolutize(root, l.path)
        val target = absolutize(root, decodePath(l.target))
        createParent(path.javaPath, created)
        jnf.Files.createLink(path.javaPath, target.javaPath)

      case _: Tar.Entry.Fifo | _: Tar.Entry.CharSpecial | _: Tar.Entry.BlockSpecial =>
        raise(Tar.Error(Tar.Error.Reason.DeviceCreationUnsupported(entry.entryName)))

      case _: Tar.Entry.Pax | _: Tar.Entry.GnuLong => ()

  private def createParent(path: jnf.Path, created: scm.HashSet[jnf.Path]): Unit =
    val parent = path.getParent.nn

    if !created.contains(parent) then
      jnf.Files.createDirectories(parent)
      created += parent

  private def absolutize[plane <: Posix: Filesystem]
    ( root: Path on plane, ref: Tar.Ref )
    ( using Tactic[Tar.Error] )
  :   Path on plane =

    decodeAbsolute(root.encode.s+"/"+ref.show.s, root)

  private def decodeAbsolute[plane <: Posix: Filesystem]
    ( text: String, base: Path on plane )
    ( using Tactic[Tar.Error] )
  :   Path on plane =

    import errorDiagnostics.emptyDiagnostics
    val rel = relativeFromPath(base.encode.s, text)
    base + rel

  private def relativeFromPath(rootText: String, fullText: String)
    ( using Tactic[Tar.Error] )
  :   Relative on Posix =

    import errorDiagnostics.emptyDiagnostics
    val prefix = if rootText.endsWith("/") then rootText else rootText+"/"

    val relText: Text =
      if fullText.startsWith(prefix) then fullText.substring(prefix.length).nn.tt else fullText.tt

    mitigate:
      case Path.Error(_, _) => Tar.Error(Tar.Error.Reason.BadName(relText))

    . protect(relText.as[Relative on Posix])

  private def decodePath(text: Text)(using Tactic[Tar.Error]): Tar.Ref =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case Path.Error(_, _) => Tar.Error(Tar.Error.Reason.BadName(text))

    . protect(text.as[Relative on Tar])

  private def applyPermissions(javaPath: jnf.Path, mode: UnixMode): Unit =
    try jnf.Files.setAttribute(javaPath, "unix:mode", Integer.valueOf(mode.int & 0xfff))
    catch case _: UnsupportedOperationException => ()

  private def applyTimestamps(javaPath: jnf.Path, mtime: U32): Unit =
    val fileTime = jnf.attribute.FileTime.fromMillis(mtime.long*1000L)
    jnf.Files.setLastModifiedTime(javaPath, fileTime)
