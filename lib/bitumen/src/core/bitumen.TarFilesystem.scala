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
package bitumen

import java.nio.file as jnf

import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import galilei.*
import hieroglyph.*, charEncoders.asciiEncoder
import hypotenuse.*, arithmeticOptions.overflow.unchecked
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import vacuous.*


private[bitumen] object TarFilesystem:
  def entryFor[plane <: Posix: Filesystem]
    ( root: Path on plane, path: Path on plane )
    ( using DereferenceSymlinks, Tactic[IoError], Tactic[TarError] )
  :   Tar.Entry =

    val ref = relativize(root, path)
    val mtime: U32 = (jnf.Files.getLastModifiedTime(path.javaPath).nn.toMillis/1000L).toInt.bits.u32
    val mode: UnixMode = readMode(path.javaPath)
    val (uid, gid) = readOwner(path.javaPath)
    val user = UnixUser(uid)
    val group = UnixGroup(gid)

    path.entry() match
      case galilei.File =>
        val bytes: Array[Byte] = jnf.Files.readAllBytes(path.javaPath).nn
        val data: LazyList[Data] = LazyList(bytes.immutable(using Unsafe))

        Tar.Entry.File(ref, mode, user, group, mtime, data)

      case galilei.Directory =>
        Tar.Entry.Directory(ref, mode, user, group, mtime)

      case galilei.Symlink =>
        val target = jnf.Files.readSymbolicLink(path.javaPath).nn.toString.nn.tt
        Tar.Entry.Symlink(ref, mode, user, group, mtime, target)

      case galilei.Fifo =>
        Tar.Entry.Fifo(ref, mode, user, group, mtime)

      case galilei.CharDevice =>
        val (major, minor) = readDeviceNumbers(path.javaPath)
        Tar.Entry.CharSpecial(ref, mode, user, group, mtime, (major.bits.u32, minor.bits.u32))

      case galilei.BlockDevice =>
        val (major, minor) = readDeviceNumbers(path.javaPath)
        Tar.Entry.BlockSpecial(ref, mode, user, group, mtime, (major.bits.u32, minor.bits.u32))

      case _ =>
        raise(TarError(TarError.Reason.DeviceCreationUnsupported(path.show)))
        Tar.Entry.Fifo(ref, mode, user, group, mtime)

  def applyEntry[plane <: Posix: Filesystem]
    ( root: Path on plane, entry: Tar.Entry )
    ( using CreateNonexistentParents on plane,
            OverwritePreexisting on plane,
            Tactic[IoError],
            Tactic[TarError] )
  :   Unit =

    (entry: @scala.unchecked) match
      case f: Tar.Entry.File =>
        val path = absolutize(root, f.path)
        jnf.Files.createDirectories(path.javaPath.getParent)
        val bytes: Array[Byte] = f.data.flatMap(_.iterator).toArray
        jnf.Files.write(path.javaPath, bytes)
        applyPermissions(path.javaPath, f.mode)
        applyTimestamps(path.javaPath, f.mtime)

      case d: Tar.Entry.Directory =>
        val path = absolutize(root, d.path)
        jnf.Files.createDirectories(path.javaPath)
        applyPermissions(path.javaPath, d.mode)

      case s: Tar.Entry.Symlink =>
        val path = absolutize(root, s.path)
        jnf.Files.createDirectories(path.javaPath.getParent)

        if jnf.Files.exists(path.javaPath, jnf.LinkOption.NOFOLLOW_LINKS) then
          jnf.Files.delete(path.javaPath)

        jnf.Files.createSymbolicLink(path.javaPath, jnf.Path.of(s.target.s))

      case l: Tar.Entry.Link =>
        val path = absolutize(root, l.path)
        val target = absolutize(root, decodePath(l.target))
        jnf.Files.createDirectories(path.javaPath.getParent)
        jnf.Files.createLink(path.javaPath, target.javaPath)

      case _: Tar.Entry.Fifo | _: Tar.Entry.CharSpecial | _: Tar.Entry.BlockSpecial =>
        raise(TarError(TarError.Reason.DeviceCreationUnsupported(entry.entryName)))

      case _: Tar.Entry.Pax | _: Tar.Entry.GnuLong => ()

  private def relativize[plane <: Posix: Filesystem]
    ( root: Path on plane, child: Path on plane )
    ( using Tactic[TarError] )
  :   TarRef =

    val rootText = root.encode.s
    val childText = child.encode.s
    val prefix = if rootText.endsWith("/") then rootText else rootText+"/"

    val relText: Text =
      if childText.startsWith(prefix) then childText.substring(prefix.length).nn.tt
      else childText.tt

    decodePath(relText)

  private def absolutize[plane <: Posix: Filesystem]
    ( root: Path on plane, ref: TarRef )
    ( using Tactic[TarError] )
  :   Path on plane =

    decodeAbsolute(root.encode.s+"/"+ref.show.s, root)

  private def decodeAbsolute[plane <: Posix: Filesystem]
    ( text: String, base: Path on plane )
    ( using Tactic[TarError] )
  :   Path on plane =

    import errorDiagnostics.emptyDiagnostics
    val rel = relativeFromPath(base.encode.s, text)
    base + rel

  private def relativeFromPath(rootText: String, fullText: String)
    ( using Tactic[TarError] )
  :   Relative on Posix =

    import errorDiagnostics.emptyDiagnostics
    val prefix = if rootText.endsWith("/") then rootText else rootText+"/"

    val relText: Text =
      if fullText.startsWith(prefix) then fullText.substring(prefix.length).nn.tt
      else fullText.tt

    mitigate:
      case PathError(_, _) => TarError(TarError.Reason.BadName(relText))

    . protect(relText.decode[Relative on Posix])

  private def decodePath(text: Text)(using Tactic[TarError]): TarRef =
    import errorDiagnostics.emptyDiagnostics

    mitigate:
      case PathError(_, _) => TarError(TarError.Reason.BadName(text))

    . protect(text.decode[Relative on Tar])

  private def readMode(javaPath: jnf.Path)(using Tactic[IoError]): UnixMode =
    try (jnf.Files.getAttribute(javaPath, "unix:mode").nn: Any) match
      case n: Int => UnixMode.from(n & 0xfff)
      case _      => UnixMode()
    catch
      case _: UnsupportedOperationException                => UnixMode()
      case _: jnf.attribute.UserPrincipalNotFoundException => UnixMode()

  private def readOwner(javaPath: jnf.Path)(using Tactic[IoError]): (Int, Int) =
    try
      val uid = (jnf.Files.getAttribute(javaPath, "unix:uid").nn: Any) match
        case n: Int => n
        case _      => 0

      val gid = (jnf.Files.getAttribute(javaPath, "unix:gid").nn: Any) match
        case n: Int => n
        case _      => 0

      (uid, gid)
    catch case _: UnsupportedOperationException => (0, 0)

  private def readDeviceNumbers(javaPath: jnf.Path)(using Tactic[IoError]): (Int, Int) =
    try
      val rdev = (jnf.Files.getAttribute(javaPath, "unix:rdev").nn: Any) match
        case n: Long => n
        case _       => 0L

      val major = ((rdev >> 8) & 0xff).toInt
      val minor = (rdev & 0xff).toInt
      (major, minor)
    catch case _: UnsupportedOperationException => (0, 0)

  private def applyPermissions(javaPath: jnf.Path, mode: UnixMode): Unit =
    try jnf.Files.setAttribute(javaPath, "unix:mode", Integer.valueOf(mode.int & 0xfff))
    catch case _: UnsupportedOperationException => ()

  private def applyTimestamps(javaPath: jnf.Path, mtime: U32): Unit =
    val fileTime = jnf.attribute.FileTime.fromMillis(mtime.long*1000L)
    jnf.Files.setLastModifiedTime(javaPath, fileTime)
