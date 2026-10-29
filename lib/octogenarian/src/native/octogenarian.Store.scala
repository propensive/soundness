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
package octogenarian

import anticipation.*
import contingency.*
import distillate.*
import galilei.*
import gastronomy.*
import gossamer.*
import monotonous.*
import pneumatic.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import alphabets.hexLowerCase
import cryptoPermits.permitDeprecatedCrypto
import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.moveAtomically
import filesystemOptions.overwritePreexisting
import providers.javaBaseProvider

// The object database of a repository: loose objects under `objects/xx/`, packfiles under
// `objects/pack/`, and the stores named in `objects/info/alternates`. Objects are immutable and
// content-addressed, so a store never caches a wrong answer; packs are re-listed when a hash is
// not found, since another process may have repacked.
private[octogenarian] object Store:
  import Git.Error.Reason as Reason

  private def loosePath(objects: Path on Linux, hash: Git.Hash): Path on Linux =
    Refstore.descend(objects, List(hash.text.keep(2), hash.text.skip(2)))

  private def packs(objects: Path on Linux)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Packfile] =

    val directory = objects / "pack"

    if !directory.existent() then Nil else
      def recur(paths: List[Path on Linux], acc: List[Packfile]): List[Packfile] = paths match
        case path :: rest =>
          if path.name.ends(t".idx") then
            val pack = Refstore.descend(directory, List(t"${path.name.skip(4, Rtl)}.pack"))
            recur(rest, Packfile(pack, Packfile.loadIndex(path.read[Data])) :: acc)
          else
            recur(rest, acc)

        case _ => acc.reverse

      recur(directory.children.to[List], Nil)

  private def alternates(objects: Path on Linux)(using Tactic[Io.Error]): List[Path on Linux] =
    val file = objects / "info" / "alternates"

    if !file.existent() then Nil else
      def recur(lines: List[Text], acc: List[Path on Linux]): List[Path on Linux] = lines match
        case line :: rest =>
          if line.length == 0 then recur(rest, acc) else
            val target = if line.starts(t"/") then line else t"${objects.encode}/$line"
            recur(rest, unsafely(target.as[Path on Linux]) :: acc)

        case _ => acc.reverse

      recur(Objects.text(file.read[Data]).cut(t"\n"), Nil)

  def exists(objects: Path on Linux, hash: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Boolean =

    read(objects, hash).present

  // An object which must exist.
  def readOr(objects: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   (Git.Object.Kind, Data) =

    read(objects, hash) match
      case found: (Git.Object.Kind, Data) @unchecked => found

      case _ =>
        abort(Git.Error(Reason.NoSuchObject(hash.text)))

  // Reads an object: its kind and its content, without the `type size\0` header.
  def read(objects: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   Optional[(Git.Object.Kind, Data)] =

    val loose = loosePath(objects, hash)

    if loose.existent() then parseLoose(loose.read[Data])
    else
      def fromPacks(remaining: List[Packfile]): Optional[(Git.Object.Kind, Data)] = remaining match
        case pack :: rest =>
          if pack.contains(hash) then pack.read(hash, read(objects, _)) else fromPacks(rest)

        case _ => Unset

      def fromAlternates(remaining: List[Path on Linux]): Optional[(Git.Object.Kind, Data)] =
        remaining match
          case store :: rest => read(store, hash).or(fromAlternates(rest))
          case _             => Unset

      fromPacks(packs(objects)).or(fromAlternates(alternates(objects)))

  // Writes a loose object, unless it is already present: written to a temporary name, then moved
  // into place, so a concurrent reader never sees a partial object.
  def write
    ( objects: Path on Linux, kind: Git.Object.Kind, data: Data )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    val hash = Store.hash(kind, data)
    val target = loosePath(objects, hash)

    if !target.existent() then
      val header = Objects.bytes(t"${kind.text} ${data.length}\u0000")
      val compressed = concatenate(header, data).compress[Zlib]
      val directory = Refstore.descend(objects, List(hash.text.keep(2)))
      val suffix = java.util.UUID.randomUUID.nn.toString.tt
      val temporary = Refstore.descend(directory, List(t"tmp_obj_$suffix"))
      Refstore.ensureDirectory(directory)
      temporary.write(compressed)
      if !target.existent() then temporary.moveTo(target) else temporary.delete()

    hash

  // Every hash the store has an object for: loose and packed.
  def all(objects: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error]): List[Git.Hash] =
    def hashesIn(directory: Path on Linux): List[Git.Hash] =
      if directory.name.length != 2 then Nil else
        def recur(files: List[Path on Linux], acc: List[Git.Hash]): List[Git.Hash] = files match
          case file :: rest =>
            if file.name.length == 38
            then recur(rest, Git.Hash.unsafe(t"${directory.name}${file.name}") :: acc)
            else recur(rest, acc)

          case _ => acc.reverse

        recur(directory.children.to[List], Nil)

    val loose = objects.children.to[List].bind(hashesIn(_))

    loose + packs(objects).bind(_.index.entries.map(_(0)))

  def hash(kind: Git.Object.Kind, data: Data): Git.Hash =
    val header = Objects.bytes(t"${kind.text} ${data.length}\u0000")
    Git.Hash.unsafe(concatenate(header, data).digest[Sha1].serialize[Hex])

  def concatenate(left: Data, right: Data): Data =
    val result = new scala.Array[Byte](left.length + right.length)
    System.arraycopy(left.asInstanceOf[scala.Array[Byte]], 0, result, 0, left.length)
    System.arraycopy(right.asInstanceOf[scala.Array[Byte]], 0, result, left.length, right.length)
    result.asInstanceOf[Data]

  // A loose object: zlib-compressed `type size\0content`.
  def parseLoose(compressed: Data): Optional[(Git.Object.Kind, Data)] =
    val whole = compressed.decompress[Zlib].asInstanceOf[scala.Array[Byte]]

    def nul(index: Int): Int =
      if index >= whole.length || whole(index) == 0 then index else nul(index + 1)

    val boundary = nul(0)
    val header = String(whole, 0, boundary, "UTF-8").nn.tt

    header.cut(t" ") match
      case List(kind, _) =>
        Git.Object.Kind.parse(kind).let: kind =>
          val content = java.util.Arrays.copyOfRange(whole, boundary + 1, whole.length).nn
          (kind, content.asInstanceOf[Data])

      case _ => Unset
