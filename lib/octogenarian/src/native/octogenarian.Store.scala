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
import filesystemOptions.createParents
import filesystemOptions.deleteRecursively
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.moveAtomically
import filesystemOptions.overwritePreexisting
import providers.javaBaseProvider
import ambience.systems.javaBaseSystem

// The object database of a repository: loose objects under `objects/xx/`, packfiles under
// `objects/pack/`, and the stores named in `objects/info/alternates`. Objects are immutable and
// content-addressed, so a store never caches a wrong answer; packs are re-listed when a hash is
// not found, since another process may have repacked.
private[octogenarian] object Store:
  import Git.Error.Reason as Reason

  private def loosePath(objects: Path on Local, hash: Git.Hash)(using Tactic[Git.Error])
  :   Path on Local =

    Refstore.descend(objects, List(hash.text.keep(2), hash.text.skip(2)))

  // The packs of each object directory, each loaded once, by the path of the directory. They
  // are listed again only when a hash is in none of them, or one of them has gone; a pack whose
  // index is still there when they are is not loaded again, and one whose index has gone is
  // retired. They are listed by one thread at a time, so that no pack is loaded twice.
  private val loaded: java.util.concurrent.ConcurrentHashMap[Text, List[Packfile]] =
    java.util.concurrent.ConcurrentHashMap()

  private def packs(objects: Path on Local, relist: Boolean = false)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Packfile] =

    val key = objects.encode
    val cached = loaded.get(key)

    if cached != null && !relist then cached else loaded.synchronized:
      val current = loaded.get(key)
      val known: List[Packfile] = if current == null then Nil else current
      val directory = objects / "pack"

      val listed: List[Packfile] =
        if !directory.existent() then Nil else
          def recur(paths: List[Path on Local], acc: List[Packfile]): List[Packfile] =
            paths match
              case path :: rest =>
                if path.name.ends(t".idx") then
                  val pack = Refstore.descend(directory, List(t"${path.name.skip(4, Rtl)}.pack"))
                  val same = known.find(_.pack.encode == pack.encode)

                  recur
                    ( rest,
                      same.or(Packfile(pack, Packfile.loadIndex(path.read[Data]))) :: acc )

                else recur(rest, acc)

              case _ => acc.reverse

          recur(directory.children.to[List], Nil)

      // A pack which is no longer listed is read no more, and gives up its mapping.
      known.each: pack =>
        if !listed.exists(_ eq pack) then pack.retire()

      loaded.put(key, listed)
      listed

  private def alternates(objects: Path on Local)(using Tactic[Io.Error], Tactic[Git.Error])
  :   List[Path on Local] =

    val file = objects / "info" / "alternates"

    if !file.existent() then Nil else
      def recur(lines: List[Text], acc: List[Path on Local]): List[Path on Local] = lines match
        case line :: rest =>
          if line.length == 0 then recur(rest, acc) else
            // An alternates line is an absolute path, or one relative to the object directory.
            val path: Path on Local = Refstore.resolve(objects, line)

            recur(rest, path :: acc)

        case _ => acc.reverse

      recur(Objects.text(file.read[Data]).cut(t"\n"), Nil)

  def exists(objects: Path on Local, hash: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Boolean =

    read(objects, hash).present

  // An object which must exist.
  def readOr(objects: Path on Local, hash: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   (Git.Object.Kind, Data) =

    read(objects, hash) match
      case found: (Git.Object.Kind, Data) @unchecked => found

      case _ =>
        abort(Git.Error(Reason.NoSuchObject(hash.text)))

  // Reads an object: its kind and its content, without the `type size\0` header.
  def read(objects: Path on Local, hash: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   Optional[(Git.Object.Kind, Data)] =

    val loose = loosePath(objects, hash)

    if loose.existent() then parseLoose(loose.read[Data])
    else
      def fromPacks(remaining: List[Packfile]): Optional[(Git.Object.Kind, Data)] = remaining match
        case pack :: rest =>
          if pack.contains(hash) then pack.read(hash, read(objects, _)) else fromPacks(rest)

        case _ => Unset

      def fromAlternates(remaining: List[Path on Local]): Optional[(Git.Object.Kind, Data)] =
        remaining match
          case store :: rest => read(store, hash).or(fromAlternates(rest))
          case _             => Unset

      // A pack which has gone since the packs were listed means they are listed again.
      def present(packs: List[Packfile]): Optional[(Git.Object.Kind, Data)] =
        if packs.exists { pack => pack.contains(hash) && !pack.pack.existent() } then Unset
        else fromPacks(packs)

      present(packs(objects))
      . or(fromPacks(packs(objects, relist = true)))
      . or(fromAlternates(alternates(objects)))

  // Writes a loose object, unless it is already present: written to a temporary name, then moved
  // into place, so a concurrent reader never sees a partial object.
  def write
    ( objects: Path on Local, kind: Git.Object.Kind, data: Data )
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
  def all(objects: Path on Local)(using Tactic[Git.Error], Tactic[Io.Error]): List[Git.Hash] =
    def hashesIn(directory: Path on Local): List[Git.Hash] =
      if directory.name.length != 2 then Nil else
        def recur(files: List[Path on Local], acc: List[Git.Hash]): List[Git.Hash] = files match
          case file :: rest =>
            if file.name.length == 38
            then recur(rest, Git.Hash.unsafe(t"${directory.name}${file.name}") :: acc)
            else recur(rest, acc)

          case _ => acc.reverse

        recur(directory.children.to[List], Nil)

    val loose = objects.children.to[List].bind(hashesIn(_))

    loose + packs(objects, relist = true).bind(_.index.entries.map(_(0)))

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
