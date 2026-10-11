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
import aperture.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import sortingAlgorithms.timsort

import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.overwritePreexisting
import filesystemOptions.moveAtomically
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.deleteRecursively

// The refs of a repository: loose files under `refs/`, the `packed-refs` file, and the pseudo-refs
// (`HEAD`, `ORIG_HEAD`, …) at the top of the git directory. A ref is written by taking its
// `.lock` file with an exclusive create, re-reading it under the lock, comparing with what the
// caller expected, and renaming the lock into place: git's own compare-and-swap, which every
// other git process honours.
object Refstore:
  import Git.Error.Reason as Reason

  private val refsPrefix: Text = t"refs/"
  private val symbolicPrefix: Text = t"ref: "

  // Pseudo-refs belong to the worktree's own git directory; `refs/**` to the common one.
  // The element at a position in a list, without a complexity gate: the lists here are short.
  def nth[element](list: List[element], index: Int): Optional[element] = list match
    case head :: tail => if index == 0 then head else nth(tail, index - 1)
    case _            => Unset

  // A path descended segment by segment, without a lambda: `/` is transparent inline.
  // Creates a directory and whichever of its ancestors are missing.
  def ensureDirectory(directory: Path on Linux)(using Tactic[Io.Error]): Unit =
    if !directory.existent() then
      directory.parent.let(ensureDirectory(_))
      directory.create[Directory]()

  def descend(base: Path on Linux, segments: List[Text]): Path on Linux =
    if segments == Nil then base
    else unsafely(t"${base.encode}/${segments.join(t"/")}".as[Path on Linux])

  def location(gitDir: Path on Linux, commonDir: Path on Linux, name: Text): Path on Linux =
    val base = if name.starts(refsPrefix) then commonDir else gitDir
    descend(base, name.cut(t"/"))

  // A ref file holds `ref: <name>` or a hash; `FETCH_HEAD` and `MERGE_HEAD` hold one hash per
  // line, the first with a tab-separated description after it.
  private def parseTarget(content: Text): Optional[Git.Backend.Target] =
    val line = content.trim.cut(t"\n").prim.or(t"")
    if line.starts(symbolicPrefix) then Git.Backend.Target.Symbolic(line.skip(5).trim)
    else if line.length == 40 then Git.Backend.Target.Direct(Git.Hash.unsafe(line))
    else if line.length > 40 && (line.s.charAt(40) == '\t' || line.s.charAt(40) == ' ')
    then Git.Backend.Target.Direct(Git.Hash.unsafe(line.keep(40)))
    else Unset

  // The `packed-refs` file: `<hash> <name>` lines, with `^<hash>` lines giving the peeled commit
  // of the tag before them, and a `#` header.
  def packed(commonDir: Path on Linux)(using Tactic[Io.Error]): List[(Text, Git.Hash)] =
    val file = commonDir / "packed-refs"

    if !file.existent() then Nil else
      Objects.text(file.read[Data]).cut(t"\n").sweep:
        case line if line.length > 41 && line.s.charAt(40) == ' ' && line.s.charAt(0) != '#' =>
          (line.skip(41), Git.Hash.unsafe(line.keep(40)))

  def read(gitDir: Path on Linux, commonDir: Path on Linux, name: Text)(using Tactic[Io.Error])
  :   Optional[Git.Backend.Target] =

    val file = location(gitDir, commonDir, name)

    if file.existent() then parseTarget(Objects.text(file.read[Data]))
    else packed(commonDir).reap { case (`name`, hash) => Git.Backend.Target.Direct(hash) }

  // Follows symbolic refs to the object they ultimately name.
  def resolve(gitDir: Path on Linux, commonDir: Path on Linux, name: Text)(using Tactic[Io.Error])
  :   Optional[Git.Hash] =

    def recur(name: Text, depth: Int): Optional[Git.Hash] =
      if depth > 10 then Unset else read(gitDir, commonDir, name) match
        case Git.Backend.Target.Direct(hash)  => hash
        case Git.Backend.Target.Symbolic(ref) => recur(ref, depth + 1)
        case _                                => Unset

    recur(name, 0)

  // The name a symbolic ref chain ends at: the ref an update to `name` really writes.
  def dereference(gitDir: Path on Linux, commonDir: Path on Linux, name: Text)(using Tactic[Io.Error]): Text =
    def recur(name: Text, depth: Int): Text =
      if depth > 10 then name else read(gitDir, commonDir, name) match
        case Git.Backend.Target.Symbolic(ref) => recur(ref, depth + 1)
        case _                                => name

    recur(name, 0)

  // Every ref under `prefix`, loose refs shadowing packed ones, sorted by name.
  def list(commonDir: Path on Linux, prefix: Optional[Text])(using Tactic[Io.Error]): List[(Text, Git.Hash)] =
    def loose(directory: Path on Linux, name: Text): List[(Text, Git.Hash)] =
      if !directory.existent() then Nil else
        def recur(children: List[Path on Linux], acc: List[(Text, Git.Hash)]): List[(Text, Git.Hash)] =
          children match
            case child :: rest =>
              val childName = t"$name/${child.name}"
              val entry = unsafely(child.entry())

              val found =
                if entry == Directory then loose(child, childName)
                else if child.name.ends(t".lock") then Nil
                else parseTarget(Objects.text(child.read[Data])) match
                  case Git.Backend.Target.Direct(hash) => List((childName, hash))
                  case _                               => Nil

              recur(rest, acc + found)

            case _ => acc

        recur(directory.children.to[List], Nil)

    val looseRefs = loose(commonDir / "refs", t"refs")
    val looseNames = looseRefs.map(_(0)).to[Set]
    val packedRefs = packed(commonDir).filter { (name, _) => !looseNames.has(name) }
    val all = looseRefs + packedRefs
    val filtered = prefix.lay(all) { prefix => all.filter(_(0).starts(prefix)) }
    filtered.order(_(0).s)

  // Takes `<file>.lock` with an exclusive create. A lock already held is reported as `Locked`.
  def lock(file: Path on Linux)(block: Path on Linux => Unit)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val parent: Path on Linux = file.parent.or(abort(Git.Error(Reason.Unexpected)))
    ensureDirectory(parent)
    val lockFile: Path on Linux = unsafely(t"${file.encode}.lock".as[Path on Linux])

    try lockFile.open[File](Write, OpenFlag.Create, OpenFlag.Exclusive) { handle ?=> () }
    catch case error: Io.Error => abort(Git.Error(Reason.Locked(file.encode)))

    try block(lockFile)
    finally if lockFile.existent() then lockFile.delete()

  // Compare-and-swap: writes `to` into `name` (or deletes it) if it currently holds `expected`.
  def update
    ( gitDir:   Path on Linux,
      commonDir: Path on Linux,
      name:     Text,
      to:       Optional[Git.Hash],
      expected: Git.Backend.Expectation,
      message:  Optional[Text],
      identity: Git.Identity,
      logging:  Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val target = dereference(gitDir, commonDir, name)
    val file = location(gitDir, commonDir, target)

    lock(file): lockFile =>
      val current = resolve(gitDir, commonDir, target)

      expected match
        case Git.Backend.Expectation.Unconditional => ()

        // Reported under the name the caller used (`HEAD`, say), as git does, not its target.
        case Git.Backend.Expectation.Absent =>
          if current.present then abort(Git.Error(Reason.Moved(name, Unset, current)))

        case Git.Backend.Expectation.Exactly(hash) =>
          if current != Optional(hash) then abort(Git.Error(Reason.Moved(name, hash, current)))

      to match
        case hash: Git.Hash =>
          lockFile.write(Objects.bytes(t"${hash.text}\n"))
          lockFile.moveTo(file)

        case _ =>
          if file.existent() then file.delete()
          unpack(commonDir, target)

      if logging then
        log(gitDir, commonDir, target, current, to, identity, message)
        if target != name then log(gitDir, commonDir, name, current, to, identity, message)

  // Rewrites `packed-refs` without `name`, under its own lock.
  private def unpack(commonDir: Path on Linux, name: Text)(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    val file = commonDir / "packed-refs"

    if file.existent() && packed(commonDir).exists(_(0) == name) then
      lock(file): lockFile =>
        val kept = Objects.text(file.read[Data]).cut(t"\n").filter: line =>
          !(line.length > 41 && line.skip(41) == name) && line.length > 0

        lockFile.write(Objects.bytes(t"${kept.join(t"\n")}\n"))
        lockFile.moveTo(file)

  // Detaches HEAD at a commit: written directly, where `update` would follow HEAD to its branch.
  def detach
    ( gitDir:    Path on Linux,
      commonDir: Path on Linux,
      commit:    Git.Hash,
      previous:  Optional[Git.Hash],
      identity:  Git.Identity,
      message:   Text,
      logging:   Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val file = location(gitDir, commonDir, t"HEAD")

    lock(file): lockFile =>
      lockFile.write(Objects.bytes(t"${commit.text}\n"))
      lockFile.moveTo(file)

    if logging then log(gitDir, commonDir, t"HEAD", previous, commit, identity, message)

  def writeSymbolic(gitDir: Path on Linux, commonDir: Path on Linux, name: Text, target: Text)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val file = location(gitDir, commonDir, name)

    lock(file): lockFile =>
      lockFile.write(Objects.bytes(t"ref: $target\n"))
      lockFile.moveTo(file)

  // Appends a reflog line: `<old> <new> <ident> <timestamp>\t<message>`.
  private def log
    ( gitDir:   Path on Linux,
      commonDir: Path on Linux,
      name:     Text,
      from:     Optional[Git.Hash],
      to:       Optional[Git.Hash],
      identity: Git.Identity,
      message:  Optional[Text] )
    ( using Tactic[Io.Error] )
  :   Unit =

    val base = if name.starts(refsPrefix) then commonDir else gitDir
    val file = descend(base, t"logs" :: name.cut(t"/"))
    val parent: Path on Linux = file.parent.or(base)
    ensureDirectory(parent)
    val signature = Git.Signature(identity, Git.Timestamp.now())
    val old = from.lay(Git.Hash.zero.text)(_.text)
    val fresh = to.lay(Git.Hash.zero.text)(_.text)
    val line = Objects.bytes(t"$old $fresh ${signature.text}\t${message.or(t"")}\n")
    val existing = if file.existent() then file.read[Data] else Data()
    file.write(Store.concatenate(existing, line))

  // The reflog of `name`, newest first.
  def reflog(gitDir: Path on Linux, commonDir: Path on Linux, name: Text)(using Tactic[Io.Error])
  :   List[ReflogEntry] =

    val base = if name.starts(refsPrefix) then commonDir else gitDir
    val file = descend(base, t"logs" :: name.cut(t"/"))
    val utc = t"+0000"

    if !file.existent() then Nil else
      val lines = Objects.text(file.read[Data]).cut(t"\n").filter(_.length > 0).reverse

      def recur(lines: List[Text], index: Int, acc: List[ReflogEntry]): List[ReflogEntry] = lines match
        case line :: rest =>
          val tab = line.s.indexOf('\t')
          val head = if tab < 0 then line else line.keep(tab)
          val message = if tab < 0 then t"" else line.skip(tab + 1)
          val fields = head.cut(t" ")
          val hash = Refstore.nth(fields, 1).or(Git.Hash.zero.text)
          val timestamp = Refstore.nth(fields.reverse, 1).let(Git.Timestamp.parse(_, utc)).let(_.seconds).or(0L)
          val entry = ReflogEntry(Git.Hash.unsafe(hash), t"$name@{$index}", timestamp, message)
          recur(rest, index + 1, entry :: acc)

        case _ => acc.reverse

      recur(lines, 0, Nil)
