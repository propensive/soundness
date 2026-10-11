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

import filesystemBackends.javaBaseFilesystem
import filesystemOptions.createNonexistentParents
import filesystemOptions.overwritePreexisting
import filesystemOptions.preserveSymlinks
import filesystemOptions.moveAtomically
import filesystemOptions.deleteRecursively

// Linked worktrees, in git's layout: each has a directory under `worktrees/` in the common git
// directory holding its own HEAD, index and `gitdir` file (naming the `.git` file in the working
// tree, which in turn names this directory), and a `locked` file while it is locked.
object Worktrees:
  import Git.Error.Reason as Reason

  private def registry(commonDir: Path on Linux): Path on Linux = commonDir / "worktrees"

  private def readText(file: Path on Linux)(using Tactic[Io.Error]): Optional[Text] =
    if file.existent() then Objects.text(file.read[Data]).trim else Unset

  private def writeText(file: Path on Linux, content: Text)(using Tactic[Io.Error]): Unit =
    if file.existent() then file.delete()
    file.write(Objects.bytes(content))

  // The working tree a registry entry names: the parent of the `.git` file in its `gitdir`.
  def workingTree(entry: Path on Linux)(using Tactic[Io.Error]): Optional[Path on Linux] =
    readText(entry / "gitdir").let { text => unsafely(text.as[Path on Linux]).parent }

  def entries(commonDir: Path on Linux)(using Tactic[Io.Error]): List[Path on Linux] =
    val directory = registry(commonDir)
    if directory.existent() then directory.children.to[List] else Nil

  // The registry entry for the linked worktree at `path`.
  private def entry(commonDir: Path on Linux, path: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error])
  :   Path on Linux =

    def matches(candidate: Path on Linux): Boolean = workingTree(candidate) == Optional(path)
    entries(commonDir).filter(matches(_)).prim.or(abort(Git.Error(Reason.NoSuchWorktree(path.encode))))

  // A registry name for a new worktree: its directory's name, suffixed with a number if taken.
  private def freshName(commonDir: Path on Linux, base: Text): Text =
    def recur(suffix: Int): Text =
      val candidate = if suffix == 0 then base else t"$base$suffix"
      if Refstore.descend(registry(commonDir), List(candidate)).existent() then recur(suffix + 1) else candidate

    recur(0)

  def add
    ( gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux, target: Path on Linux,
      revision: Text, detach: Boolean, newBranch: Optional[Git.Branch], identity: Git.Identity, logging: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    if target.existent() && target.children.to[List] != Nil
    then abort(Git.Error(Reason.Unexpected, t"'${target.encode}' already exists"))

    val hash =
      Revwalk.resolve(gitDir, commonDir, objects, revision).or(abort(Git.Error(Reason.NoSuchRef(revision))))

    val commit = Revwalk.peel(objects, hash)
    val existingBranch = t"refs/heads/$revision"

    // Which ref HEAD should be attached to, if any.
    val branchRef: Optional[Text] = newBranch match
      case branch: Git.Branch =>
        val ref = t"refs/heads/${branch.text}"

        Refstore.update
          ( gitDir, commonDir, ref, commit, Git.Backend.Expectation.Absent, t"branch: Created from $revision",
            identity, logging )

        ref

      case _ =>
        if !detach && Refstore.resolve(gitDir, commonDir, existingBranch).present then existingBranch else Unset

    val name = freshName(commonDir, target.name)
    val worktreeGitDir = Refstore.descend(registry(commonDir), List(name))
    Refstore.ensureDirectory(worktreeGitDir)
    writeText(worktreeGitDir / "gitdir", t"${target.encode}/.git\n")
    writeText(worktreeGitDir / "commondir", t"../..\n")
    writeText(worktreeGitDir / "HEAD", branchRef.lay(t"${commit.text}\n") { ref => t"ref: $ref\n" })

    Refstore.ensureDirectory(target)
    writeText(target / ".git", t"gitdir: ${worktreeGitDir.encode}\n")

    val tree = Workspace.treeSnapshot(objects, Revwalk.commit(objects, commit).tree)
    val entries = Workspace.checkoutFiles(objects, target, Map(), tree)
    Index.write(worktreeGitDir, Index.Contents(2, entries, Nil))

  def lockReason(entry: Path on Linux)(using Tactic[Io.Error]): Optional[Text] = readText(entry / "locked")

  def remove(commonDir: Path on Linux, objects: Path on Linux, path: Path on Linux, force: Boolean)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val registered = entry(commonDir, path)
    if lockReason(registered).present then abort(Git.Error(Reason.Locked(path.encode)))

    if !force && path.existent() then
      val changes = Workspace.status(registered, commonDir, objects, path, false, true).map(_.path)
      if changes != Nil then abort(Git.Error(Reason.Dirty(changes)))

    if path.existent() then path.delete()
    registered.delete()

  def lock(commonDir: Path on Linux, path: Path on Linux, reason: Optional[Text])
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val registered = entry(commonDir, path)
    if lockReason(registered).present then abort(Git.Error(Reason.Locked(path.encode)))
    writeText(registered / "locked", reason.or(t""))

  def unlock(commonDir: Path on Linux, path: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    val registered = entry(commonDir, path)
    val file = registered / "locked"
    if !file.existent() then abort(Git.Error(Reason.Unexpected, t"'${path.encode}' is not locked"))
    file.delete()

  def move(commonDir: Path on Linux, path: Path on Linux, to: Path on Linux)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val registered = entry(commonDir, path)
    if lockReason(registered).present then abort(Git.Error(Reason.Locked(path.encode)))
    if to.existent() then abort(Git.Error(Reason.Unexpected, t"'${to.encode}' already exists"))
    path.moveTo(to)
    writeText(registered / "gitdir", t"${to.encode}/.git\n")

  // Removes the registry entries whose working trees have gone, returning their paths.
  def prune(commonDir: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error]): List[Path on Linux] =
    def prunable(registered: Path on Linux): Boolean =
      lockReason(registered).absent && workingTree(registered).lay(true)(!_.existent())

    def recur(remaining: List[Path on Linux], acc: List[Path on Linux]): List[Path on Linux] = remaining match
      case registered :: rest =>
        if prunable(registered) then
          val path = workingTree(registered)
          registered.delete()
          recur(rest, path.let(_ :: acc).or(acc))
        else recur(rest, acc)

      case _ => acc.reverse

    recur(entries(commonDir), Nil)
