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
import urticose.*
import vacuous.*

import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import filesystemOptions.dereferenceSymlinks
import filesystemOptions.overwritePreexisting

// The backend that reads and writes a repository's files itself, with no `git` binary: objects,
// refs, configuration and notes are complete; the index, working-tree and transport operations
// are supplied by later work and report `Unsupported` until then.
class Native(transports: List[Transport] = Nil) extends Git.Backend:
  import Git.Error.Reason as Reason

  private val dot: Text = t"."

  // Filesystem failures are reported as `Unexpected`, with the failure's own message as detail.
  private def io[result](block: Tactic[Io.Error] ?=> result)(using Tactic[Git.Error]): result =
    try throwErrors[Io.Error](block)
    catch case error: Io.Error => abort(Git.Error(Reason.Unexpected, error.toString.tt))

  private def unsupported(operation: Text)(using Tactic[Git.Error]): Nothing =
    abort(Git.Error(Reason.Unsupported(operation)))

  private def objects(gitDir: Path on Linux)(using Tactic[Git.Error]): Path on Linux =
    commonDir(gitDir) / "objects"

  // A linked worktree's git directory names the shared one in `commondir`.
  private def commonDir(gitDir: Path on Linux)(using Tactic[Git.Error]): Path on Linux = io:
    val file = gitDir / "commondir"

    if !file.existent() then gitDir else
      val target = Objects.text(file.read[Data]).trim

      if target.starts(t"/") then unsafely(target.as[Path on Linux])
      else Refstore.descend(gitDir, target.cut(t"/").filter(_ != dot))

  private def config(gitDir: Path on Linux)(using Tactic[Git.Error]): List[(Git.Config.Key, Text)] =
    io(Gitconfig.effective(gitDir, commonDir(gitDir)))

  private def setting(gitDir: Path on Linux, key: Text)(using Tactic[Git.Error]): Optional[Text] =
    Operations.setting(config(gitDir), unsafely(Git.Config.Key.parse(key)))

  private def identity(gitDir: Path on Linux)(using Tactic[Git.Error]): Git.Identity =
    Git.Identity
      ( setting(gitDir, t"user.name").or(t"unknown"), setting(gitDir, t"user.email")
      . or(t"unknown") )

  private def bare(gitDir: Path on Linux)(using Tactic[Git.Error]): Boolean =
    setting(gitDir, t"core.bare").lay(false)(_.lower == t"true")

  private def logging(gitDir: Path on Linux)(using Tactic[Git.Error]): Boolean =
    setting(gitDir, t"core.logallrefupdates").lay(!bare(gitDir))(_.lower != t"false")

  private def isGitDir(path: Path on Linux)(using Tactic[Io.Error]): Boolean =
    (path / "HEAD").existent() && (path / "objects").existent() && (path / "refs").existent()

  // ─────────────────────────────────────────── Repository ─────────────────────────────────────

  def locate(path: Path on Linux)(using Tactic[Git.Error]): Optional[Git.Backend.Location] = io:
    val dotGit = path / ".git"

    if !path.existent() then Unset
    else if dotGit.existent() then
      unsafely(dotGit.entry()) match
        case Directory =>
          Git.Backend.Location(dotGit, commonDir(dotGit), path)

        case _ =>
          // A linked worktree: `.git` is a file naming the worktree's git directory.
          val content = Objects.text(dotGit.read[Data]).trim

          if !content.starts(t"gitdir: ") then Unset else
            val target = content.skip(8).trim

            val gitDir =
              if target.starts(t"/") then unsafely(target.as[Path on Linux])
              else Refstore.descend(path, target.cut(t"/").filter(_ != dot))

            Git.Backend.Location(gitDir, commonDir(gitDir), path)

    else if isGitDir(path) then
      val worktree: Optional[Path on Linux] =
        if bare(path) then Unset else if path.name == t".git" then path.parent else Unset

      Git.Backend.Location(path, commonDir(path), worktree)

    else
      Unset

  def init(target: Path on Linux, initialBranch: Optional[Git.Branch], bare: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    io:

      val gitDir = if bare then target else target / ".git"
      Refstore.ensureDirectory(gitDir)

      def ensure(name: Text): Unit =
        val directory = Refstore.descend(gitDir, name.cut(t"/"))
        Refstore.ensureDirectory(directory)

      List
        ( t"objects", t"objects/info", t"objects/pack", t"refs", t"refs/heads", t"refs/tags",
          t"info" )

      . each(ensure(_))

      val branch = initialBranch.lay(t"main")(_.text)
      val bareText = if bare then t"true" else t"false"

      if !(gitDir / "HEAD").existent()
      then (gitDir / "HEAD").write(Objects.bytes(t"ref: refs/heads/$branch\n"))

      def entry(name: Text, value: Text): Gitconfig.Line =
        Gitconfig.Line.Entry(Git.Config.Key(t"core", Unset, name), value, t"\t$name = $value")

      val lines =
        List
          ( Gitconfig.Line.Section(t"core", Unset, t"[core]"),
            entry(t"repositoryformatversion", t"0"),
            entry(t"filemode", t"true"),
            entry(t"bare", bareText) ) +
          (if bare then Nil else List(entry(t"logallrefupdates", t"true")))

      if !(gitDir / "config").existent()
      then (gitDir / "config").write(Objects.bytes(Gitconfig.render(lines)))

      if !(gitDir / "description").existent() then
        val text = t"Unnamed repository; edit this file 'description' to name the repository.\n"
        (gitDir / "description").write(Objects.bytes(text))

  def clone
    ( source: Text, target: Path on Linux, branch: Optional[Git.Branch], depth: Optional[Int],
      bare: Boolean, recursive: Boolean, activity: Git.Activity^ )
    ( using Internet, Tactic[Git.Error] )
  :   Unit =

    if recursive then unsupported(t"cloning submodules recursively")
    val peer = Transfer.connect(source, transports, locate(_), Git.Identity(t"unknown", t"unknown"))
    val remoteHead = peer.symbolicHead()
    val chosen = branch.let(_.text).or(remoteHead.let(_.skip(11))).or(t"main")
    init(target, Git.Branch.unsafe(chosen), bare)
    val location = locate(target).or(abort(Git.Error(Reason.NoRepository(target.encode))))
    val gitDir = location.gitDir

    io:
      val file = gitDir / "config"
      val origin: Optional[Text] = t"origin"
      val lines = Gitconfig.readFile(file)
      val withUrl = Gitconfig.set(lines, Git.Config.Key(t"remote", origin, t"url"), source, true)

      val fetchSpec =
        if bare then t"+refs/heads/*:refs/heads/*" else t"+refs/heads/*:refs/remotes/origin/*"

      val withFetch =
        Gitconfig.set(withUrl, Git.Config.Key(t"remote", origin, t"fetch"), fetchSpec, true)

      Gitconfig.writeFile(file, withFetch)

    val context = transferContext(gitDir)
    io(Transfer.fetch(context, t"origin", peer, source, Nil, depth, false, true, activity))

    io:
      val tracking = if bare then t"refs/heads/$chosen" else t"refs/remotes/origin/$chosen"
      val tip = Refstore.resolve(gitDir, context.commonDir, tracking)

      tip.let: tip =>
        if !bare then
          Refstore.update
            ( gitDir, context.commonDir, t"refs/heads/$chosen", tip, Git.Backend.Expectation.Absent,
              t"clone: from $source", context.identity, context.logging )

          val file = gitDir / "config"
          val lines = Gitconfig.readFile(file)

          val withRemote =
            Gitconfig.set(lines, Git.Config.Key(t"branch", chosen, t"remote"), t"origin", true)

          val mergeKey = Git.Config.Key(t"branch", chosen, t"merge")
          val withMerge = Gitconfig.set(withRemote, mergeKey, t"refs/heads/$chosen", true)
          Gitconfig.writeFile(file, withMerge)

          activity.begin(Git.Activity.Phase.Checkout)

          val tree =
            Workspace.treeSnapshot(context.objects, Revwalk.commit(context.objects, tip).tree)

          val entries = Workspace.checkoutFiles(context.objects, target, Map(), tree)
          Index.write(gitDir, Index.Contents(2, entries, Nil))

    activity.finish()

  private def transferContext(gitDir: Path on Linux)(using Tactic[Git.Error]): Transfer.Context =
    Transfer.Context(gitDir, commonDir(gitDir), objects(gitDir), identity(gitDir), logging(gitDir))

  // ────────────────────────────────────────────── Refs ────────────────────────────────────────

  def head(gitDir: Path on Linux)(using Tactic[Git.Error]): Git.Head = io:
    val common = commonDir(gitDir)

    Refstore.read(gitDir, common, t"HEAD") match
      case Git.Backend.Target.Symbolic(ref) =>
        val name = if ref.starts(t"refs/heads/") then ref.skip(11) else ref
        Git.Head.Attached(Git.Branch.unsafe(name), Refstore.resolve(gitDir, common, ref))

      case Git.Backend.Target.Direct(hash) => Git.Head.Detached(hash)
      case _                               => abort(Git.Error(Reason.NoSuchRef(t"HEAD")))

  def readRef(gitDir: Path on Linux, ref: Text)
    ( using Tactic[Git.Error] )
  :   Optional[Git.Backend.Target] =

    io(Refstore.read(gitDir, commonDir(gitDir), ref))

  def listRefs(gitDir: Path on Linux, prefix: Optional[Text])
    ( using Tactic[Git.Error] )
  :   List[(Text, Git.Hash)] =

    io(Refstore.list(commonDir(gitDir), prefix))

  def peel(gitDir: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error]): Git.Hash =
    io(Revwalk.peel(objects(gitDir), hash))

  def updateRef
    ( gitDir: Path on Linux, ref: Text, to: Optional[Git.Hash], expected: Git.Backend.Expectation,
      message: Optional[Text] )
    ( using Tactic[Git.Error] )
  :   Unit = io:

    Refstore.update
      ( gitDir, commonDir(gitDir), ref, to, expected, message, identity(gitDir), logging(gitDir) )

  def updateSymbolicRef(gitDir: Path on Linux, name: Text, target: Text)
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Refstore.writeSymbolic(gitDir, commonDir(gitDir), name, target))

  def resolve(gitDir: Path on Linux, revision: Text)(using Tactic[Git.Error]): Optional[Git.Hash] =
    io(Revwalk.resolve(gitDir, commonDir(gitDir), objects(gitDir), revision))

  def revList
    ( gitDir: Path on Linux,
      from: Text,
      until: Optional[Text],
      limit: Optional[Int],
      paths: List[Text] )
    ( using Tactic[Git.Error] )
  :   List[Git.Hash] =

    io:

      val start = resolve(gitDir, from).or(abort(Git.Error(Reason.NoSuchRef(from))))

      val stop =
        until.let: until => resolve(gitDir, until).or(abort(Git.Error(Reason.NoSuchRef(until))))

      Revwalk.walk(objects(gitDir), start, stop, limit, paths)

  def reflog(gitDir: Path on Linux, ref: Optional[Text])
    ( using Tactic[Git.Error] )
  :   List[ReflogEntry] =

    io(Refstore.reflog(gitDir, commonDir(gitDir), ref.or(t"HEAD")))

  def mergeBase(gitDir: Path on Linux, left: Git.Hash, right: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Optional[Git.Hash] =

    io(Revwalk.mergeBase(objects(gitDir), left, right))

  def isAncestor(gitDir: Path on Linux, ancestor: Git.Hash, descendant: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Boolean =

    io(Revwalk.isAncestor(objects(gitDir), ancestor, descendant))

  // ──────────────────────────────────────────── Objects ───────────────────────────────────────

  def readObject(gitDir: Path on Linux, hash: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Optional[(Git.Object.Kind, Data)] =

    io(Store.read(objects(gitDir), hash))

  def readObjects(gitDir: Path on Linux, hashes: List[Git.Hash])(using Tactic[Git.Error])
  :   List[(Git.Hash, Git.Object.Kind, Data)] =

    io:

      val store = objects(gitDir)

      def read(hash: Git.Hash): List[(Git.Hash, Git.Object.Kind, Data)] =
        Store.read(store, hash).lay(Nil): (kind, data) => List((hash, kind, data))

      hashes.bind(read(_))

  def writeObject(gitDir: Path on Linux, kind: Git.Object.Kind, data: Data)
    ( using Tactic[Git.Error] )
  :   Git.Hash =

    io(Store.write(objects(gitDir), kind, data))

  def diffTree(gitDir: Path on Linux, left: Git.Hash, right: Git.Hash)
    ( using Tactic[Git.Error] )
  :   List[FileDiff] =

    io(Workspace.diffTree(objects(gitDir), left, right))

  // ─────────────────────────────────────── Index and working files ────────────────────────────

  def status(gitDir: Path on Linux, worktree: Path on Linux, ignored: Boolean, untracked: Boolean)
    ( using Tactic[Git.Error] )
  :   List[Git.Backend.StatusLine] =

    io(Workspace.status(gitDir, commonDir(gitDir), objects(gitDir), worktree, ignored, untracked))

  def add(gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], all: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Workspace.add(gitDir, objects(gitDir), worktree, paths, all))

  def remove
    ( gitDir: Path on Linux,
      worktree: Path on Linux,
      paths: List[Text],
      cached: Boolean,
      force: Boolean )
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Workspace.remove(gitDir, worktree, paths, cached, force))

  def move(gitDir: Path on Linux, worktree: Path on Linux, from: Text, to: Text)
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Workspace.move(gitDir, worktree, from, to))

  def restore
    ( gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], source: Optional[Text],
      staged: Boolean, workingFiles: Boolean )
    ( using Tactic[Git.Error] )
  :   Unit =

    io:
      Workspace.restore
        ( gitDir,
          commonDir(gitDir),
          objects(gitDir),
          worktree,
          paths,
          source,
          staged,
          workingFiles )

  def clean
    ( gitDir: Path on Linux,
      worktree: Path on Linux,
      paths: List[Text],
      directories: Boolean,
      ignored: Boolean )
    ( using Tactic[Git.Error] )
  :   List[Text] =

    io(Workspace.clean(gitDir, worktree, paths, directories, ignored))

  def writeTree(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): Git.Hash =
    io(Workspace.writeTree(gitDir, objects(gitDir)))

  def readTree(gitDir: Path on Linux, worktree: Path on Linux, mode: ResetMode)
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Workspace.readTree(gitDir, commonDir(gitDir), objects(gitDir), worktree, mode))

  def diffWorktree
    ( gitDir: Path on Linux, worktree: Path on Linux, staged: Boolean, against: Optional[Text] )
    ( using Tactic[Git.Error] )
  :   List[FileDiff] =

    io:
      Workspace.diffWorktree
        ( gitDir, commonDir(gitDir), objects(gitDir), worktree, staged, against )

  def tracked(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): List[Text] =
    io(Workspace.tracked(gitDir))

  def conflicts(gitDir: Path on Linux, worktree: Path on Linux)
    ( using Tactic[Git.Error] )
  :   List[Text] =

    io(Workspace.conflicts(gitDir))

  def checkout(gitDir: Path on Linux, worktree: Path on Linux, revision: Text)
    ( using Tactic[Git.Error] )
  :   Unit =

    io:
      Workspace.checkout
        ( gitDir,
          commonDir(gitDir),
          objects(gitDir),
          worktree,
          revision,
          identity(gitDir),
          logging(gitDir) )

  def switch(gitDir: Path on Linux, worktree: Path on Linux, branch: Git.Branch, create: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    io:
      Workspace.switch
        ( gitDir,
          commonDir(gitDir),
          objects(gitDir),
          worktree,
          branch,
          create,
          identity(gitDir),
          logging(gitDir) )

  private def context(gitDir: Path on Linux, worktree: Path on Linux)
    ( using Tactic[Git.Error] )
  :   Sequencer.Context =

    Sequencer.Context
      ( gitDir, commonDir(gitDir), objects(gitDir), worktree, identity(gitDir), logging(gitDir) )

  def merge
    ( gitDir: Path on Linux,
      worktree: Path on Linux,
      revision: Text,
      ff: FastForward,
      message: Optional[Text],
      sign: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Merge.Outcome =

    io(Sequencer.merge(context(gitDir, worktree), revision, ff, message, sign))

  def cherryPick
    ( gitDir: Path on Linux, worktree: Path on Linux, commits: List[Git.Hash], noCommit: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    io(Sequencer.cherryPick(context(gitDir, worktree), commits, false, noCommit))

  def revert
    ( gitDir: Path on Linux, worktree: Path on Linux, commits: List[Git.Hash], noCommit: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    io(Sequencer.cherryPick(context(gitDir, worktree), commits, true, noCommit))

  def rebase
    ( gitDir: Path on Linux, worktree: Path on Linux, onto: Text, upstream: Optional[Text],
      options: List[Git.Rebase.Option] )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    io(Sequencer.rebase(context(gitDir, worktree), onto, upstream, options))

  def sequence
    ( gitDir: Path on Linux, worktree: Path on Linux, verb: Git.Sequence.Verb )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    io(Sequencer.sequence(context(gitDir, worktree), verb))

  def operation
    ( gitDir: Path on Linux, worktree: Optional[Path on Linux] )
    ( using Tactic[Git.Error] )
  :   Optional[Git.Operation] =

    io:
      Sequencer.operation(gitDir, worktree.lay(Nil) { _ => Workspace.conflicts(gitDir) })

  def stash
    ( gitDir: Path on Linux,
      worktree: Path on Linux,
      message: Optional[Text],
      includeUntracked: Boolean,
      keepIndex: Boolean )
    ( using Tactic[Git.Error] )
  :   Optional[Git.Hash] =

    io(Sequencer.stash(context(gitDir, worktree), message, includeUntracked, keepIndex))

  def stashes(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Git.Stash.Entry] = io:
    def recur(remaining: List[ReflogEntry], index: Int, acc: List[Git.Stash.Entry])
    :   List[Git.Stash.Entry] =

      remaining match
        case entry :: rest =>
          recur(rest, index + 1, Git.Stash.Entry(index, entry.hash, entry.message) :: acc)

        case _             => acc.reverse

    recur(Refstore.reflog(gitDir, commonDir(gitDir), t"refs/stash"), 0, Nil)

  def applyStash
    ( gitDir: Path on Linux, worktree: Path on Linux, entry: Int, index: Boolean, drop: Boolean )
    ( using Tactic[Git.Error] )
  :   Git.Sequence.Outcome =

    io(Sequencer.applyStash(context(gitDir, worktree), entry, index, drop))

  def dropStash(gitDir: Path on Linux, entry: Int)(using Tactic[Git.Error]): Unit =
    io(Sequencer.dropStash(context(gitDir, gitDir), entry))

  // ───────────────────────────────────────────── Notes ────────────────────────────────────────

  def readNote(gitDir: Path on Linux, ref: Text, target: Git.Hash)
    ( using Tactic[Git.Error] )
  :   Optional[Text] =

    io(Annotations.read(gitDir, commonDir(gitDir), objects(gitDir), ref, target))

  def listNotes(gitDir: Path on Linux, ref: Text)
    ( using Tactic[Git.Error] )
  :   List[(Git.Hash, Git.Hash)] =

    io(Annotations.list(gitDir, commonDir(gitDir), objects(gitDir), ref))

  def editNotes(gitDir: Path on Linux, base: Optional[Git.Hash], edit: Git.Backend.NoteEdit)
    ( using Tactic[Git.Error] )
  :   Git.Hash =

    io(Annotations.edit(gitDir, commonDir(gitDir), objects(gitDir), base, edit, identity(gitDir)))

  // ───────────────────────────────────────────── Config ───────────────────────────────────────

  private def configFile
    ( source: Git.Config.Source )
    ( using Tactic[Git.Error] )
  :   Path on Linux =

    source match
      case Git.Config.Source.File(path)                                    => path

      case Git.Config.Source.Repository(gitDir, Git.Config.Scope.Worktree) =>
        gitDir / "config.worktree"

      case Git.Config.Source.Repository(gitDir, Git.Config.Scope.Local) =>
        commonDir(gitDir) / "config"

      case Git.Config.Source.Repository(_, Git.Config.Scope.Global) =>
        unsafely(t"${System.getProperty("user.home").nn}/.gitconfig".as[Path on Linux])

      case Git.Config.Source.Repository(_, Git.Config.Scope.System) =>
        unsafely(t"/etc/gitconfig".as[Path on Linux])

      case Git.Config.Source.Effective(gitDir)                      => commonDir(gitDir) / "config"

  def configRead
    ( source: Git.Config.Source )
    ( using Tactic[Git.Error] )
  :   List[(Git.Config.Key, Text)] =

    io:
      source match
        case Git.Config.Source.Effective(gitDir) => Gitconfig.effective(gitDir, commonDir(gitDir))

        case other =>
          Gitconfig.entries(Gitconfig.readFile(configFile(other)))

  def configWrite
    ( source: Git.Config.Source, key: Git.Config.Key, value: Text, add: Boolean )
    ( using Tactic[Git.Error] )
  :   Unit =

    io:
      val file = configFile(source)
      val lines = Gitconfig.readFile(file)

      val updated =
        if add then Gitconfig.add(lines, key, value) else Gitconfig.set(lines, key, value, false)

      Gitconfig.writeFile(file, updated)

  def configUnset
    ( source: Git.Config.Source, key: Git.Config.Key, all: Boolean )
    ( using Tactic[Git.Error] )
  :   Unit =

    io:
      val file = configFile(source)
      Gitconfig.writeFile(file, Gitconfig.unset(Gitconfig.readFile(file), key, all))

  // ────────────────────────────────────── Remotes and transport ───────────────────────────────

  def remoteRefs(gitDir: Path on Linux, remote: Text)
    ( using Internet, Tactic[Git.Error] )
  :   List[(Text, Git.Hash)] =

    val context = transferContext(gitDir)
    val url = io(Transfer.resolveUrl(context, remote, false))
    val peer = Transfer.connect(url, transports, locate(_), context.identity)
    io(Transfer.remoteRefs(context, remote, peer))

  def fetch
    ( gitDir: Path on Linux,
      remote: Text,
      refspecs: List[Git.Refspec],
      depth: Optional[Int],
      prune: Boolean,
      tags: Boolean,
      activity: Git.Activity^ )
    ( using Internet, Tactic[Git.Error] )
  :   List[(Text, Git.Hash)] =

    val context = transferContext(gitDir)
    val url = io(Transfer.resolveUrl(context, remote, false))
    val peer = Transfer.connect(url, transports, locate(_), context.identity)
    io(Transfer.fetch(context, remote, peer, url, refspecs, depth, prune, tags, activity))

  def push
    ( gitDir: Path on Linux,
      remote: Text,
      refspecs: List[Git.Refspec],
      force: Git.Push.Force,
      tags: Boolean,
      setUpstream: Boolean,
      activity: Git.Activity^ )
    ( using Internet, Tactic[Git.Error] )
  :   List[(Text, Git.Push.Outcome)] =

    val context = transferContext(gitDir)
    val url = io(Transfer.resolveUrl(context, remote, true))
    val peer = Transfer.connect(url, transports, locate(_), context.identity)
    io(Transfer.push(context, remote, peer, url, refspecs, force, tags, setUpstream, activity))

  // ────────────────────────────────────────── Linked worktrees ────────────────────────────────

  def worktrees
    ( gitDir: Path on Linux )
    ( using Tactic[Git.Error] )
  :   List[Git.Backend.WorktreeEntry] =

    io:
      val common = commonDir(gitDir)

      def entry(dir: Path on Linux, worktree: Optional[Path on Linux]): Git.Backend.WorktreeEntry =
        val head = Refstore.read(dir, common, t"HEAD")

        val branch = head match
          case Git.Backend.Target.Symbolic(ref) if ref.starts(t"refs/heads/") =>
            Git.Branch.unsafe(ref.skip(11))

          case _                                                              => Unset

        val locked = (dir / "locked").existent()

        val reason: Optional[Text] =
          if locked then Objects.text((dir / "locked").read[Data]).trim else Unset

        val path = worktree.or(common)

        val missing = worktree.lay(false)(!_.existent())
        val prunable = if missing then t"gitdir file points to non-existent location" else Unset

        Git.Backend.WorktreeEntry
          ( path, Refstore.resolve(dir, common, t"HEAD"), branch, worktree.absent, reason,
            prunable )

      val main = entry(common, if bare(common) then Unset else common.parent)
      val linked = common / "worktrees"

      def linkedEntry(dir: Path on Linux): Git.Backend.WorktreeEntry =
        val gitdirFile = dir / "gitdir"

        val target: Optional[Path on Linux] =
          if gitdirFile.existent() then
            val text = Objects.text(gitdirFile.read[Data]).trim
            unsafely(text.as[Path on Linux]).parent
          else
            Unset

        entry(dir, target)

      val others = if !linked.existent() then Nil else linked.children.to[List].map(linkedEntry(_))

      main :: others

  def addWorktree
    ( gitDir: Path on Linux, target: Path on Linux, revision: Text, detach: Boolean,
      newBranch: Optional[Git.Branch] )
    ( using Tactic[Git.Error] )
  :   Unit = io:
    Worktrees.add
      ( gitDir,
        commonDir(gitDir),
        objects(gitDir),
        target,
        revision,
        detach,
        newBranch,
        identity(gitDir),
        logging(gitDir) )

  def removeWorktree(gitDir: Path on Linux, path: Path on Linux, force: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Worktrees.remove(commonDir(gitDir), objects(gitDir), path, force))

  def lockWorktree(gitDir: Path on Linux, path: Path on Linux, reason: Optional[Text])
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Worktrees.lock(commonDir(gitDir), path, reason))

  def unlockWorktree(gitDir: Path on Linux, path: Path on Linux)(using Tactic[Git.Error]): Unit =
    io(Worktrees.unlock(commonDir(gitDir), path))

  def moveWorktree(gitDir: Path on Linux, path: Path on Linux, to: Path on Linux)
    ( using Tactic[Git.Error] )
  :   Unit =

    io(Worktrees.move(commonDir(gitDir), path, to))

  def pruneWorktrees(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Path on Linux] =
    io(Worktrees.prune(commonDir(gitDir)))
