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
import dissonance.*
import distillate.*
import galilei.*
import gossamer.*
import prepositional.*
import rudiments.*
import serpentine.*
import symbolism.*
import turbulence.*
import vacuous.*

import filesystemOptions.createNonexistentParents
import filesystemOptions.deleteRecursively
import filesystemOptions.moveAtomically
import filesystemOptions.overwritePreexisting
import filesystemOptions.preserveSymlinks
import sortingAlgorithms.timsort

// The working tree and the index together: what differs between HEAD, the index and the files,
// and the operations that move content between them. Paths are worktree-relative, slash-separated.
private[octogenarian] object Workspace:
  import Git.Error.Reason as Reason

  // A path's entry in a tree or the index: its mode and the hash of its content.
  type Snapshot = Map[Text, (Int, Git.Hash)]

  private def modeInt(mode: Git.Object.Tree.Mode): Int = mode match
    case Git.Object.Tree.Mode.Regular    => Index.regular
    case Git.Object.Tree.Mode.Executable => Index.executable
    case Git.Object.Tree.Mode.Symlink    => Index.symlink
    case Git.Object.Tree.Mode.Gitlink    => Index.gitlink
    case Git.Object.Tree.Mode.Directory  => 0x4000

  private def treeMode(mode: Int): Git.Object.Tree.Mode =
    if mode == Index.symlink then Git.Object.Tree.Mode.Symlink
    else if mode == Index.gitlink then Git.Object.Tree.Mode.Gitlink
    else if (mode & 0x49) != 0 then Git.Object.Tree.Mode.Executable
    else Git.Object.Tree.Mode.Regular

  def file(worktree: Path on Linux, path: Text): Path on Linux =
    Refstore.descend(worktree, path.cut(t"/"))

  // Every blob in a tree, by path.
  def treeSnapshot
    ( objects: Path on Linux, tree: Optional[Git.Hash] )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Snapshot =

    tree.lay(Map()): tree =>
      def recur(tree: Git.Hash, prefix: Text, acc: List[(Text, (Int, Git.Hash))])
      :   List[(Text, (Int, Git.Hash))] =

        def visit(entries: List[Git.Object.Tree.Entry], acc: List[(Text, (Int, Git.Hash))])
        :   List[(Text, (Int, Git.Hash))] =

          entries match
            case entry :: rest =>
              val path = t"$prefix${entry.name}"

              entry.mode match
                case Git.Object.Tree.Mode.Directory =>
                  visit(rest, recur(entry.hash, t"$path/", acc))

                case mode =>
                  visit(rest, (path, (modeInt(mode), entry.hash)) :: acc)

            case _ => acc

        visit(Trees.entries(objects, tree), acc)

      recur(tree, t"", Nil).to[Map]

  def headTree(gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Git.Hash] =

    Refstore.resolve(gitDir, commonDir, t"HEAD").let(Revwalk.commit(objects, _).tree)

  def indexSnapshot(contents: Index.Contents): Snapshot =
    contents.staged.map { entry => (entry.path, (entry.mode, entry.hash)) }.to[Map]

  // The working files as they are now, for every path the index knows: an entry whose stat data
  // still matches is trusted, any other is hashed, a conflicted path gets a null entry, and a
  // deleted file has none. A checkout from this snapshot rewrites exactly what differs.
  def workingSnapshot(gitDir: Path on Linux, worktree: Path on Linux, contents: Index.Contents)
    ( using Tactic[Io.Error] )
  :   Snapshot =

    val indexModified = Index.modified(gitDir)

    def working(entry: Index.Entry): Optional[(Text, (Int, Git.Hash))] =
      val target = file(worktree, entry.path)

      if !target.existent() then Unset
      else if entry.stage > 0 then (entry.path, (0, Git.Hash.zero))
      else
        val mode =
          if entry.mode == Index.symlink || entry.mode == Index.gitlink then entry.mode
          else if target.executable() then Index.executable
          else Index.regular

        if Index.fresh(entry, worktree, indexModified) && mode == entry.mode
        then (entry.path, (mode, entry.hash))
        else (entry.path, (mode, hashFile(worktree, entry.path, mode)))

    contents.entries.bind { entry => working(entry).let(List(_)).or(Nil) }.to[Map]

  // Every file and symbolic link beneath the worktree, except the git directory, with its mode.
  def files(worktree: Path on Linux)(using Tactic[Io.Error]): List[(Text, Int)] =
    val backend = summon[FilesystemBackend on Linux]

    def recur(directory: Path on Linux, prefix: Text, acc: List[(Text, Int)]): List[(Text, Int)] =
      def visit
        ( children: List[Path on Linux], acc: List[(Text, Int)] )
      :   List[(Text, Int)] =

        children match
          case child :: rest =>
            val path = t"$prefix${child.name}"

            if child.name == t".git" then visit(rest, acc) else
              backend.stat(child, false).entry match
                case Directory => visit(rest, recur(child, t"$path/", acc))
                case Symlink   => visit(rest, (path, Index.symlink) :: acc)

                case File =>
                  val executable = backend.attribute(child, FilesystemBackend.Attribute.Executable)
                  val mode = if executable then Index.executable else Index.regular
                  visit(rest, (path, mode) :: acc)

                case _ => visit(rest, acc)

          case _ => acc

      visit(directory.children.to[List], acc)

    recur(worktree, t"", Nil)

  // A working file's content as git would store it: a symbolic link is the text of its target.
  def content(worktree: Path on Linux, path: Text, mode: Int)(using Tactic[Io.Error]): Data =
    val target = file(worktree, path)

    if mode == Index.symlink
    then Objects.bytes(summon[FilesystemBackend on Linux].linkTarget(target))
    else target.read[Data]

  def hashFile(worktree: Path on Linux, path: Text, mode: Int)(using Tactic[Io.Error]): Git.Hash =
    Store.hash(Git.Object.Kind.Blob, content(worktree, path, mode))

  // ───────────────────────────────────────────── Status ───────────────────────────────────────

  private def kindOf
    ( mode: Int )
  :   Int =

    if mode == Index.symlink then 1 else if mode == Index.gitlink then 2 else 0

  // The index column: how the staged tree differs from HEAD at a path.
  private def indexStatus(head: Optional[(Int, Git.Hash)], staged: Optional[(Int, Git.Hash)])
  :   Optional[Git.Status] =

    head match
      case (headMode: Int, headHash: Git.Hash) =>
        staged match
          case (mode: Int, hash: Git.Hash) =>
            if kindOf(headMode) != kindOf(mode) then Git.Status.TypeChanged
            else if headHash != hash || headMode != mode then Git.Status.Updated
            else Unset

          case _ => Git.Status.Deleted

      case _ =>
        staged match
          case (_: Int, _: Git.Hash) => Git.Status.Added
          case _                     => Unset

  def status
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      showIgnored: Boolean,
      showUntracked: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Git.Backend.StatusLine] =

    val contents = Index.read(gitDir)
    val head = treeSnapshot(objects, headTree(gitDir, commonDir, objects))
    val staged = indexSnapshot(contents)
    val present = files(worktree)
    val presentModes = present.to[Map]
    val indexModified = Index.modified(gitDir)
    val rules = Ignore.load(gitDir, worktree)
    val conflicted = contents.entries.filter(_.stage > 0).map(_.path).to[Set]

    // The worktree column for a path the index knows.
    def worktreeStatus(entry: Index.Entry): Optional[Git.Status] =
      presentModes.at(entry.path) match
        case mode: Int =>
          if kindOf(mode) != kindOf(entry.mode) then Git.Status.TypeChanged
          else if Index.fresh(entry, worktree, indexModified) && mode == entry.mode then Unset
          else if hashFile(worktree, entry.path, mode) != entry.hash || mode != entry.mode
          then Git.Status.Updated
          else Unset

        case _ => Git.Status.Deleted

    def tracked(path: Text): Optional[Git.Backend.StatusLine] =
      if conflicted.has(path)
      then Git.Backend.StatusLine(Git.Status.Unmerged, Git.Status.Unmerged, path, Unset)
      else
        val index = indexStatus(head.at(path), staged.at(path))
        val working = contents.at(path).let(worktreeStatus(_))

        if index.absent && working.absent then Unset
        else Git.Backend.StatusLine(index, working, path, Unset)

    def untracked(entry: (Text, Int)): Optional[Git.Backend.StatusLine] =
      val (path, _) = entry

      if staged.at(path).present || conflicted.has(path) then Unset
      else if rules.ignored(path, false) then
        if showIgnored
        then Git.Backend.StatusLine(Git.Status.Ignored, Git.Status.Ignored, path, Unset)
        else Unset
      else if showUntracked then
        Git.Backend.StatusLine(Git.Status.Untracked, Git.Status.Untracked, path, Unset)
      else
        Unset

    def collect[element](items: List[element])(lambda: element => Optional[Git.Backend.StatusLine])
    :   List[Git.Backend.StatusLine] =

      def recur(remaining: List[element], acc: List[Git.Backend.StatusLine])
      :   List[Git.Backend.StatusLine] =

        remaining match
          case item :: rest => lambda(item) match
            case line: Git.Backend.StatusLine => recur(rest, line :: acc)
            case _                            => recur(rest, acc)

          case _ => acc

      recur(items, Nil)

    val knownPaths = (head.keys.to[List] + staged.keys.to[List] + conflicted.to[List]).distinct
    val lines = collect(knownPaths)(tracked(_)) + collect(present)(untracked(_))
    lines.order(_.path.s)

  // ─────────────────────────────────────────── Index edits ────────────────────────────────────

  private def withoutPaths(contents: Index.Contents, paths: List[Text]): Index.Contents =
    contents.copy(entries = contents.entries.filter { entry => !paths.has(entry.path) })

  // Whether `candidate` is `path` or lies beneath it.
  private def under(candidate: Text, path: Text): Boolean =
    path == t"." || candidate == path || candidate.starts(t"$path/")

  // The working files under a worktree-relative path, which may name a file or a directory.
  private def expand(worktree: Path on Linux, path: Text)
    ( using Tactic[Io.Error] )
  :   List[(Text, Int)] =

    val all = files(worktree)
    val prefix = t"$path/"

    if path == t"." || path.length == 0 then all
    else all.filter: entry => entry(0) == path || entry(0).starts(prefix)

  def add
    ( gitDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      paths: List[Text],
      all: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val contents = Index.read(gitDir)
    val rules = Ignore.load(gitDir, worktree)
    val staged = indexSnapshot(contents)

    def stage(entry: (Text, Int)): Index.Entry =
      val (path, mode) = entry
      val hash = Store.write(objects, Git.Object.Kind.Blob, content(worktree, path, mode))
      Index.entryFor(worktree, path, hash, mode)

    def wanted(entry: (Text, Int), requested: Text): Boolean =
      staged.at(entry(0)).present || !rules.ignored(entry(0), false) || entry(0) == requested

    def requestedFor(path: Text): List[(Text, Int)] =
      val found = expand(worktree, path)
      if found == Nil && staged.at(path).absent then abort(Git.Error(Reason.Untracked(path)))
      found.filter(wanted(_, path))

    val requested: List[(Text, Int)] =
      if all then files(worktree).filter(wanted(_, t"")) else paths.bind(requestedFor(_))

    // Tracked files which no longer exist are removed from the index, as `git add` does.
    val present = files(worktree).map(_(0))
    val requestedPaths = requested.map(_(0))

    val vanished: List[Text] =
      if all then staged.keys.to[List].filter(!present.has(_))
      else paths.filter: path => staged.at(path).present && !present.has(path)

    val fresh = requested.map(stage(_))
    val kept = withoutPaths(contents, requestedPaths + vanished)
    Index.write(gitDir, kept.copy(entries = kept.entries + fresh))

  def remove
    ( gitDir: Path on Linux,
      worktree: Path on Linux,
      paths: List[Text],
      cached: Boolean,
      force: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val contents = Index.read(gitDir)
    val staged = indexSnapshot(contents)

    def matching(path: Text): List[Text] =
      val matched = staged.keys.to[List].filter(under(_, path))
      if matched == Nil then abort(Git.Error(Reason.Untracked(path)))
      matched

    val targets: List[Text] = paths.bind(matching(_))
    val present = files(worktree).to[Map]

    def modified(path: Text): Boolean =
      !force && present.at(path).lay(false): mode =>
        hashFile(worktree, path, mode) != staged.at(path).let(_(1))

    val dirty = targets.filter(modified(_))
    if dirty != Nil then abort(Git.Error(Reason.Dirty(dirty)))
    Index.write(gitDir, withoutPaths(contents, targets))

    def unlink(path: Text): Unit =
      val target = file(worktree, path)
      if target.existent() then target.delete()
      pruneEmpty(worktree, target)

    if !cached then targets.each(unlink(_))

  // Removes a deleted file's parent directories while they are empty, as git does.
  def pruneEmpty(worktree: Path on Linux, deleted: Path on Linux)(using Tactic[Io.Error]): Unit =
    val parent: Optional[Path on Linux] = deleted.parent

    parent.lay(()): parent =>
      if parent != worktree && parent.existent() && parent.children.to[List] == Nil then
        parent.delete()
        pruneEmpty(worktree, parent)

  def move(gitDir: Path on Linux, worktree: Path on Linux, from: Text, to: Text)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val contents = Index.read(gitDir)
    val entry = contents.at(from).or(abort(Git.Error(Reason.Untracked(from))))
    val source = file(worktree, from)
    val destination = file(worktree, to)
    if !source.existent() then abort(Git.Error(Reason.Untracked(from)))
    destination.parent.let(Refstore.ensureDirectory(_))
    source.moveTo(destination)
    pruneEmpty(worktree, source)
    val moved = entry.copy(path = to)

    Index.write
      ( gitDir, contents.copy(entries = contents.entries.filter(_.path != from) + List(moved)) )

  def restore
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      paths: List[Text],
      source: Optional[Text],
      staged: Boolean,
      workingFiles: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val contents = Index.read(gitDir)

    val sourceTree: Optional[Git.Hash] = source match
      case revision: Text =>
        val hash =
          Revwalk.resolve(gitDir, commonDir, objects, revision)
          . or(abort(Git.Error(Reason.NoSuchRef(revision))))
        Revwalk.commit(objects, Revwalk.peel(objects, hash)).tree

      case _ => if staged then headTree(gitDir, commonDir, objects) else Unset

    val fromTree = treeSnapshot(objects, sourceTree)
    val fromIndex = indexSnapshot(contents)
    val origin = if source.present || staged then fromTree else fromIndex

    def requested(candidate: Text): Boolean = paths.exists(under(candidate, _))
    def selected(snapshot: Snapshot): List[Text] = snapshot.keys.to[List].filter(requested(_))

    val targets =
      selected(origin) + selected(fromIndex).filter: path =>
        paths.has(path) && origin.at(path).absent

    def restored(path: Text): List[Index.Entry] =
      origin.at(path).lay(Nil): (mode, hash) => List(Index.entryFor(worktree, path, hash, mode))

    def rewrite(path: Text): Unit =
      origin.at(path).lay(()): (mode, hash) => writeFile(objects, worktree, path, mode, hash)

    val updatedIndex =
      if !staged then contents else
        val kept = withoutPaths(contents, targets)
        kept.copy(entries = kept.entries + targets.bind(restored(_)))

    if workingFiles then targets.each(rewrite(_))
    if staged then Index.write(gitDir, updatedIndex)

  // Writes a blob to a working file with the given mode, creating directories as needed.
  def writeFile
    ( objects: Path on Linux, worktree: Path on Linux, path: Text, mode: Int, hash: Git.Hash )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val target = file(worktree, path)
    val (_, data) = Store.readOr(objects, hash)
    target.parent.let(Refstore.ensureDirectory(_))
    if target.existent() then target.delete()

    if mode == Index.symlink then
      java.nio.file.Files.createSymbolicLink
        ( java.nio.file.Path.of(target.encode.s), java.nio.file.Path.of(Objects.text(data).s) )
    else
      target.write(data)
      target.executable() = mode == Index.executable

  def clean
    ( gitDir: Path on Linux,
      worktree: Path on Linux,
      paths: List[Text],
      directories: Boolean,
      showIgnored: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Text] =

    val contents = Index.read(gitDir)
    val staged = indexSnapshot(contents)
    val rules = Ignore.load(gitDir, worktree)

    def candidate(entry: (Text, Int)): Boolean =
      val path = entry(0)
      val wanted = paths == Nil || paths.exists(under(path, _))
      wanted && staged.at(path).absent && (showIgnored || !rules.ignored(path, false))

    val candidates = files(worktree).filter(candidate(_))

    // Without `directories`, untracked files inside untracked directories stay, as git leaves them.
    val trackedDirectories = staged.keys.to[List].bind(ancestors(_))

    def removable(entry: (Text, Int)): Boolean =
      directories || !ancestors(entry(0)).exists(!trackedDirectories.has(_))

    val removals = candidates.filter(removable(_))

    def unlink(entry: (Text, Int)): Unit =
      val target = file(worktree, entry(0))
      if target.existent() then target.delete()
      if directories then pruneEmpty(worktree, target)

    removals.each(unlink(_))
    removals.map(_(0))

  private def ancestors(path: Text): List[Text] =
    def recur(segments: List[Text], prefix: Text, acc: List[Text]): List[Text] = segments match
      case segment :: rest if rest != Nil =>
        val directory = if prefix.length == 0 then segment else t"$prefix/$segment"
        recur(rest, directory, directory :: acc)

      case _ => acc

    recur(path.cut(t"/"), t"", Nil)

  // ──────────────────────────────────────────── Trees ─────────────────────────────────────────

  // Writes the staged tree to the object store: directories become trees, bottom-up.
  def writeTree(gitDir: Path on Linux, objects: Path on Linux)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    val contents = Index.read(gitDir)
    val unmerged = contents.entries.filter(_.stage > 0).map(_.path).distinct
    if unmerged != Nil then abort(Git.Error(Reason.Conflict(unmerged)))
    writeSnapshot(objects, indexSnapshot(contents))

  def writeSnapshot(objects: Path on Linux, snapshot: Snapshot)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    def build(prefix: Text): Git.Hash =
      val under = snapshot.keys.to[List].filter: path => prefix.length == 0 || path.starts(prefix)

      val names = under.map: path => path.skip(prefix.length)
      val slash = t"/"
      val direct = names.filter(!_.contains(slash))

      val directories =
        names.filter(_.contains(slash)).map { name => name.keep(name.s.indexOf('/')) }.distinct

      def fileEntry(name: Text): Git.Object.Tree.Entry =
        val (mode, hash) = snapshot.at(t"$prefix$name").or(abort(Git.Error(Reason.Unexpected)))
        Git.Object.Tree.Entry(treeMode(mode), name, hash)

      def directoryEntry(name: Text): Git.Object.Tree.Entry =
        Git.Object.Tree.Entry(Git.Object.Tree.Mode.Directory, name, build(t"$prefix$name/"))

      val fileEntries = direct.map(fileEntry(_))
      val directoryEntries = directories.map(directoryEntry(_))

      Trees.write(objects, fileEntries + directoryEntries)

    build(t"")

  // Brings the working files from one snapshot to another: files absent from `to` are deleted,
  // files whose entry differs are rewritten. Returns the index entries for `to`.
  def checkoutFiles(objects: Path on Linux, worktree: Path on Linux, from: Snapshot, to: Snapshot)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Index.Entry] =

    def removeAbsent(path: Text): Unit = if to.at(path).absent then
      val target = file(worktree, path)
      if target.existent() then target.delete()
      pruneEmpty(worktree, target)

    from.keys.to[List].each(removeAbsent(_))

    def entry(path: Text): Index.Entry =
      val (mode, hash) = to.at(path).or(abort(Git.Error(Reason.Unexpected)))

      if from.at(path) != Optional((mode, hash)) || !file(worktree, path).existent() then
        writeFile(objects, worktree, path, mode, hash)

      Index.entryFor(worktree, path, hash, mode)

    to.keys.to[List].map(entry(_))

  def readTree
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      mode: ResetMode )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val target = treeSnapshot(objects, headTree(gitDir, commonDir, objects))
    val contents = Index.read(gitDir)

    mode match
      case ResetMode.Soft => ()

      case ResetMode.Mixed =>
        def entry(path: Text): Index.Entry =
          val (mode, hash) = target.at(path).or(abort(Git.Error(Reason.Unexpected)))
          // Stat data is left unverified, so the next status hashes the file.
          Index.Entry(path, mode, hash, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        val entries = target.keys.to[List].map(entry(_))
        Index.write(gitDir, Index.Contents(contents.version, entries, contents.extensions))

      case ResetMode.Hard =>
        val entries =
          checkoutFiles(objects, worktree, workingSnapshot(gitDir, worktree, contents), target)

        Index.write(gitDir, Index.Contents(contents.version, entries, contents.extensions))

      case other => abort(Git.Error(Reason.Unsupported(t"reset ${other.flag}")))

  // Paths whose working or staged content differs from HEAD: a checkout must not overwrite them.
  def modifiedPaths
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Set[Text] =

    status(gitDir, commonDir, objects, worktree, false, false)
    . filter: line => line.index != Git.Status.Untracked
    . map(_.path)
    . to[Set]

  def checkout
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      revision: Text,
      identity: Git.Identity,
      logging: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val branchRef = t"refs/heads/$revision"
    val branch: Optional[Git.Hash] = Refstore.resolve(gitDir, commonDir, branchRef)

    val target =
      branch.or(Revwalk.resolve(gitDir, commonDir, objects, revision)
      . or(abort(Git.Error(Reason.NoSuchRef(revision)))))

    val commit = Revwalk.peel(objects, target)
    val from = treeSnapshot(objects, headTree(gitDir, commonDir, objects))
    val to = treeSnapshot(objects, Revwalk.commit(objects, commit).tree)

    val changing =
      (from.keys.to[List] + to.keys.to[List]).distinct.filter: path => from.at(path) != to.at(path)

    val modified = modifiedPaths(gitDir, commonDir, objects, worktree)
    val clobbered = changing.filter(modified.has(_))
    if clobbered != Nil then abort(Git.Error(Reason.Dirty(clobbered)))

    val contents = Index.read(gitDir)
    val entries = checkoutFiles(objects, worktree, indexSnapshot(contents), to)
    Index.write(gitDir, Index.Contents(contents.version, entries, contents.extensions))

    val previous = Refstore.resolve(gitDir, commonDir, t"HEAD")
    val message = t"checkout: moving from ${describeHead(gitDir, commonDir)} to $revision"

    if branch.present then Refstore.writeSymbolic(gitDir, commonDir, t"HEAD", branchRef)
    else Refstore.detach(gitDir, commonDir, commit, previous, identity, message, logging)

  def describeHead(gitDir: Path on Linux, commonDir: Path on Linux)(using Tactic[Io.Error]): Text =
    Refstore.read(gitDir, commonDir, t"HEAD") match
      case Git.Backend.Target.Symbolic(ref) =>
        if ref.starts(t"refs/heads/") then ref.skip(11) else ref

      case Git.Backend.Target.Direct(hash)  => hash.text

      case _                                => t"HEAD"

  def switch
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      branch: Git.Branch,
      create: Boolean,
      identity: Git.Identity,
      logging: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val ref = t"refs/heads/${branch.text}"

    if create then
      val head =
        Refstore.resolve(gitDir, commonDir, t"HEAD").or(abort(Git.Error(Reason.NoSuchRef(t"HEAD"))))

      Refstore.update
        ( gitDir,
          commonDir,
          ref,
          head,
          Git.Backend.Expectation.Absent,
          t"branch: Created from HEAD",
          identity,
          logging )

      Refstore.writeSymbolic(gitDir, commonDir, t"HEAD", ref)
    else
      if Refstore.resolve(gitDir, commonDir, ref).absent
      then abort(Git.Error(Reason.NoSuchRef(ref)))

      checkout(gitDir, commonDir, objects, worktree, branch.text, identity, logging)

  // ───────────────────────────────────────────── Diffs ────────────────────────────────────────

  private def lines(data: Data): List[Text] =
    val text = Objects.text(data)
    val split = text.cut(t"\n")

    if text.ends(t"\n") then split.reverse match
      case _ :: rest => rest.reverse
      case _         => Nil
    else split

  private def binary(data: Data): Boolean =
    val array = data.asInstanceOf[scala.Array[Byte]]

    def recur
      ( index: Int )

    :   Boolean =

      index < array.length.min(8000) && (array(index) == 0 || recur(index + 1))

    recur(0)

  // Hunks with three lines of context, as git prints them, from a line diff. Edits are walked once,
  // carrying the old and new line counts consumed so far; a hunk opens three edits before a change
  // and closes three after the last change of a run; runs within six lines of each other share it.
  def hunks(before: Data, after: Data): List[Hunk] =
    if binary(before) || binary(after) then Nil else
      val edits = diff(Sequence(lines(before)*), Sequence(lines(after)*)).edits.to(List)

      def isChange(edit: Edit[Text]): Boolean = !edit.isInstanceOf[Par[?]]
      def consumesOld(edit: Edit[Text]): Boolean = !edit.isInstanceOf[Ins[?]]
      def consumesNew(edit: Edit[Text]): Boolean = !edit.isInstanceOf[Del[?]]

      def convert(edit: Edit[Text]): Edit[Text] & Retained = edit match
        case Par(left, right, value) => Par(left + 1, right + 1, value).retained
        case Del(left, value)        => Del(left + 1, value).retained
        case Ins(right, value)       => Ins(right + 1, value).retained

      // How many edits from here precede the next change, or none if no change follows.
      def distanceToChange(remaining: List[Edit[Text]], count: Int): Optional[Int] = remaining match
        case edit :: rest => if isChange(edit) then count else distanceToChange(rest, count + 1)
        case _            => Unset

      def hunk(oldStart: Int, newStart: Int, collected: List[Edit[Text]]): Hunk =
        val oldLines = collected.filter(consumesOld(_)).fold(0): (n, _) => n + 1
        val newLines = collected.filter(consumesNew(_)).fold(0): (n, _) => n + 1

        Hunk
          ( if oldLines == 0 then oldStart else oldStart + 1,
            oldLines,
            if newLines == 0 then newStart else newStart + 1,
            newLines,
            t"",
            collected.map(convert(_)) )

      // The hunk being built: where it began on each side, and its edits, newest first.
      case class Open(oldStart: Int, newStart: Int, collected: List[Edit[Text]])

      def count(list: List[Edit[Text]]): Int = list.fold(0): (n, _) => n + 1

      def firstN(list: List[Edit[Text]], n: Int): List[Edit[Text]] = list match
        case head :: tail if n > 0 => head :: firstN(tail, n - 1)
        case _                     => Nil

      // `context` is the run of unchanged edits since the last change (or the start), newest first.
      def recur
        ( remaining: List[Edit[Text]],
          oldPos:    Int,
          newPos:    Int,
          context:   List[Edit[Text]],
          open:      Optional[Open],
          acc:       List[Hunk] )
      :   List[Hunk] =

        remaining match
          case edit :: rest =>
            val nextOld = if consumesOld(edit) then oldPos + 1 else oldPos
            val nextNew = if consumesNew(edit) then newPos + 1 else newPos

            if isChange(edit) then
              open match
                case Open(start0, start1, collected) =>
                  // Within an open hunk: the context since the last change joins it.
                  recur
                    ( rest,
                      nextOld,
                      nextNew,
                      Nil,
                      Open(start0, start1, (edit :: context) + collected),
                      acc )

                case _ =>
                  // A new hunk opens up to three lines before this change.
                  val leading = firstN(context, 3)
                  val startOld = oldPos - count(leading.filter(consumesOld(_)))
                  val startNew = newPos - count(leading.filter(consumesNew(_)))
                  recur(rest, nextOld, nextNew, Nil, Open(startOld, startNew, edit :: leading), acc)
            else
              open match
                case Open(start0, start1, collected) =>
                  val trailing = edit :: context

                  // Close the hunk when six unchanged lines have passed and no change is near.
                  if count(trailing) >= 6 && distanceToChange(rest, 0).lay(true)(_ >= 0) then
                    val kept = firstN(trailing.reverse, 3).reverse
                    val closed = hunk(start0, start1, (kept + collected).reverse)
                    recur(rest, nextOld, nextNew, Nil, Unset, closed :: acc)
                  else
                    recur(rest, nextOld, nextNew, trailing, open, acc)

                case _ =>
                  recur(rest, nextOld, nextNew, edit :: context, Unset, acc)

          case _ =>
            open match
              case Open(start0, start1, collected) =>
                val kept = firstN(context.reverse, 3).reverse
                (hunk(start0, start1, (kept + collected).reverse) :: acc).reverse

              case _ => acc.reverse

      recur(edits, 0, 0, Nil, Unset, Nil)

  // The differences between two snapshots, with content supplied by the caller for each side.
  def fileDiffs(left: Snapshot, right: Snapshot)(leftContent: (Text, Int, Git.Hash) => Data)
    ( rightContent: (Text, Int, Git.Hash) => Data )
  :   List[FileDiff] =

    val paths = (left.keys.to[List] + right.keys.to[List]).distinct.order(_.s)

    def describe(path: Text): Optional[FileDiff] = left.at(path) match
      case (leftMode: Int, leftHash: Git.Hash) =>
        right.at(path) match
          case (rightMode: Int, rightHash: Git.Hash) =>
            if leftHash == rightHash && leftMode == rightMode then Unset
            else
              val kind =
                if kindOf(leftMode) != kindOf(rightMode) then ChangeKind.TypeChanged
                else ChangeKind.Modified

              val content =
                if leftHash == rightHash then Nil
                else
                  val left = leftContent(path, leftMode, leftHash)
                  val right = rightContent(path, rightMode, rightHash)
                  hunks(left, right)

              FileDiff(path, path, kind, content)

          case _ =>
            FileDiff
              ( path,
                Unset,
                ChangeKind.Deleted,
                hunks(leftContent(path, leftMode, leftHash), Data()) )

      case _ =>
        right.at(path) match
          case (mode: Int, hash: Git.Hash) =>
            FileDiff(Unset, path, ChangeKind.Added, hunks(Data(), rightContent(path, mode, hash)))

          case _ => Unset

    def recur(remaining: List[Text], acc: List[FileDiff]): List[FileDiff] = remaining match
      case path :: rest => describe(path) match
        case diff: FileDiff => recur(rest, diff :: acc)
        case _              => recur(rest, acc)

      case _ => acc.reverse

    recur(paths, Nil)

  def stored(objects: Path on Linux)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   (Text, Int, Git.Hash) => Data =

    (_, _, hash) => Store.readOr(objects, hash)(1)

  def working(worktree: Path on Linux)(using Tactic[Io.Error]): (Text, Int, Git.Hash) => Data =
    (path, mode, _) => content(worktree, path, mode)

  def diffWorktree
    ( gitDir: Path on Linux,
      commonDir: Path on Linux,
      objects: Path on Linux,
      worktree: Path on Linux,
      staged: Boolean,
      against: Optional[Text] )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[FileDiff] =

    val contents = Index.read(gitDir)
    val index = indexSnapshot(contents)

    if staged then
      val head = treeSnapshot(objects, headTree(gitDir, commonDir, objects))
      fileDiffs(head, index)(stored(objects))(stored(objects))
    else
      val base: Snapshot = against match
        case revision: Text =>
          val hash =
            Revwalk.resolve(gitDir, commonDir, objects, revision)
            . or(abort(Git.Error(Reason.NoSuchRef(revision))))
          treeSnapshot(objects, Revwalk.commit(objects, Revwalk.peel(objects, hash)).tree)

        case _ => index

      // Only tracked files take part; each working file is hashed to find whether it changed.
      val present = files(worktree).to[Map]

      def workingEntry(path: Text): List[(Text, (Int, Git.Hash))] =
        present.at(path).lay(Nil): mode => List((path, (mode, hashFile(worktree, path, mode))))

      val workingSnapshot: Snapshot = index.keys.to[List].bind(workingEntry(_)).to[Map]

      fileDiffs(base, workingSnapshot)(stored(objects))(working(worktree))

  def diffTree
    ( objects: Path on Linux, left: Git.Hash, right: Git.Hash )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[FileDiff] =

    def treeOf(hash: Git.Hash): Git.Hash = Store.readOr(objects, hash) match
      case (Git.Object.Kind.Commit, data) => Objects.parseCommit(hash, data).tree
      case (Git.Object.Kind.Tag, _)       => treeOf(Revwalk.peel(objects, hash))
      case _                              => hash

    val leftTree = treeSnapshot(objects, treeOf(left))
    val rightTree = treeSnapshot(objects, treeOf(right))
    fileDiffs(leftTree, rightTree)(stored(objects))(stored(objects))

  def tracked(gitDir: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error]): List[Text] =
    Index.read(gitDir).staged.map(_.path).order(_.s)

  def conflicts(gitDir: Path on Linux)(using Tactic[Git.Error], Tactic[Io.Error]): List[Text] =
    Index.read(gitDir).entries.filter(_.stage > 0).map(_.path).distinct.order(_.s)
