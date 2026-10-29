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

// The operations which move HEAD by replaying or combining commits — merge, cherry-pick, revert,
// rebase and the stash — and the state files in `.git` through which they stop on a conflict
// and are later continued, skipped or abandoned, in the same layout git uses, so that an
// operation begun natively can be finished by `git` and vice versa.
private[octogenarian] object Sequencer:
  import Git.Error.Reason as Reason

  // Where an operation runs: the repository's directories, and who is making the commits.
  case class Context
    ( gitDir:    Path on Linux,
      commonDir: Path on Linux,
      objects:   Path on Linux,
      worktree:  Path on Linux,
      identity:  Git.Identity,
      logging:   Boolean )

  // ─────────────────────────────────────────── State files ────────────────────────────────────

  private def stateFile(context: Context, name: Text): Path on Linux =
    Refstore.descend(context.gitDir, name.cut(t"/"))

  private def readState(context: Context, name: Text)(using Tactic[Io.Error]): Optional[Text] =
    val file = stateFile(context, name)
    if file.existent() then Objects.text(file.read[Data]) else Unset

  private def readHash(context: Context, name: Text)(using Tactic[Io.Error]): Optional[Git.Hash] =
    readState(context, name).let(_.trim).let(Git.Hash.unsafe(_))

  private def readNumber(context: Context, name: Text)(using Tactic[Io.Error]): Int =
    readState(context, name).let(_.trim).let(parseInt(_)).or(0)

  private def parseInt(text: Text): Optional[Int] = safely(text.as[Int])

  private def writeState(context: Context, name: Text, content: Text)
    ( using Tactic[Io.Error] )
  :   Unit =

    val file = stateFile(context, name)
    file.parent.let(Refstore.ensureDirectory(_))
    if file.existent() then file.delete()
    file.write(Objects.bytes(content))

  private def deleteState(context: Context, name: Text)(using Tactic[Io.Error]): Unit =
    val file = stateFile(context, name)
    if file.existent() then file.delete()

  private def present(context: Context, name: Text): Boolean = stateFile(context, name).existent()

  private def count[element](list: List[element]): Int = list.fold(0): (n, _) => n + 1

  // What `.git` records about an operation that has stopped part-way, if any.
  def operation(gitDir: Path on Linux, conflicting: List[Text])
    ( using Tactic[Io.Error] )
  :   Optional[Git.Operation] =

    val context = Context(gitDir, gitDir, gitDir, gitDir, Git.Identity(t"", t""), false)
    val rebaseMerge = gitDir / "rebase-merge"
    val rebaseApply = gitDir / "rebase-apply"

    if rebaseMerge.existent() then
      Git.Operation.Rebasing
        ( readHash(context, t"rebase-merge/onto").or(Git.Hash.zero),
          readHash(context, t"rebase-merge/orig-head").or(Git.Hash.zero),
          readNumber(context, t"rebase-merge/msgnum"),
          readNumber(context, t"rebase-merge/end") - readNumber(context, t"rebase-merge/msgnum"),
          conflicting )
    else if rebaseApply.existent() then
      Git.Operation.Rebasing
        ( readHash(context, t"rebase-apply/onto").or(Git.Hash.zero),
          readHash(context, t"rebase-apply/orig-head").or(Git.Hash.zero),
          readNumber(context, t"rebase-apply/next"),
          readNumber(context, t"rebase-apply/last") - readNumber(context, t"rebase-apply/next"),
          conflicting )
    else
      readHash(context, t"CHERRY_PICK_HEAD").let(Git.Operation.CherryPicking(_, conflicting))
      . or(readHash(context, t"REVERT_HEAD").let(Git.Operation.Reverting(_, conflicting)))
      . or(readState(context, t"MERGE_HEAD").let(mergeHeads(_))
      . let(Git.Operation.Merging(_, conflicting)))
      . or(if present(context, t"BISECT_LOG") then Git.Operation.Bisecting else Unset)

  private def mergeHeads(text: Text): List[Git.Hash] =
    text.cut(t"\n").filter(_.length == 40).map(Git.Hash.unsafe(_))

  private def ensureIdle(context: Context)(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    operation(context.gitDir, Nil).let: operation =>
      abort(Git.Error(Reason.OperationInProgress(operation)))

  // ───────────────────────────────────────────── Commits ──────────────────────────────────────

  private def headCommit(context: Context)(using Tactic[Git.Error], Tactic[Io.Error]): Git.Hash =
    Refstore.resolve(context.gitDir, context.commonDir, t"HEAD")
    . or(abort(Git.Error(Reason.NoSuchRef(t"HEAD"))))

  private def treeOf
    ( context: Context, commit: Optional[Git.Hash] )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Workspace.Snapshot =

    Workspace.treeSnapshot(context.objects, commit.let(Revwalk.commit(context.objects, _).tree))

  private def resolve(context: Context, revision: Text)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    val hash =
      Revwalk.resolve(context.gitDir, context.commonDir, context.objects, revision)
      . or(abort(Git.Error(Reason.NoSuchRef(revision))))

    Revwalk.peel(context.objects, hash)

  private def subject(message: Text): Text = message.cut(t"\n").head

  private def writeCommit
    ( context: Context,
      tree: Git.Hash,
      parents: List[Git.Hash],
      author: Optional[Git.Signature],
      message: Text )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    val committer = Git.Signature(context.identity, Git.Timestamp.now())
    val payload = Objects.serializeCommit(tree, parents, author.or(committer), committer, message)
    Store.write(context.objects, Git.Object.Kind.Commit, payload)

  // Moves HEAD (or, when attached, its branch) from `from` to `to`, logging `message`.
  private def advance(context: Context, from: Git.Hash, to: Git.Hash, message: Text)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    Refstore.update
      ( context.gitDir,
        context.commonDir,
        t"HEAD",
        to,
        Git.Backend.Expectation.Exactly(from),
        message,
        context.identity,
        context.logging )

  // The name of the branch HEAD is on, as git prints it, or `detached HEAD`.
  private def headName(context: Context)(using Tactic[Io.Error]): Text =
    Refstore.read(context.gitDir, context.commonDir, t"HEAD") match
      case Git.Backend.Target.Symbolic(ref) => ref
      case _                                => t"detached HEAD"

  private def shortName(ref: Text): Text = if ref.starts(t"refs/heads/") then ref.skip(11) else ref

  // ──────────────────────────────────────── Index and files ───────────────────────────────────

  // Brings the index and working files to `commit`'s tree, discarding every local change.
  private def resetHard(context: Context, commit: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val contents = Index.read(context.gitDir)
    val working = Workspace.workingSnapshot(context.gitDir, context.worktree, contents)

    val entries =
      Workspace.checkoutFiles(context.objects, context.worktree, working, treeOf(context, commit))

    Index.write(context.gitDir, Index.Contents(contents.version, entries, contents.extensions))

  // Fails if a path which would change between the snapshots has local modifications.
  private def ensureUnmodified(context: Context, from: Workspace.Snapshot, to: Workspace.Snapshot)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val modified =
      Workspace.modifiedPaths(context.gitDir, context.commonDir, context.objects, context.worktree)

    val changing =
      (from.keys.to[List] + to.keys.to[List]).distinct.filter: path => from.at(path) != to.at(path)

    val clobbered = changing.filter(modified.has(_))
    if clobbered != Nil then abort(Git.Error(Reason.Dirty(clobbered)))

  private def ensureClean(context: Context)(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    val modified =
      Workspace.modifiedPaths(context.gitDir, context.commonDir, context.objects, context.worktree)

    if modified != Set() then abort(Git.Error(Reason.Dirty(modified.to[List])))

  // Checks out `to` over `from` in the working files and the index, without moving HEAD.
  private def checkoutTree(context: Context, from: Workspace.Snapshot, to: Workspace.Snapshot)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val contents = Index.read(context.gitDir)
    val entries = Workspace.checkoutFiles(context.objects, context.worktree, from, to)
    Index.write(context.gitDir, Index.Contents(contents.version, entries, contents.extensions))

  // Merges `theirs` into the current index and files, with `base` as the common ancestor;
  // returns the conflicted paths.
  private def mergeInto
    ( context: Context, base: Optional[Git.Hash], ours: Git.Hash, theirs: Git.Hash, oursLabel: Text,
      theirsLabel: Text )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Text] =

    val oursTree = treeOf(context, ours)

    val result =
      Merging.trees
        ( context.objects,
          treeOf(context, base),
          oursTree,
          treeOf(context, theirs),
          oursLabel,
          theirsLabel )

    val conflicted = result.conflicts.map: conflict => (conflict.path, (0, Git.Hash.zero))
    ensureUnmodified(context, oursTree, (conflicted + result.merged.to[List]).to[Map])
    Merging.apply(context.gitDir, context.objects, context.worktree, result)

  // ───────────────────────────────────────────── Merge ────────────────────────────────────────

  def merge
    ( context: Context, revision: Text, ff: FastForward, message: Optional[Text], sign: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Merge.Outcome =

    ensureIdle(context)
    if sign then abort(Git.Error(Reason.Unsupported(t"signing a merge commit natively")))
    val ours = headCommit(context)
    val theirs = resolve(context, revision)

    val isBranch =
      Refstore.resolve(context.gitDir, context.commonDir, t"refs/heads/$revision").present

    val current = shortName(headName(context))

    val described =
      if isBranch then t"branch '$revision'"
      else if revision.length == 40 then t"commit '$revision'"
      else t"commit '$revision'"

    if Revwalk.isAncestor(context.objects, theirs, ours) then Git.Merge.Outcome.UpToDate
    else if ff != FastForward.Never && Revwalk.isAncestor(context.objects, ours, theirs) then
      val from = treeOf(context, ours)
      val to = treeOf(context, theirs)
      ensureUnmodified(context, from, to)
      checkoutTree(context, from, to)
      advance(context, ours, theirs, t"merge $revision: Fast-forward")
      Git.Merge.Outcome.FastForwarded(theirs)
    else if ff == FastForward.Only then
      abort(Git.Error(Reason.NotFastForward))
    else
      val base = Revwalk.mergeBase(context.objects, ours, theirs)
      val conflicts = mergeInto(context, base, ours, theirs, t"HEAD", revision)

      val into =
        if current == t"main" || current == t"master" || current == t"detached HEAD" then t""
        else t" into $current"

      val text = message.or(t"Merge $described$into")

      if conflicts != Nil then
        writeState(context, t"MERGE_HEAD", t"${theirs.text}\n")
        writeState(context, t"MERGE_MODE", if ff == FastForward.Never then t"no-ff" else t"")

        val conflictText = conflicts.map(conflictLine(_)).join
        writeState(context, t"MERGE_MSG", t"$text\n\n# Conflicts:\n$conflictText")

        Git.Merge.Outcome.Stopped(Git.Operation.Merging(List(theirs), conflicts))
      else
        val tree = Workspace.writeTree(context.gitDir, context.objects)
        val commit = writeCommit(context, tree, List(ours, theirs), Unset, text)
        advance(context, ours, commit, t"merge $revision: Merge made by the 'ort' strategy.")
        Git.Merge.Outcome.Merged(commit)

  private def conflictLine(path: Text): Text = t"#\t$path\n"

  // The message in `MERGE_MSG`, without the commented lines git adds.
  private def mergeMessage(context: Context)(using Tactic[Io.Error]): Text =
    readState(context, t"MERGE_MSG").or(t"").cut(t"\n").filter(!_.starts(t"#")).join(t"\n").trim

  private def mergeVerb
    ( context: Context, verb: Git.Sequence.Verb )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    def clear(): Unit =
      deleteState(context, t"MERGE_HEAD")
      deleteState(context, t"MERGE_MSG")
      deleteState(context, t"MERGE_MODE")

    verb match
      case Git.Sequence.Verb.Continue =>
        val conflicts = Workspace.conflicts(context.gitDir)
        if conflicts != Nil then abort(Git.Error(Reason.Conflict(conflicts)))
        val ours = headCommit(context)
        val heads = readState(context, t"MERGE_HEAD").let(mergeHeads(_)).or(Nil)
        val tree = Workspace.writeTree(context.gitDir, context.objects)
        val message = mergeMessage(context)
        val commit = writeCommit(context, tree, ours :: heads, Unset, message)
        advance(context, ours, commit, t"commit (merge): ${subject(message)}")
        clear()
        Git.Sequence.Outcome.Completed(commit)

      case Git.Sequence.Verb.Skip =>
        abort(Git.Error(Reason.Unsupported(t"skipping")))

      case Git.Sequence.Verb.Abort =>
        val head = headCommit(context)
        resetHard(context, head)
        clear()
        Git.Sequence.Outcome.Completed(head)

      case Git.Sequence.Verb.Quit =>
        clear()
        Git.Sequence.Outcome.Completed(headCommit(context))

  // ────────────────────────────────────── Cherry-pick and revert ──────────────────────────────

  // Replays `commit` (or, for a revert, its inverse) onto HEAD: the conflicted paths if it
  // stops, else the new commit, or none if the replay changed nothing.
  private def pick
    ( context: Context, commit: Git.Hash, revert: Boolean, noCommit: Boolean, signoff: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Either[List[Text], Optional[Git.Hash]] =

    val ours = headCommit(context)
    val picked = Revwalk.commit(context.objects, commit)
    val parent = picked.parents.head
    val short = commit.text.keep(7)

    val label =
      if revert then t"parent of $short (${subject(picked.message)})"
      else t"$short (${subject(picked.message)})"

    val message =
      if revert then t"Revert \"${subject(picked.message)}\"\n\nThis reverts commit ${commit.text}."
      else picked.message

    val signed = if signoff then withSignoff(context, message) else message

    val conflicts =
      if revert then
        parent match
          case parent: Git.Hash => mergeInto(context, commit, ours, parent, t"HEAD", label)

          case _ =>
            mergeInto(context, commit, ours, emptyCommit(context), t"HEAD", label)
      else
        mergeInto(context, parent, ours, commit, t"HEAD", label)

    if conflicts != Nil then
      writeState
        ( context, if revert then t"REVERT_HEAD" else t"CHERRY_PICK_HEAD", t"${commit.text}\n" )

      val conflictText = conflicts.map(conflictLine(_)).join
      writeState(context, t"MERGE_MSG", t"$signed\n\n# Conflicts:\n$conflictText")

      Left(conflicts)
    else if noCommit then
      Right(Unset)
    else
      val tree = Workspace.writeTree(context.gitDir, context.objects)

      if tree == Revwalk.commit(context.objects, ours).tree then Right(Unset)
      else
        val author: Optional[Git.Signature] = if revert then Unset else picked.author
        val created = writeCommit(context, tree, List(ours), author, signed)
        val verb = if revert then t"revert" else t"cherry-pick"
        advance(context, ours, created, t"$verb: ${subject(signed)}")
        Right(created)

  // A commit with an empty tree, to revert a root commit against.
  private def emptyCommit(context: Context)(using Tactic[Git.Error], Tactic[Io.Error]): Git.Hash =
    val tree = Trees.write(context.objects, Nil)
    writeCommit(context, tree, Nil, Unset, t"")

  private def withSignoff(context: Context, message: Text): Text =
    val line = t"Signed-off-by: ${context.identity.text}"
    if message.contains(line) then message else t"${message.trim}\n\n$line"

  private def todoLine(context: Context, hash: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Text =

    t"pick ${hash.text} ${subject(Revwalk.commit(context.objects, hash).message)}"

  private def todoHashes(text: Text): List[Git.Hash] =
    text.cut(t"\n").map(_.trim).filter(_.starts(t"pick ")).bind: line =>
      Refstore.nth(line.cut(t" "), 1).let(Git.Hash.unsafe(_)).let(List(_)).or(Nil)

  // Picks each of `commits` in turn, recording the rest in `sequencer/` when there are several,
  // so that a stop can be continued.
  def cherryPick(context: Context, commits: List[Git.Hash], revert: Boolean, noCommit: Boolean)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    ensureIdle(context)
    val head = headCommit(context)

    commits match
      case _ :: _ :: _ if !noCommit =>
        writeState(context, t"sequencer/head", t"${head.text}\n")

        writeState
          ( context, t"sequencer/todo", t"${commits.map(todoLine(context, _)).join(t"\n")}\n" )

        if revert then writeState(context, t"sequencer/opts", t"") else ()

      case _ => ()

    runPicks(context, commits, revert, noCommit)

  private def runPicks
    ( context: Context, commits: List[Git.Hash], revert: Boolean, noCommit: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    commits match
      case commit :: rest =>
        pick(context, commit, revert, noCommit, false) match
          case Left(conflicts) =>
            val operation =
              if revert then Git.Operation.Reverting(commit, conflicts)
              else Git.Operation.CherryPicking(commit, conflicts)

            Git.Sequence.Outcome.Stopped(operation)

          case Right(created) =>
            if created.absent && !noCommit then abort(Git.Error(Reason.NothingToCommit))

            if present(context, t"sequencer") then
              writeState
                ( context, t"sequencer/todo", t"${rest.map(todoLine(context, _)).join(t"\n")}\n" )

            runPicks(context, rest, revert, noCommit)

      case _ =>
        clearSequencer(context)
        Git.Sequence.Outcome.Completed(headCommit(context))

  private def clearSequencer(context: Context)(using Tactic[Io.Error]): Unit =
    deleteState(context, t"CHERRY_PICK_HEAD")
    deleteState(context, t"REVERT_HEAD")
    deleteState(context, t"MERGE_MSG")
    val sequencer = context.gitDir / "sequencer"
    if sequencer.existent() then sequencer.delete()

  private def pickVerb
    ( context: Context, verb: Git.Sequence.Verb )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    val revert = present(context, t"REVERT_HEAD")
    val stopped = readHash(context, t"CHERRY_PICK_HEAD").or(readHash(context, t"REVERT_HEAD"))
    val remaining = readState(context, t"sequencer/todo").let(todoHashes(_)).or(Nil)

    // The picks still to do once the stopped one is dealt with: the todo list, less its head.
    val rest = remaining match
      case first :: rest if stopped == Optional(first) => rest
      case other                                       => other

    verb match
      case Git.Sequence.Verb.Continue =>
        val conflicts = Workspace.conflicts(context.gitDir)
        if conflicts != Nil then abort(Git.Error(Reason.Conflict(conflicts)))

        stopped.let: commit =>
          val ours = headCommit(context)
          val tree = Workspace.writeTree(context.gitDir, context.objects)
          val message = mergeMessage(context)

          val author: Optional[Git.Signature] =
            if revert then Unset else Revwalk.commit(context.objects, commit).author

          val created = writeCommit(context, tree, List(ours), author, message)

          val name = if revert then t"revert" else t"cherry-pick"
          advance(context, ours, created, t"$name: ${subject(message)}")

        deleteState(context, t"CHERRY_PICK_HEAD")
        deleteState(context, t"REVERT_HEAD")
        deleteState(context, t"MERGE_MSG")
        runPicks(context, rest, revert, false)

      case Git.Sequence.Verb.Skip =>
        resetHard(context, headCommit(context))
        deleteState(context, t"CHERRY_PICK_HEAD")
        deleteState(context, t"REVERT_HEAD")
        deleteState(context, t"MERGE_MSG")
        runPicks(context, rest, revert, false)

      case Git.Sequence.Verb.Abort =>
        val head = headCommit(context)
        val original = readHash(context, t"sequencer/head").or(head)
        resetHard(context, original)
        if original != head then advance(context, head, original, t"cherry-pick: aborting")
        clearSequencer(context)
        Git.Sequence.Outcome.Completed(original)

      case Git.Sequence.Verb.Quit =>
        clearSequencer(context)
        Git.Sequence.Outcome.Completed(headCommit(context))

  // ───────────────────────────────────────────── Rebase ───────────────────────────────────────

  private val rebaseDir: Text = t"rebase-merge"

  def rebase
    ( context: Context, onto: Text, upstream: Optional[Text], options: List[Git.Rebase.Option] )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    ensureIdle(context)

    if options.has(Git.Rebase.Option.Autosquash)
    then abort(Git.Error(Reason.Unsupported(t"rebase --autosquash")))

    val ontoHash = resolve(context, onto)
    val upstreamHash = upstream.lay(ontoHash)(resolve(context, _))
    val original = headCommit(context)
    val name = headName(context)

    val autostash: Optional[Git.Hash] =
      if options.has(Git.Rebase.Option.Autostash) then stash(context, Unset, false, false)
      else Unset

    if autostash.absent then ensureClean(context)

    def linear(hash: Git.Hash): Boolean = Revwalk.commit(context.objects, hash).parents match
      case _ :: _ :: _ => false
      case _           => true

    val commits =
      Revwalk.walk(context.objects, original, upstreamHash, Unset, Nil).reverse.filter(linear(_))

    val outcome =
      if commits == Nil then
        // Nothing to replay: the branch moves to `onto`, a fast-forward.
        if ontoHash != original then
          val from = treeOf(context, original)
          val to = treeOf(context, ontoHash)
          checkoutTree(context, from, to)
          advance(context, original, ontoHash, t"rebase (finish): $name onto ${ontoHash.text}")

        Git.Sequence.Outcome.Completed(ontoHash)
      else
        writeState(context, t"$rebaseDir/head-name", t"$name\n")
        writeState(context, t"$rebaseDir/onto", t"${ontoHash.text}\n")
        writeState(context, t"$rebaseDir/orig-head", t"${original.text}\n")
        writeState(context, t"$rebaseDir/interactive", t"")
        writeState(context, t"$rebaseDir/quiet", t"")
        writeState(context, t"$rebaseDir/msgnum", t"0\n")
        writeState(context, t"$rebaseDir/end", t"${count(commits)}\n")

        writeState
          ( context,
            t"$rebaseDir/git-rebase-todo",
            t"${commits.map(todoLine(context, _)).join(t"\n")}\n" )

        writeState(context, t"$rebaseDir/done", t"")

        if options.has(Git.Rebase.Option.KeepEmpty)
        then writeState(context, t"$rebaseDir/keep_redundant_commits", t"")

        if options.has(Git.Rebase.Option.Signoff)
        then writeState(context, t"$rebaseDir/signoff", t"")

        // Detach at `onto`, then replay.
        val from = treeOf(context, original)
        val to = treeOf(context, ontoHash)
        checkoutTree(context, from, to)

        Refstore.detach
          ( context.gitDir, context.commonDir, ontoHash, original, context.identity,
            t"rebase (start): checkout $onto", context.logging )

        runRebase(context)

    outcome match
      case Git.Sequence.Outcome.Completed(_) =>
        autostash.let: _ => applyStash(context, 0, false, true)
        outcome

      case _ => outcome

  private def rebaseOptionSet
    ( context: Context, name: Text )
  :   Boolean =

    present(context, t"$rebaseDir/$name")

  // Replays the picks left in the todo list, stopping at the first conflict.
  private def runRebase(context: Context)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    val todo = readState(context, t"$rebaseDir/git-rebase-todo").let(todoHashes(_)).or(Nil)
    val onto = readHash(context, t"$rebaseDir/onto").or(Git.Hash.zero)
    val original = readHash(context, t"$rebaseDir/orig-head").or(Git.Hash.zero)
    val total = readNumber(context, t"$rebaseDir/end")
    val keepEmpty = rebaseOptionSet(context, t"keep_redundant_commits")
    val signoff = rebaseOptionSet(context, t"signoff")

    todo match
      case commit :: rest =>
        val done = readNumber(context, t"$rebaseDir/msgnum") + 1
        writeState(context, t"$rebaseDir/msgnum", t"$done\n")

        pick(context, commit, false, false, signoff) match
          case Left(conflicts) =>
            // As git does, the stopped command is already in `done`; `stopped-sha` names it.
            val picked = Revwalk.commit(context.objects, commit)
            finishPick(context, commit, rest)
            writeState(context, t"$rebaseDir/stopped-sha", t"${commit.text}\n")
            writeState(context, t"$rebaseDir/message", t"${picked.message}\n")
            writeState(context, t"$rebaseDir/author-script", authorScript(picked.author))
            val operation = Git.Operation.Rebasing(onto, original, done, total - done, conflicts)
            Git.Sequence.Outcome.Stopped(operation)

          case Right(created) =>
            if created.absent && keepEmpty then
              val ours = headCommit(context)
              val picked = Revwalk.commit(context.objects, commit)
              val tree = Revwalk.commit(context.objects, ours).tree
              val empty = writeCommit(context, tree, List(ours), picked.author, picked.message)
              advance(context, ours, empty, t"rebase (pick): ${subject(picked.message)}")

            finishPick(context, commit, rest)
            runRebase(context)

      case _ => finishRebase(context)

  // Moves a pick from the todo list to the done list.
  private def finishPick(context: Context, commit: Git.Hash, rest: List[Git.Hash])
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Unit =

    val done = readState(context, t"$rebaseDir/done").or(t"")
    writeState(context, t"$rebaseDir/done", t"$done${todoLine(context, commit)}\n")

    writeState
      ( context, t"$rebaseDir/git-rebase-todo", t"${rest.map(todoLine(context, _)).join(t"\n")}\n" )

  private def authorScript(author: Git.Signature): Text =
    val name = t"GIT_AUTHOR_NAME='${author.identity.name}'"
    val email = t"GIT_AUTHOR_EMAIL='${author.identity.email}'"
    val date = t"GIT_AUTHOR_DATE='@${author.timestamp.text}'"
    t"$name\n$email\n$date\n"

  // Reattaches HEAD to the rebased branch, moving it to the replayed tip.
  private def finishRebase(context: Context)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    val head = headCommit(context)
    val name = readState(context, t"$rebaseDir/head-name").let(_.trim).or(t"detached HEAD")
    val original = readHash(context, t"$rebaseDir/orig-head").or(head)
    val onto = readHash(context, t"$rebaseDir/onto").or(head)

    if name.starts(t"refs/") then
      Refstore.update
        ( context.gitDir, context.commonDir, name, head, Git.Backend.Expectation.Exactly(original),
          t"rebase (finish): $name onto ${onto.text}", context.identity, context.logging )

      Refstore.writeSymbolic(context.gitDir, context.commonDir, t"HEAD", name)

    clearRebase(context)
    Git.Sequence.Outcome.Completed(head)

  private def clearRebase(context: Context)(using Tactic[Io.Error]): Unit =
    val directory = context.gitDir / "rebase-merge"
    if directory.existent() then directory.delete()

  private def rebaseVerb
    ( context: Context, verb: Git.Sequence.Verb )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    val stopped = readHash(context, t"$rebaseDir/stopped-sha")

    def clearStopped(): Unit =
      deleteState(context, t"$rebaseDir/stopped-sha")
      deleteState(context, t"$rebaseDir/message")
      deleteState(context, t"$rebaseDir/author-script")

    verb match
      case Git.Sequence.Verb.Continue =>
        val conflicts = Workspace.conflicts(context.gitDir)
        if conflicts != Nil then abort(Git.Error(Reason.Conflict(conflicts)))

        stopped.let: commit =>
          val ours = headCommit(context)
          val tree = Workspace.writeTree(context.gitDir, context.objects)
          val picked = Revwalk.commit(context.objects, commit)
          val message = readState(context, t"$rebaseDir/message").or(picked.message).trim

          if tree != Revwalk.commit(context.objects, ours).tree then
            val created = writeCommit(context, tree, List(ours), picked.author, message)
            advance(context, ours, created, t"rebase (continue): ${subject(message)}")

        clearStopped()
        runRebase(context)

      case Git.Sequence.Verb.Skip =>
        resetHard(context, headCommit(context))
        clearStopped()
        runRebase(context)

      case Git.Sequence.Verb.Abort =>
        val head = headCommit(context)
        val original = readHash(context, t"$rebaseDir/orig-head").or(head)
        val name = readState(context, t"$rebaseDir/head-name").let(_.trim).or(t"detached HEAD")
        resetHard(context, original)

        if name.starts(t"refs/")
        then Refstore.writeSymbolic(context.gitDir, context.commonDir, t"HEAD", name)
        else
          Refstore.detach
            ( context.gitDir,
              context.commonDir,
              original,
              head,
              context.identity,
              t"rebase (abort): updating HEAD",
              context.logging )

        clearRebase(context)
        Git.Sequence.Outcome.Completed(original)

      case Git.Sequence.Verb.Quit =>
        clearRebase(context)
        Git.Sequence.Outcome.Completed(headCommit(context))

  // Continues, skips, abandons or quits whichever operation is in progress.
  def sequence(context: Context, verb: Git.Sequence.Verb)(using Tactic[Git.Error], Tactic[Io.Error])
  :   Git.Sequence.Outcome =

    val picking =
      present(context, t"CHERRY_PICK_HEAD") || present(context, t"REVERT_HEAD") ||
        present(context, t"sequencer")

    if present(context, rebaseDir) then rebaseVerb(context, verb)
    else if present(context, t"rebase-apply")
    then abort(Git.Error(Reason.Unsupported(t"continuing an `am`-style rebase")))
    else if picking then pickVerb(context, verb)
    else if present(context, t"MERGE_HEAD") then mergeVerb(context, verb)
    else abort(Git.Error(Reason.NoOperationInProgress))

  // ───────────────────────────────────────────── Stash ────────────────────────────────────────

  private def stashRef: Text = t"refs/stash"

  // Records the index and working files as commits under `refs/stash` and resets them, as
  // `git stash push` does: none if there is nothing to save.
  def stash
    ( context: Context, message: Optional[Text], includeUntracked: Boolean, keepIndex: Boolean )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Git.Hash] =

    val head = headCommit(context)
    val contents = Index.read(context.gitDir)

    if contents.entries.exists(_.stage > 0)
    then abort(Git.Error(Reason.Unmerged(Git.Branch.unsafe(t"HEAD"), head)))

    val lines =
      Workspace.status
        ( context.gitDir,
          context.commonDir,
          context.objects,
          context.worktree,
          false,
          includeUntracked )

    val untrackedPaths = lines.filter(_.index == Optional(Git.Status.Untracked)).map(_.path)
    val changed = lines.filter(_.index != Optional(Git.Status.Untracked))

    if changed == Nil && untrackedPaths == Nil then Unset else
      val branch = shortName(headName(context))
      val branchName = if branch == t"detached HEAD" then t"(no branch)" else branch
      val headCommitObject = Revwalk.commit(context.objects, head)
      val description = t"$branchName: ${head.text.keep(7)} ${subject(headCommitObject.message)}"
      val indexTree = Workspace.writeTree(context.gitDir, context.objects)
      val indexCommit = writeCommit(context, indexTree, List(head), Unset, t"index on $description")

      // The working files as they are, for every path the index tracks that still exists.
      def working(entry: Index.Entry): Optional[(Text, (Int, Git.Hash))] =
        val file = Workspace.file(context.worktree, entry.path)

        if !file.existent() then Unset else
          val mode = fileMode(file, entry.mode)
          val data = Workspace.content(context.worktree, entry.path, mode)
          (entry.path, (mode, Store.write(context.objects, Git.Object.Kind.Blob, data)))

      val workingSnapshot =
        contents.staged.bind { entry => working(entry).let(List(_)).or(Nil) }.to[Map]

      val workingTree = Workspace.writeSnapshot(context.objects, workingSnapshot)

      def untrackedEntry(path: Text): (Text, (Int, Git.Hash)) =
        val file = Workspace.file(context.worktree, path)
        val mode = fileMode(file, Index.regular)
        val data = Workspace.content(context.worktree, path, mode)
        (path, (mode, Store.write(context.objects, Git.Object.Kind.Blob, data)))

      val untrackedCommit: Optional[Git.Hash] =
        if untrackedPaths == Nil then Unset else
          val tree =
            Workspace.writeSnapshot(context.objects, untrackedPaths.map(untrackedEntry(_)).to[Map])

          writeCommit(context, tree, Nil, Unset, t"untracked files on $description")

      val text = message.lay(t"WIP on $description"): message => t"On $branchName: $message"
      val parents = List(head, indexCommit) + untrackedCommit.let(List(_)).or(Nil)
      val stashCommit = writeCommit(context, workingTree, parents, Unset, text)

      Refstore.update
        ( context.gitDir,
          context.commonDir,
          stashRef,
          stashCommit,
          Git.Backend.Expectation.Unconditional,
          text,
          context.identity,
          true )

      // Then put everything back.
      if keepIndex then checkoutTree(context, workingSnapshot, Workspace.indexSnapshot(contents))
      else resetHard(context, head)

      def unlink(path: Text): Unit =
        val file = Workspace.file(context.worktree, path)
        if file.existent() then file.delete()
        Workspace.pruneEmpty(context.worktree, file)

      untrackedPaths.each(unlink(_))
      stashCommit

  private def fileMode(file: Path on Linux, default: Int)(using Tactic[Io.Error]): Int =
    if default == Index.symlink || default == Index.gitlink then default
    else if file.executable() then Index.executable
    else Index.regular

  private def stashEntry(context: Context, entry: Int)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    Refstore.nth(Refstore.reflog(context.gitDir, context.commonDir, stashRef), entry)
    . or(abort(Git.Error(Reason.NoSuchStash)))
    . hash

  // Replays a stash onto the working files (and, with `index`, the index), dropping it if asked.
  def applyStash(context: Context, entry: Int, index: Boolean, drop: Boolean)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Sequence.Outcome =

    val stashHash = stashEntry(context, entry)
    val stashCommit = Revwalk.commit(context.objects, stashHash)
    val base = stashCommit.parents.head.or(abort(Git.Error(Reason.NoSuchStash)))
    val indexCommit = Refstore.nth(stashCommit.parents, 1)
    val untrackedCommit = Refstore.nth(stashCommit.parents, 2)
    val contents = Index.read(context.gitDir)
    val ours = Workspace.indexSnapshot(contents)
    val baseTree = treeOf(context, base)
    val theirs = treeOf(context, stashHash)

    val result =
      Merging.trees
        ( context.objects, baseTree, ours, theirs, t"Updated upstream", t"Stashed changes" )

    val conflicted = result.conflicts.map: conflict => (conflict.path, (0, Git.Hash.zero))

    ensureUnmodified(context, ours, (conflicted + result.merged.to[List]).to[Map])

    // Untracked files are restored only where nothing stands in their way.
    val untracked = treeOf(context, untrackedCommit)

    val blocking =
      untracked.keys.to[List].filter: path => Workspace.file(context.worktree, path).existent()

    if blocking != Nil then abort(Git.Error(Reason.Dirty(blocking)))

    val conflicts = Merging.apply(context.gitDir, context.objects, context.worktree, result)
    if conflicts != Nil then abort(Git.Error(Reason.Conflict(conflicts)))

    // Without `index`, a path the index already knew is left as a working-file change; with it,
    // the index is restored as it was stashed.
    val applied = Index.read(context.gitDir)
    val indexSnapshot = if index then treeOf(context, indexCommit) else ours

    def restage(entry: Index.Entry): Index.Entry =
      def changed(mode: Int, hash: Git.Hash): Boolean =
        index || result.merged.at(entry.path) != Optional((mode, hash))

      indexSnapshot.at(entry.path) match
        case (mode: Int, hash: Git.Hash) if changed(mode, hash) =>
          Index.Entry(entry.path, mode, hash, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        case _ => entry

    Index.write(context.gitDir, applied.copy(entries = applied.entries.map(restage(_))))

    def restore(path: Text): Unit = untracked.at(path).let: (mode, hash) =>
      Workspace.writeFile(context.objects, context.worktree, path, mode, hash)

    untracked.keys.to[List].each(restore(_))
    if drop then dropStash(context, entry)
    Git.Sequence.Outcome.Completed(headCommit(context))

  // Removes an entry from the stash's reflog, moving `refs/stash` to the newest that remains.
  def dropStash(context: Context, entry: Int)(using Tactic[Git.Error], Tactic[Io.Error]): Unit =
    val file = Refstore.descend(context.commonDir, List(t"logs", t"refs", t"stash"))
    val ref = Refstore.location(context.gitDir, context.commonDir, stashRef)

    val lines =
      if file.existent() then Objects.text(file.read[Data]).cut(t"\n").filter(_.length > 0) else Nil

    val total = count(lines)

    if entry < 0 || entry >= total then abort(Git.Error(Reason.NoSuchStash))
    val position = total - 1 - entry

    def without(remaining: List[Text], index: Int, acc: List[Text]): List[Text] = remaining match
      case line :: rest =>
        val kept = if index == position then acc else line :: acc
        without(rest, index + 1, kept)

      case _            => acc.reverse

    val kept = without(lines, 0, Nil)

    Refstore.lock(ref): lockFile =>
      if kept == Nil then
        if file.existent() then file.delete()
        if ref.existent() then ref.delete()
      else
        file.delete()
        file.write(Objects.bytes(t"${kept.join(t"\n")}\n"))

        val newest =
          kept.reverse.head.let(_.cut(t" ")).let(Refstore.nth(_, 1)).or(Git.Hash.zero.text)

        lockFile.write(Objects.bytes(t"$newest\n"))

        lockFile.moveTo(ref)
