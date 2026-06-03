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
package octogenarian

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import enigmatic.*
import fulminate.*
import galilei.*
import gossamer.*
import guillotine.*
import kaleidoscope.*
import nomenclature.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

import GitError.Reason.*

object GitRepo:
  def at[abstractable: Abstractable across Paths to Text](path: abstractable)
  :   GitRepo raises PathError raises NameError raises GitError raises IoError =

    unsafely(path.generic.decode[Path on Linux]).pipe: path =>
      if !path.exists() then abort(GitError(RepoDoesNotExist))

      if (path / ".git").exists() then GitRepo((path / ".git"))
      else GitRepo(path)


case class GitRepo(gitDir: Path on Linux):
  val repoOptions = sh"--git-dir=$gitDir"


  def pushTags()(using Internet, GitCommand, WorkingDirectory)
  :   Unit logs GitEvent raises GitError raises ExecError =

    sh"$git $repoOptions push --tags".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(PushFailed))


  def push()(using Internet, Tactic[GitError], GitCommand, WorkingDirectory, Tactic[ExecError])
  :   Unit logs GitEvent =

    sh"$git $repoOptions push".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(PushFailed))


  def fetch(depth: Optional[Int] = Unset, repo: Text, refspec: Refspec)
    ( using GitCommand, Internet, WorkingDirectory )
    ( using gitError: Tactic[GitError], exec: Tactic[ExecError] )
  :   GitProcess[Unit] logs GitEvent =

    val depthOption = depth.lay(sh""): depth => sh"--depth=$depth"
    val command = sh"$git $repoOptions fetch $depthOption --progress $repo $refspec"
    val process = command.fork[Exit]()

    GitProcess[Unit](Git.progress(process)):
      process.await() match
        case Exit.Ok => ()
        case failure => abort(GitError(PullFailed))


  object config:
    def get[value: Decodable in Text](variable: Text)
      ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
    :   value logs GitEvent =

      sh"$git $repoOptions config --get $variable".exec[Text]().decode[value]

  def tags()(using GitCommand, WorkingDirectory, Tactic[ExecError]): List[GitTag] logs GitEvent =
    sh"$git $repoOptions tag".exec[Stream[Text]]().to(List).map(GitTag.unsafe(_))


  def tag(name: GitTag)(using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError])
  :   GitTag logs GitEvent =

    sh"$git $repoOptions tag $name".exec[Exit]() match
      case Exit.Ok => name
      case failure => abort(GitError(TagFailed))


  def deleteTag(name: GitTag)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions tag -d $name".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(TagFailed))


  def deleteBranch(branch: GitBranch, force: Boolean = false)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    val flag = if force then sh"-D" else sh"-d"

    sh"$git $repoOptions branch $flag $branch".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(BranchFailed))


  def renameBranch(from: GitBranch, to: GitBranch, force: Boolean = false)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    val flag = if force then sh"-M" else sh"-m"

    sh"$git $repoOptions branch $flag $from $to".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(BranchFailed))


  def remotes()(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   List[Remote] logs GitEvent =

    val lines = sh"$git $repoOptions remote -v".exec[Stream[Text]]()

    val grouped = lines.collect:
      case r"$name(\S+)\t$url(\S+) \($kind(fetch|push)\)" => (name, url, kind)

    grouped.to(List).groupBy(_._1).to[List].map: (name, rows) =>
      val fetch = rows.collectFirst { case (_, url, t"fetch") => url }.getOrElse(t"")
      val push  = rows.collectFirst { case (_, url, t"push")  => url }
      Remote(name, fetch, push.getOrElse(Unset))


  def addRemote(name: Text, url: Text)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Remote logs GitEvent =

    sh"$git $repoOptions remote add $name $url".exec[Exit]() match
      case Exit.Ok => Remote(name, url)
      case failure => abort(GitError(RemoteFailed))


  def removeRemote(name: Text)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions remote remove $name".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(RemoteFailed))


  private def parsePem(text: Text): Optional[Pem] = safely(Pem.parse(text))

  def log()(using GitCommand, WorkingDirectory, Tactic[ExecError]): Stream[Commit] logs GitEvent =
    def recur
      ( stream:    Stream[Text],
        hash:      Optional[GitHash] = Unset,
        tree:      Optional[GitHash] = Unset,
        parents:   List[GitHash]     = Nil,
        author:    Optional[Text]    = Unset,
        committer: Optional[Text]    = Unset,
        signature: List[Text]        = Nil,
        lines:     List[Text]        = Nil )
    :   Stream[Commit] =

      def commit(): Stream[Commit] =
        if hash.absent || tree.absent || author.absent || committer.absent then Stream()
        else unsafely:
          val pem = parsePem(signature.join(t"\n"))

          Stream:
            Commit
              ( hash.vouch,
                tree.vouch,
                parents.reverse,
                author.vouch,
                committer.vouch,
                pem,
                lines.reverse )

      def read(stream: Stream[Text], lines: List[Text]): (List[Text], Stream[Text]) =
        stream match
          case r" $line(.*)" #:: tail => read(tail, line :: lines)
          case _                      => (lines.reverse, stream)

      stream match
        case head #:: tail =>
          head match
            case t"" =>
              recur(tail, hash, tree, parents, author, committer, signature, lines)

            case r"commit $hash(.{40})" =>
              commit() #::: recur(tail, GitHash.unsafe(hash), Unset, Nil, Unset, Unset, Nil, Nil)

            case r"tree $tree(.{40})" =>
              recur(tail, hash, GitHash.unsafe(tree), parents, author, committer, signature, lines)

            case r"parent $parent(.{40})" =>
              val parents2 = GitHash.unsafe(parent) :: parents
              recur(tail, hash, tree, parents2, author, committer, signature, lines)

            case r"author $author(.*) $timestamp([0-9]+) $time(.....)" =>
              recur(tail, hash, tree, parents, author, committer, signature, lines)

            case r"committer $committer(.*) $timestamp([0-9]+) $time(.....)" =>
              recur(tail, hash, tree, parents, author, committer, signature, lines)

            case r"gpgsig $start(.*)" =>
              val (signature, rest) = read(tail, Nil)
              recur(rest, hash, tree, parents, author, committer, start :: signature, lines)

            case r"    $line(.*)" =>
              recur(tail, hash, tree, parents, author, committer, signature, line :: lines)

            case other =>
              recur(tail, hash, tree, parents, author, committer, signature, lines)

        case _ =>
          commit()

    recur(sh"$git $repoOptions log --format=raw --color=never".exec[Stream[Text]]())


  def diff(refA: Refspec, refB: Refspec)
    ( using GitCommand, WorkingDirectory, Tactic[ExecError] )
  :   Stream[FileDiff] logs GitEvent =

    Patch.parse(sh"$git $repoOptions diff --no-color $refA $refB".exec[Stream[Text]]())


  def reflog(ref: Optional[Refspec] = Unset)
    ( using GitCommand, WorkingDirectory, Tactic[ExecError] )
  :   Stream[ReflogEntry] logs GitEvent =

    val refArg = ref.lay(sh""): ref => sh"$ref"
    val format = t"--format=%H %gd %ct %gs"

    sh"$git $repoOptions reflog show $format $refArg".exec[Stream[Text]]().collect:
      case r"$hash([a-f0-9]{40}) $selector(\S+) $time([0-9]+) $message(.*)" =>
        ReflogEntry(GitHash.unsafe(hash), selector, time.s.toLong, message)


  def revParse(refspec: Refspec)(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   GitHash logs GitEvent =

    GitHash.unsafe(sh"$git $repoOptions rev-parse $refspec".exec[Text]().trim)


  object notes:
    def show(target: GitHash, ref: Path on GitRefs = GitRefs.defaultNotes)
      ( using GitCommand, WorkingDirectory, Tactic[ExecError] )
    :   Optional[Text] logs GitEvent =

      val refArg = sh"--ref=${ref.encode}"

      sh"$git $repoOptions notes $refArg show $target".exec[Exit]() match
        case Exit.Ok =>
          // `git notes show` appends a trailing newline to its output that is
          // not part of the stored note; strip it to round-trip cleanly with
          // bodies passed in to `add` / `append`.
          val raw = sh"$git $repoOptions notes $refArg show $target".exec[Text]()
          if raw.ends(t"\n") then raw.skip(1, Rtl) else raw

        case _ =>
          Unset


    def add
      ( target: GitHash, body: Text, force: Boolean = false,
        ref:    Path on GitRefs = GitRefs.defaultNotes )
      ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
    :   Unit logs GitEvent =

      val refArg   = sh"--ref=${ref.encode}"
      val forceOpt = if force then sh"-f" else sh""

      sh"$git $repoOptions notes $refArg add $forceOpt -m $body $target".exec[Exit]() match
        case Exit.Ok => ()
        case _       => abort(GitError(NotesFailed))


    def append
      ( target: GitHash, body: Text, ref: Path on GitRefs = GitRefs.defaultNotes )
      ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
    :   Unit logs GitEvent =

      val refArg = sh"--ref=${ref.encode}"

      sh"$git $repoOptions notes $refArg append -m $body $target".exec[Exit]() match
        case Exit.Ok => ()
        case _       => abort(GitError(NotesFailed))


    def remove
      ( target: GitHash, ignoreMissing: Boolean = false,
        ref:    Path on GitRefs = GitRefs.defaultNotes )
      ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
    :   Unit logs GitEvent =

      val refArg     = sh"--ref=${ref.encode}"
      val missingOpt = if ignoreMissing then sh"--ignore-missing" else sh""

      sh"$git $repoOptions notes $refArg remove $missingOpt $target".exec[Exit]() match
        case Exit.Ok => ()
        case _       => abort(GitError(NotesFailed))


    def list(ref: Path on GitRefs = GitRefs.defaultNotes)
      ( using GitCommand, WorkingDirectory, Tactic[ExecError] )
    :   Stream[(GitHash, GitHash)] logs GitEvent =

      val refArg = sh"--ref=${ref.encode}"

      sh"$git $repoOptions notes $refArg list".exec[Stream[Text]]().collect:
        case r"$noteHash([a-f0-9]{40}) $target([a-f0-9]{40})" =>
          (GitHash.unsafe(noteHash), GitHash.unsafe(target))


    def copy
      ( from: GitHash, to: GitHash, force: Boolean = false,
        ref:  Path on GitRefs = GitRefs.defaultNotes )
      ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
    :   Unit logs GitEvent =

      val refArg   = sh"--ref=${ref.encode}"
      val forceOpt = if force then sh"-f" else sh""

      sh"$git $repoOptions notes $refArg copy $forceOpt $from $to".exec[Exit]() match
        case Exit.Ok => ()
        case _       => abort(GitError(NotesFailed))


  // Lists every non-bare worktree attached to this object database.
  def worktrees()(using GitCommand, WorkingDirectory, Tactic[ExecError])
  :   List[Worktree] logs GitEvent raises GitError =

    val lines = sh"$git $repoOptions worktree list --porcelain".exec[List[Text]]()

    // Each worktree block is separated by an empty line. Split, then keep
    // only the non-bare entries (a `bare` line indicates a bare worktree
    // entry, which has no working tree).
    def blocks(remaining: List[Text]): List[List[Text]] = remaining match
      case Nil => Nil

      case _ =>
        val (block, rest) = remaining.span(_ != t"")
        block :: blocks(rest.dropWhile(_ == t""))

    blocks(lines).flatMap: block =>
      val isBare = block.scala.contains(t"bare")

      block.collect:
        case r"worktree $path(.*)" if !isBare =>
          val pathOnLinux = unsafely(path.decode[Path on Linux])
          Worktree(this, pathOnLinux)


  def addWorktree
    [ path: Abstractable across Paths to Text ]
    ( target: path, ref: Refspec, detach: Boolean = false )
    ( using WorkingDirectory,
            Tactic[GitError],
            (Path on Linux) is Decodable in Text,
            Tactic[ExecError],
            GitCommand )
  :   Worktree logs GitEvent raises NameError raises PathError =

    val targetPath: Path on Linux =
      try target.generic.decode[Path on Linux]
      catch case error: PathError => abort(GitError(WorktreeFailed))

    val detachOpt = if detach then sh"--detach" else sh""

    sh"$git $repoOptions worktree add $detachOpt $targetPath $ref".exec[Exit]() match
      case Exit.Ok => Worktree(this, targetPath)
      case failure => abort(GitError(WorktreeFailed))


  def removeWorktree(worktree: Worktree, force: Boolean = false)
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    val forceOpt = if force then sh"--force" else sh""

    sh"$git $repoOptions worktree remove $forceOpt ${worktree.path}".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(WorktreeFailed))


  def pruneWorktrees()
    ( using GitCommand, WorkingDirectory, Tactic[GitError], Tactic[ExecError] )
  :   Unit logs GitEvent =

    sh"$git $repoOptions worktree prune".exec[Exit]() match
      case Exit.Ok => ()
      case failure => abort(GitError(WorktreeFailed))
