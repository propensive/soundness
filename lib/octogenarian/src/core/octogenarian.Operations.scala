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

import scala.caps

import anticipation.*
import aperture.*
import contingency.*
import distillate.*
import galilei.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import serpentine.*
import urticose.*
import vacuous.*

// The logic shared by both backends, written once over the backend's plumbing: compare-and-swap
// commits, notes, remotes read from configuration, and the bookkeeping of the handles.
private[octogenarian] object Operations:
  import Git.Error.Reason as Reason

  val emptyTree: Git.Hash = Git.Hash.unsafe(t"4b825dc642cb6eb9a060e54bf8d69288fbee4904")

  def path(text: Text)(using Tactic[Git.Error]): Path on Linux =
    safely(text.as[Path on Linux]).or(abort(Git.Error(Reason.NoRepository(text))))

  // Enrols the open in galilei's register when it is exclusive, keyed on the real path so that
  // two routes to one repository register as the same one.
  def registered[result](path: Path on Linux, mode: Mode, flags: List[Git.OpenFlag])(block: => result)
    ( using Tactic[Git.Error] )
  :   result =

    if !mode.atoms.has(Exclusive) then block else
      val real: Text =
        try java.nio.file.Path.of(path.encode.s).nn.toRealPath().nn.toString.tt
        catch case _: Exception => path.encode

      if flags.has(Git.OpenFlag.Await) then AccessRegister.acquireAwait(real, mode.atoms)
      else if !AccessRegister.acquire(real, mode.atoms)
      then abort(Git.Error(Reason.Unsupported(t"a concurrent exclusive open of $real")))

      try block finally AccessRegister.release(real, mode.atoms)

  // A worktree-relative path as a path on the handle's plane.
  def planePath(worktree: Git.Worktree.Handle^, text: Text): Path on worktree.Plane =
    def recur(path: Path on worktree.Plane, names: List[Text]): Path on worktree.Plane = names match
      case Nil          => path
      case name :: rest => recur(path.unsafeChild(name)(using Unsafe), rest)

    recur(worktree.base, text.cut(t"/"))

  // Pairs of ref name and value as a map keyed by parsed ref paths, dropping names which are not
  // fully-qualified refs (`HEAD`, say).
  def refMap[value](pairs: List[(Text, value)]): Map[Git.Ref, value] =
    def recur(pairs: List[(Text, value)], parsed: List[(Git.Ref, value)]): List[(Git.Ref, value)] =
      pairs match
        case Nil                   => parsed
        case (name, value) :: rest =>
          safely(Git.Refs.parse(name)) match
            case ref: Git.Ref => recur(rest, (ref, value) :: parsed)
            case _            => recur(rest, parsed)

    recur(pairs, Nil).to[Map]

  def relative[plane](path: Path on plane): Text = path.descent.to(List).reverse.join(t"/")

  def resolveRef(backend: Git.Backend^, gitDir: Path on Linux, name: Text)(using Tactic[Git.Error])
  :   Optional[Git.Hash] =

    def recur(name: Text, depth: Int): Optional[Git.Hash] =
      if depth > 10 then Unset else backend.readRef(gitDir, name) match
        case Git.Backend.Target.Direct(hash)  => hash
        case Git.Backend.Target.Symbolic(ref) => recur(ref, depth + 1)
        case _                                => Unset

    recur(name, 0)

  def readObject(backend: Git.Backend^, gitDir: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error])
  :   (Git.Object.Kind, Data) =

    backend.readObject(gitDir, hash).or(abort(Git.Error(Reason.NoSuchObject(hash.text))))

  // The value in force for a single-valued key: git lists scopes from system to local and the
  // last setting wins.
  def setting(config: List[(Git.Config.Key, Text)], key: Git.Config.Key): Optional[Text] =
    config.sweep { case (`key`, value) => value }.reverse.prim

  def identity(backend: Git.Backend^, gitDir: Path on Linux)(using Tactic[Git.Error]): Git.Identity =
    val config = backend.configRead(Git.Config.Source.Effective(gitDir))
    def value(key: Git.Config.Key): Text =
      setting(config, key).or(abort(Git.Error(Reason.NoSuchConfig(key.text))))

    val nameKey = Git.Config.Key(t"user", Unset, t"name")
    val emailKey = Git.Config.Key(t"user", Unset, t"email")
    Git.Identity(value(nameKey), value(emailKey))

  // A named class, so that the backend is a constructor field rather than a parameter hidden
  // behind a fresh anonymous instance, which separation checking rejects.
  class ConfigHandle[grants <: Grant](val source: Git.Config.Source, val backend: Git.Backend^)
  extends Git.Config.Handle, Granting[grants]

  // Lends a configuration handle for the block, as `open` would.
  def config[grants <: Grant, result](backend: Git.Backend^, source: Git.Config.Source)
    ( block: ((Git.Config.Handle & Granting[grants])^) ?=> result )
  :   result =

    block(using ConfigHandle[grants](source, backend))

  def expectHead(backend: Git.Backend^, gitDir: Path on Linux, head: Stale[Git.Head])(using Tactic[Git.Error]): Unit =
    val actual = backend.head(gitDir)
    if actual != head.value then abort(Git.Error(Reason.Moved(t"HEAD", head.value.commit, actual.commit)))

  def expectStash(backend: Git.Backend^, gitDir: Path on Linux, entry: Stale[Git.Stash.Entry])(using Tactic[Git.Error]): Unit =
    val present = backend.stashes(gitDir).reap { case current if current.index == entry.value.index => current }
    if present.let(_.commit) != Optional(entry.value.commit) then abort(Git.Error(Reason.NoSuchStash))

  def commit
    ( backend:    Git.Backend^,
      gitDir:     Path on Linux,
      root:       Path on Linux,
      message:    Text,
      head:       Git.Head,
      parents:    List[Git.Hash],
      author:     Optional[Git.Signature],
      signer:     Optional[Git.Signer],
      allowEmpty: Boolean,
      verb:       Text )
    ( using Tactic[Git.Error] )
  :   Git.Hash =

    val tree = backend.writeTree(gitDir, root)

    if !allowEmpty then
      val parentTree = parents.prim.lay(emptyTree): parent =>
        Objects.parseCommit(parent, readObject(backend, gitDir, parent)(1)).tree
      if parentTree == tree then abort(Git.Error(Reason.NothingToCommit))

    val committer = Git.Signature(identity(backend, gitDir), Git.Timestamp.now())
    val payload = Objects.serializeCommit(tree, parents, author.or(committer), committer, message)
    val signed = signer.lay(payload) { signer => Objects.signCommit(payload, signer.sign(payload)) }
    val hash = backend.writeObject(gitDir, Git.Object.Kind.Commit, signed)
    val subject = message.cut(t"\n").prim.or(t"")
    backend.updateRef(gitDir, t"HEAD", hash, Git.Backend.Expectation(head.commit), t"$verb: $subject")
    hash

  def clone
    ( source: Text, target: Path on Linux, branch: Optional[Git.Branch], depth: Optional[Int],
      bare: Boolean, recursive: Boolean, activity: Git.Activity^ )
    ( using backend: Git.Backend^, internet: Internet, tactic: Tactic[Git.Error] )
  :   Unit =

    backend.clone(source, target, branch, depth, bare, recursive, activity)

  def cloneCommit(source: Text, target: Path on Linux, commit: Git.Hash, activity: Git.Activity^)
    ( using backend: Git.Backend^, internet: Internet, tactic: Tactic[Git.Error] )
  :   Unit =

    backend.init(target, Unset, false)
    val location = backend.locate(target).or(abort(Git.Error(Reason.NoRepository(target.encode))))
    backend.fetch(location.gitDir, source, List(Git.Refspec(commit.text, Unset, false)), 1, false, false, activity)
    backend.checkout(location.gitDir, target, commit.text)

  // Notes: a write is a new notes commit made by the backend on no ref, then a compare-and-swap
  // of the notes ref from the value the caller saw.
  def readNote(backend: Git.Backend^, gitDir: Path on Linux, target: Git.Hash, ref: Git.Ref)(using Tactic[Git.Error])
  :   Optional[Text] =

    backend.readNote(gitDir, ref.encode, target)

  def listNotes(backend: Git.Backend^, gitDir: Path on Linux, ref: Git.Ref)(using Tactic[Git.Error])
  :   Map[Git.Hash, Git.Hash] =

    backend.listNotes(gitDir, ref.encode).map { (note, target) => target -> note }.to[Map]

  def writeNote
    ( backend: Git.Backend^, gitDir: Path on Linux, ref: Git.Ref, from: Optional[Git.Hash],
      edit: Git.Backend.NoteEdit )
    ( using Tactic[Git.Error] )
  :   Git.Hash =

    val commit = backend.editNotes(gitDir, from, edit)
    backend.updateRef(gitDir, ref.encode, commit, Git.Backend.Expectation(from), t"notes: ${edit.toString.tt}")
    commit

  // Remotes live in the repository's configuration.
  private val remoteSection: Text = t"remote"
  private val urlName: Text = t"url"
  private val pushurlName: Text = t"pushurl"
  private val fetchName: Text = t"fetch"
  private val branchSection: Text = t"branch"

  private def localConfig(gitDir: Path on Linux): Git.Config.Source =
    Git.Config.Source.Repository(gitDir, Git.Config.Scope.Local)

  def listRemotes(backend: Git.Backend^, gitDir: Path on Linux)(using Tactic[Git.Error]): List[Remote] =
    val config = backend.configRead(localConfig(gitDir))

    val names = config.sweep:
      case (Git.Config.Key(`remoteSection`, name, `urlName`), _) => name.or(t"")
    . distinct

    names.map: name =>
      val urlKey = Git.Config.Key(remoteSection, name, urlName)
      val pushKey = Git.Config.Key(remoteSection, name, pushurlName)
      val url = setting(config, urlKey).or(t"")
      val push = setting(config, pushKey)
      Remote(name, url, push)

  def upstream(backend: Git.Backend^, gitDir: Path on Linux, branch: Git.Branch)(using Tactic[Git.Error])
  :   Optional[(Text, Git.Branch)] =

    val config = backend.configRead(Git.Config.Source.Repository(gitDir, Git.Config.Scope.Local))
    val remoteKey = Git.Config.Key(t"branch", branch.text, t"remote")
    val mergeKey = Git.Config.Key(t"branch", branch.text, t"merge")
    val heads = t"refs/heads/"

    config.reap { case (`remoteKey`, remote) => remote }.let: remote =>
      config.reap { case (`mergeKey`, merge) => merge }.let: merge =>
        (remote, Git.Branch.unsafe(if merge.starts(heads) then merge.skip(11) else merge))

  def addRemote(backend: Git.Backend^, gitDir: Path on Linux, name: Text, url: Text, fetch: Optional[Git.Refspec])
    ( using Tactic[Git.Error] )
  :   Remote =

    if listRemotes(backend, gitDir).exists(_.name == name) then abort(Git.Error(Reason.RemoteExists(name)))
    val source = localConfig(gitDir)
    backend.configWrite(source, Git.Config.Key(remoteSection, name, urlName), url, false)
    val refspec = fetch.or(Git.Refspec(t"refs/heads/*", t"refs/remotes/$name/*", true))
    backend.configWrite(source, Git.Config.Key(remoteSection, name, fetchName), refspec.text, false)
    Remote(name, url)

  def removeRemote(backend: Git.Backend^, gitDir: Path on Linux, name: Text)(using Tactic[Git.Error]): Unit =
    if !listRemotes(backend, gitDir).exists(_.name == name) then abort(Git.Error(Reason.NoSuchRemote(name)))
    val source = localConfig(gitDir)
    val config = backend.configRead(source)
    val subsection: Optional[Text] = name

    val keys = config.sweep:
      case (key@Git.Config.Key(`remoteSection`, `subsection`, _), _) => key
    . distinct

    keys.each: key =>
      backend.configUnset(source, key, true)

    backend.listRefs(gitDir, t"refs/remotes/$name/").each: (ref, hash) =>
      backend.updateRef(gitDir, ref, Unset, Git.Backend.Expectation.Exactly(hash), Unset)

  private def reprefix(text: Text, oldPrefix: Text, newPrefix: Text): Text =
    if text.starts(oldPrefix) then t"$newPrefix${text.skip(oldPrefix.length)}" else text

  def renameRemote(backend: Git.Backend^, gitDir: Path on Linux, from: Text, to: Text)(using Tactic[Git.Error]): Unit =
    if !listRemotes(backend, gitDir).exists(_.name == from) then abort(Git.Error(Reason.NoSuchRemote(from)))
    if listRemotes(backend, gitDir).exists(_.name == to) then abort(Git.Error(Reason.RemoteExists(to)))
    val source = localConfig(gitDir)
    val config = backend.configRead(source)
    val fromSubsection: Optional[Text] = from
    val oldPrefix = t"refs/remotes/$from/"
    val newPrefix = t"refs/remotes/$to/"

    config.each:
      case (Git.Config.Key(`remoteSection`, `fromSubsection`, key), value) =>
        val rewritten = if key == fetchName then reprefix(value, oldPrefix, newPrefix) else value
        backend.configWrite(source, Git.Config.Key(remoteSection, to, key), rewritten, true)
      case (Git.Config.Key(`branchSection`, branch, `remoteSection`), `from`) =>
        backend.configWrite(source, Git.Config.Key(branchSection, branch, remoteSection), to, false)
      case _ => ()

    val keys = config.sweep:
      case (key@Git.Config.Key(`remoteSection`, `fromSubsection`, _), _) => key
    . distinct

    keys.each: key =>
      backend.configUnset(source, key, true)

    backend.listRefs(gitDir, oldPrefix).each: (ref, hash) =>
      val renamed = reprefix(ref, oldPrefix, newPrefix)
      backend.updateRef(gitDir, renamed, hash, Git.Backend.Expectation.Absent, Unset)
      backend.updateRef(gitDir, ref, Unset, Git.Backend.Expectation.Exactly(hash), Unset)

  def setRemoteUrl(backend: Git.Backend^, gitDir: Path on Linux, name: Text, url: Text, push: Boolean)
    ( using Tactic[Git.Error] )
  :   Unit =

    if !listRemotes(backend, gitDir).exists(_.name == name) then abort(Git.Error(Reason.NoSuchRemote(name)))
    val key = Git.Config.Key(remoteSection, name, if push then pushurlName else urlName)
    backend.configWrite(localConfig(gitDir), key, url, false)
