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
import filesystemOptions.overwritePreexisting

// Fetching, pushing and cloning against a `Peer`: which refs a refspec selects, which objects
// must travel, and how the local refs (or the peer's) are moved afterwards.
private[octogenarian] object Transfer:
  import Git.Error.Reason as Reason

  case class Context
    ( gitDir:    Path on Linux,
      commonDir: Path on Linux,
      objects:   Path on Linux,
      identity:  Git.Identity,
      logging:   Boolean )

  def count[element](list: List[element]): Int = list.fold(0): (n, _) => n + 1

  // ───────────────────────────────────────── Object graphs ────────────────────────────────────

  // The objects an object refers to: a commit its tree and parents, a tree its entries, a tag
  // its target, a blob nothing.
  private def references
    ( kind: Git.Object.Kind, data: Data )
    ( using Tactic[Git.Error] )
  :   List[Git.Hash] =

    kind match
      case Git.Object.Kind.Blob => Nil
      case Git.Object.Kind.Tree => Objects.parseTree(data).map(_.hash)
      case Git.Object.Kind.Tag  => Objects.parseTag(data).target :: Nil

      case Git.Object.Kind.Commit =>
        val commit = Objects.parseCommit(Git.Hash.zero, data)
        commit.tree :: commit.parents

  // Copies every object reachable from `wants` which `has` does not already know, from `read`
  // to `store`, commits newest first. With a `depth`, a commit's parents are not followed beyond
  // it, and the commits whose parents were cut off are returned.
  def copy
    ( read: Git.Hash => Optional[(Git.Object.Kind, Data)],
      wants: List[Git.Hash],
      has: Git.Hash => Boolean,
      depth: Optional[Int],
      store: (Git.Object.Kind, Data) => Git.Hash,
      activity: Git.Activity^ )
    ( using Tactic[Git.Error] )
  :   List[Git.Hash] =

    val seen = java.util.HashSet[Git.Hash]()
    val shallow = java.util.ArrayList[Git.Hash]()

    // Each pending object with the number of commits followed to reach it.
    def recur(pending: List[(Git.Hash, Int)]): Unit = pending match
      case (hash, level) :: rest =>
        if seen.contains(hash) || has(hash) then recur(rest)
        else
          seen.add(hash)
          val (kind, data) = read(hash).or(abort(Git.Error(Reason.NoSuchObject(hash.text))))
          store(kind, data)
          activity.advance()

          val next: List[(Git.Hash, Int)] = kind match
            case Git.Object.Kind.Commit =>
              val commit = Objects.parseCommit(hash, data)
              val cut = depth.lay(false)(level + 1 >= _)
              if cut && commit.parents != Nil then shallow.add(hash)
              val parents = if cut then Nil else commit.parents.map((_, level + 1))
              (commit.tree, level) :: parents

            case _ => references(kind, data).map((_, level))

          recur(next + rest)

      case _ => ()

    recur(wants.map((_, 0)))

    def collect(index: Int, acc: List[Git.Hash]): List[Git.Hash] =
      if index < 0 then acc else collect(index - 1, shallow.get(index).nn :: acc)

    collect(shallow.size - 1, Nil)

  // The objects to send so that a peer which has everything reachable from `haves` can reach
  // `wants`: commits newest first, each followed by what its tree adds.
  def enumerate(objects: Path on Linux, wants: List[Git.Hash], haves: List[Git.Hash])
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Git.Hash] =

    val known = java.util.HashSet[Git.Hash]()
    val excludedCommits = java.util.HashSet[Git.Hash]()

    // Everything reachable from `haves` is known, commits and their trees alike: trees are
    // walked lazily, as they are met.
    def excludeCommits(pending: List[Git.Hash]): Unit = pending match
      case hash :: rest =>
        if excludedCommits.contains(hash) then excludeCommits(rest)
        else
          excludedCommits.add(hash)

          Store.readOr(objects, hash) match
            case (Git.Object.Kind.Commit, data) =>
              val commit = Objects.parseCommit(hash, data)
              excludeCommits(commit.parents + rest)

            case (Git.Object.Kind.Tag, data) =>
              excludeCommits(Objects.parseTag(data).target :: rest)

            case _                           => excludeCommits(rest)

      case _ => ()

    excludeCommits(haves)

    def markTree(hash: Git.Hash): Unit = if !known.contains(hash) then
      known.add(hash)

      Store.readOr(objects, hash) match
        case (Git.Object.Kind.Tree, data) => Objects.parseTree(data).each: entry =>
          markTree(entry.hash)

        case _                            => ()

    // The trees of excluded commits adjacent to included ones are known to the peer.
    def markCommitTree(hash: Git.Hash): Unit = Store.readOr(objects, hash) match
      case (Git.Object.Kind.Commit, data) => markTree(Objects.parseCommit(hash, data).tree)
      case _                              => ()

    val sent = java.util.HashSet[Git.Hash]()

    def recur(pending: List[Git.Hash], acc: List[Git.Hash]): List[Git.Hash] = pending match
      case hash :: rest =>
        if sent.contains(hash) || known.contains(hash) || excludedCommits.contains(hash)
        then recur(rest, acc)
        else
          sent.add(hash)
          val (kind, data) = Store.readOr(objects, hash)

          val next = kind match
            case Git.Object.Kind.Commit =>
              val commit = Objects.parseCommit(hash, data)
              commit.parents.filter(excludedCommits.contains(_)).each(markCommitTree(_))
              commit.tree :: commit.parents

            case _ => references(kind, data)

          recur(next + rest, hash :: acc)

      case _ => acc.reverse

    recur(wants, Nil)

  // ─────────────────────────────────────────── Refspecs ───────────────────────────────────────

  // Whether `pattern` (with at most one `*`) matches `name`, and what the `*` stood for.
  private def matchGlob(pattern: Text, name: Text): Optional[Text] =
    val star = pattern.s.indexOf('*')

    if star < 0 then if pattern == name then t"" else Unset
    else
      val prefix = pattern.keep(star)
      val suffix = pattern.skip(star + 1)

      if name.starts(prefix) && name.ends(suffix) && name.length >= prefix.length + suffix.length
      then name.skip(prefix.length).skip(suffix.length, Rtl)
      else Unset

  private def substitute(pattern: Text, middle: Text): Text =
    val star = pattern.s.indexOf('*')
    if star < 0 then pattern else t"${pattern.keep(star)}$middle${pattern.skip(star + 1)}"

  private def isHex(text: Text): Boolean =
    text.length == 40 && text.s.chars.nn.allMatch: char =>
      (char >= '0' && char <= '9') || (char >= 'a' && char <= 'f')

  // The full name of a ref given by a short name, as git's DWIM rules find it among `refs`.
  private def expand(short: Text, refs: List[(Text, Git.Hash)]): Optional[Text] =
    val candidates =
      List(short, t"refs/$short", t"refs/tags/$short", t"refs/heads/$short", t"refs/remotes/$short")

    def named(candidate: Text): Boolean = refs.exists(_(0) == candidate)

    candidates.filter(named(_)).head

  // One ref a fetch refspec selected on the peer: its name there, its hash, where it lands
  // locally (if anywhere), and whether a non-fast-forward is allowed.
  case class Selection(remote: Text, hash: Git.Hash, local: Optional[Text], force: Boolean)

  private def select(refspec: Git.Refspec, refs: List[(Text, Git.Hash)]): List[Selection] =
    def globbed(source: Text, entry: (Text, Git.Hash)): List[Selection] =
      val (name, hash) = entry

      def selection(middle: Text): Selection =
        Selection(name, hash, refspec.destination.let(substitute(_, middle)), refspec.force)

      matchGlob(source, name).let(selection(_)).let(List(_)).or(Nil)

    def named(name: Text): List[Selection] =
      def selection
        ( entry: (Text, Git.Hash) )
      :   Selection =

        Selection(name, entry(1), refspec.destination, refspec.force)

      refs.filter(_(0) == name).map(selection(_))

    refspec.source match
      case source: Text =>
        if source.s.indexOf('*') >= 0 then refs.bind(globbed(source, _))
        else if isHex(source)
        then List(Selection(source, Git.Hash.unsafe(source), refspec.destination, refspec.force))
        else expand(source, refs).let(named(_)).or(Nil)

      case _ => Nil

  // ───────────────────────────────────────────── Remotes ──────────────────────────────────────

  private def config(context: Context)(using Tactic[Io.Error]): List[(Git.Config.Key, Text)] =
    Gitconfig.effective(context.gitDir, context.commonDir)

  private def remoteSetting(context: Context, remote: Text, name: Text)
    ( using Tactic[Io.Error] )
  :   Optional[Text] =

    Operations.setting(config(context), Git.Config.Key(t"remote", remote, name))

  private def remoteSettings(context: Context, remote: Text, name: Text)
    ( using Tactic[Io.Error] )
  :   List[Text] =

    val key = Git.Config.Key(t"remote", remote, name)
    config(context).sweep { case (`key`, value) => value }

  // The URL a remote name stands for, or the text itself if it is not a configured remote.
  private def url(context: Context, remote: Text, push: Boolean)(using Tactic[Io.Error]): Text =
    val pushUrl = if push then remoteSetting(context, remote, t"pushurl") else Unset
    pushUrl.or(remoteSetting(context, remote, t"url")).or(remote)

  private def isLocalUrl(url: Text): Boolean =
    url.starts(t"/") || url.starts(t"file://") || url.starts(t"./") || url.starts(t"../")

  private def localPath(url: Text): Text =
    if url.starts(t"file://") then url.skip(7) else url

  def connect
    ( url: Text,
      transports: List[Transport],
      locate: Path on Linux => Optional[Git.Backend.Location],
      identity: Git.Identity )
    ( using Tactic[Git.Error] )
  :   Peer^ =

    if isLocalUrl(url) then
      val path = Operations.path(localPath(url))
      val location = locate(path).or(abort(Git.Error(Reason.NoRepository(path.encode))))
      Peer.Local(location, identity)
    else
      val transport =
        transports.filter(_.accepts(url)).head
        . or(abort(Git.Error(Reason.Unsupported(t"the transport for $url"))))

      transport.connect(url)

  // ──────────────────────────────────────────── Fetching ──────────────────────────────────────

  private def describe(name: Text): Text =
    if name.starts(t"refs/heads/") then t"branch '${name.skip(11)}'"
    else if name.starts(t"refs/tags/") then t"tag '${name.skip(10)}'"
    else if isHex(name) then name
    else t"'$name'"

  def remoteRefs
    ( context: Context, remote: Text, peer: Peer^ )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[(Text, Git.Hash)] =

    val refs = peer.refs()
    refs

  // Fetches from `peer`, moving the local refs the refspecs name and writing `FETCH_HEAD`;
  // returns the refs it changed.
  def fetch
    ( context: Context,
      remote: Text,
      peer: Peer^,
      url: Text,
      refspecs: List[Git.Refspec],
      depth: Optional[Int],
      prune: Boolean,
      tags: Boolean,
      activity: Git.Activity^ )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[(Text, Git.Hash)] =

    val refs = peer.refs().filter(!_(0).ends(t"^{}"))

    def parseRefspec(text: Text): Git.Refspec = unsafely(Git.Refspec.parse(text))

    val configured =
      if refspecs != Nil then refspecs
      else remoteSettings(context, remote, t"fetch").map(parseRefspec(_))

    val specs =
      configured + (if tags then List(Git.Refspec(t"refs/tags/*", t"refs/tags/*", false)) else Nil)

    val selections = specs.bind(select(_, refs))
    def have(hash: Git.Hash): Boolean = Store.exists(context.objects, hash)
    val wants = selections.map(_.hash).distinct.filter(!have(_))

    activity.begin(Git.Activity.Phase.Enumerating)

    val haves = Refstore.list(context.commonDir, Unset).map(_(1)).distinct.filter(have(_))

    val boundary =
      peer.download
        ( wants,
          haves,
          have(_),
          depth,
          (kind, data) => Store.write(context.objects, kind, data),
          activity )

    if boundary != Nil then
      val file = context.gitDir / "shallow"

      val existing =
        if file.existent() then Objects.text(file.read[Data]).cut(t"\n").filter(_.length == 40)
        else Nil

      val all = (existing + boundary.map(_.text)).distinct

      if file.existent() then file.delete()
      file.write(Objects.bytes(t"${all.join(t"\n")}\n"))

    // Moves a local ref to what the peer has, if that is a fast-forward or forcing is allowed.
    def update
      ( selection: Selection )
    :   Optional[(Text, Git.Hash)] =

      selection.local.let(move(selection, _))

    def move(selection: Selection, local: Text): Optional[(Text, Git.Hash)] =
      val current = Refstore.resolve(context.gitDir, context.commonDir, local)

      if current == Optional(selection.hash) then Unset
      else
        val fastForward =
          current.lay(true): current => Revwalk.isAncestor(context.objects, current, selection.hash)

        if !fastForward && !selection.force then
          abort(Git.Error(Reason.NotFastForward, t"$local: ${describe(selection.remote)} of $url"))

        Refstore.update
          ( context.gitDir,
            context.commonDir,
            local,
            selection.hash,
            Git.Backend.Expectation(current),
            t"fetch $url: ${if fastForward then t"fast-forward" else t"forced-update"}",
            context.identity,
            context.logging )

        (local, selection.hash)

    def updatedBy
      ( selection: Selection )
    :   List[(Text, Git.Hash)] =

      update(selection).let(List(_)).or(Nil)

    val updated = selections.bind(updatedBy(_))

    // Locals matching a pattern destination whose remote counterpart has gone.
    if prune then
      def stale(spec: Git.Refspec): List[Text] = spec.destination match
        case destination: Text if destination.s.indexOf('*') >= 0 =>
          val remoteNames = selections.map(_.local)

          Refstore.list(context.commonDir, Unset).map(_(0)).filter: name =>
            matchGlob(destination, name).present && !remoteNames.has(name)

        case _ => Nil

      def delete(name: Text): Unit =
        val current = Refstore.resolve(context.gitDir, context.commonDir, name)

        Refstore.update
          ( context.gitDir,
            context.commonDir,
            name,
            Unset,
            Git.Backend.Expectation(current),
            t"fetch $url: prune",
            context.identity,
            context.logging )

      specs.bind(stale(_)).distinct.each(delete(_))

    // `FETCH_HEAD`: every selection, the first one for merging.
    def fetchHeadLine(selection: Selection, first: Boolean): Text =
      val marker = if first then t"" else t"not-for-merge"
      t"${selection.hash.text}\t$marker\t${describe(selection.remote)} of $url"

    val lines = selections match
      case first :: rest => fetchHeadLine(first, true) :: rest.map(fetchHeadLine(_, false))
      case _             => Nil

    val fetchHead = context.gitDir / "FETCH_HEAD"
    if fetchHead.existent() then fetchHead.delete()
    if lines != Nil then fetchHead.write(Objects.bytes(t"${lines.join(t"\n")}\n"))

    activity.finish()
    updated

  // ──────────────────────────────────────────── Pushing ───────────────────────────────────────

  // Where a push refspec's source lands on the peer.
  private def destinationOf(source: Text, local: Text, remoteRefs: List[(Text, Git.Hash)]): Text =
    if local.starts(t"refs/") then local
    else if local.starts(t"refs/tags/") then local
    else
      val fallback = if source.starts(t"refs/tags/") then source else t"refs/heads/$local"
      expand(local, remoteRefs).or(fallback)

  def push
    ( context: Context,
      remote: Text,
      peer: Peer^,
      url: Text,
      refspecs: List[Git.Refspec],
      force: Git.Push.Force,
      tags: Boolean,
      setUpstream: Boolean,
      activity: Git.Activity^ )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[(Text, Git.Push.Outcome)] =

    def listed(entry: (Text, Git.Hash)): Boolean = entry(0) != t"HEAD" && !entry(0).ends(t"^{}")
    val remoteRefs = peer.refs().filter(listed(_))

    // A move the peer can make without discarding history: its current commit is one we have
    // and the new one descends from it.
    def fastForward(from: Git.Hash, to: Git.Hash): Boolean =
      Store.exists(context.objects, from) && Revwalk.isAncestor(context.objects, from, to)

    val localRefs = Refstore.list(context.commonDir, Unset)

    val headRef: Optional[Text] = Refstore.read(context.gitDir, context.commonDir, t"HEAD") match
      case Git.Backend.Target.Symbolic(ref) => ref
      case _                                => Unset

    // With no refspec, the current branch goes to its namesake.
    val specs0 =
      if refspecs != Nil then refspecs
      else headRef.lay(abort(Git.Error(Reason.Detached))): ref => List(Git.Refspec(ref, ref, false))

    val specs =
      specs0 + (if tags then List(Git.Refspec(t"refs/tags/*", t"refs/tags/*", false)) else Nil)

    // Resolves one refspec to the remote ref it names, what the peer should hold and whether
    // forcing was asked for.
    case class Intent(remote: Text, to: Optional[Git.Hash], force: Boolean)

    def globbed(spec: Git.Refspec, source: Text, entry: (Text, Git.Hash)): List[Intent] =
      val (name, hash) = entry

      def intent(middle: Text): Intent =
        Intent(spec.destination.let(substitute(_, middle)).or(name), hash, spec.force)

      matchGlob(source, name).let(intent(_)).let(List(_)).or(Nil)

    def deletion(destination: Text): List[Intent] = List(Intent(destination, Unset, true))

    def intents(spec: Git.Refspec): List[Intent] = spec.source match
      case source: Text =>
        if source.s.indexOf('*') >= 0 then localRefs.bind(globbed(spec, source, _))
        else
          val resolved =
            Revwalk.resolve(context.gitDir, context.commonDir, context.objects, source)
            . or(abort(Git.Error(Reason.NoSuchRef(source))))

          val localName = expand(source, localRefs).or(source)

          val remoteName =
            spec.destination.lay(destinationOf(source, localName, remoteRefs)): destination =>
              destinationOf(source, destination, remoteRefs)

          List(Intent(remoteName, resolved, spec.force))

      case _ => spec.destination.let(deletion(_)).or(Nil)

    val all = specs.bind(intents(_))
    val remoteMap = remoteRefs.to[Map]

    def leaseEntry(entry: (Git.Ref, Stale[Optional[Git.Hash]])): (Text, Optional[Git.Hash]) =
      (entry(0).encode, entry(1).value)

    val lease: List[(Text, Optional[Git.Hash])] = force match
      case Git.Push.Force.Lease(expectations) => expectations.to[List].map(leaseEntry(_))
      case _                                  => Nil

    // Decides each intent before anything is sent: rejected ones never reach the peer.
    def decide(intent: Intent): (Intent, Either[Text, Optional[Git.Push.Outcome]]) =
      val current: Optional[Git.Hash] = remoteMap.at(intent.remote)
      val forced = intent.force || force == Git.Push.Force.Always

      lease.filter(_(0) == intent.remote).head match
        case (_, expected) if expected != current => (intent, Left(t"stale info"))

        case _ =>
          intent.to match
            case to: Git.Hash =>
              current match
                case from: Git.Hash =>
                  if from == to then (intent, Right(Git.Push.Outcome.UpToDate))
                  else if !fastForward(from, to) then
                    if forced then (intent, Right(Unset)) else (intent, Left(t"non-fast-forward"))
                  else
                    (intent, Right(Unset))

                case _ => (intent, Right(Unset))

            case _ =>
              current match
                case from: Git.Hash => (intent, Right(Unset))
                case _              => (intent, Left(t"remote ref does not exist"))

    val decided = all.map(decide(_))

    val pending = decided.bind:
      case (intent, Right(outcome)) if outcome.absent => List(intent)
      case _                                          => Nil

    def target(intent: Intent): List[Git.Hash] = intent.to.let(List(_)).or(Nil)
    val wants = pending.bind(target(_))
    activity.begin(Git.Activity.Phase.Enumerating)

    val objects =
      enumerate(context.objects, wants, remoteRefs.map(_(1))
      . filter(Store.exists(context.objects, _)))

    def update(intent: Intent): RefUpdate =
      RefUpdate(intent.remote, Git.Backend.Expectation(remoteMap.at(intent.remote)), intent.to)

    val results =
      if pending == Nil then Nil
      else
        def read(hash: Git.Hash): (Git.Object.Kind, Data) = Store.readOr(context.objects, hash)
        peer.upload(objects, read(_), pending.map(update(_)), activity)

    val resultMap = results.to[Map]

    def outcome(entry: (Intent, Either[Text, Optional[Git.Push.Outcome]]))
    :   (Text, Git.Push.Outcome) =

      val (intent, decision) = entry
      val current = remoteMap.at(intent.remote)
      val forced = intent.force || force == Git.Push.Force.Always

      val result: Git.Push.Outcome = decision match
        case Left(reason)   => Git.Push.Outcome.Rejected(reason)

        case Right(outcome) => outcome.or:
          resultMap.at(intent.remote).or(Unset) match
            case reason: Text => Git.Push.Outcome.Rejected(reason)

            case _ =>
              intent.to match
                case to: Git.Hash => current match
                  case from: Git.Hash =>
                    Git.Push.Outcome.Updated(from, to, forced && !fastForward(from, to))

                  case _              => Git.Push.Outcome.Created(to)

                case _ => Git.Push.Outcome.Deleted(current.or(Git.Hash.zero))

      (intent.remote, result)

    val outcomes = decided.map(outcome(_))

    def configureUpstream(ref: Text): Unit = if ref.starts(t"refs/heads/") then
      val branch = ref.skip(11)
      val file = context.gitDir / "config"
      val lines = Gitconfig.readFile(file)

      val withRemote =
        Gitconfig.set(lines, Git.Config.Key(t"branch", branch, t"remote"), remote, true)

      val withMerge =
        Gitconfig.set(withRemote, Git.Config.Key(t"branch", branch, t"merge"), ref, true)

      Gitconfig.writeFile(file, withMerge)

    if setUpstream then headRef.let(configureUpstream(_))

    activity.finish()
    outcomes

  def resolveUrl(context: Context, remote: Text, push: Boolean)(using Tactic[Io.Error]): Text =
    url(context, remote, push)
