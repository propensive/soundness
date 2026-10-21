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

// Revisions and history: resolving `gitrevisions(7)` expressions, walking commits in date order,
// and finding merge bases.
private[octogenarian] object Revwalk:
  import Git.Error.Reason as Reason

  def commit(objects: Path on Linux, hash: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Commit =

    Store.readOr(objects, hash) match
      case (Git.Object.Kind.Commit, data) =>
        val parsed = Objects.parseCommit(hash, data)
        // A shallow clone's boundary commits have parents the repository does not hold.
        if parsed.parents != Nil && shallow(objects).contains(hash) then parsed.copy(parents = Nil)
        else parsed

      case _ => abort(Git.Error(Reason.NoSuchObject(hash.text)))

  // The commits listed in the repository's `shallow` file, whose parents are absent by design.
  private def shallow(objects: Path on Linux)(using Tactic[Io.Error]): java.util.HashSet[Git.Hash] =
    val set = java.util.HashSet[Git.Hash]()
    val file: Optional[Path on Linux] = objects.parent.let(Refstore.descend(_, List(t"shallow")))

    def load(file: Path on Linux): Unit = if file.existent() then
      Objects.text(file.read[Data]).cut(t"\n").filter(_.length == 40).each: line =>
        set.add(Git.Hash.unsafe(line))

    file.let(load(_))

    set

  // Follows tag objects to the object they ultimately name.
  def peel(objects: Path on Linux, hash: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    Store.read(objects, hash) match
      case found: (Git.Object.Kind, Data) @unchecked =>
        found match
          case (Git.Object.Kind.Tag, data) => peel(objects, Objects.parseTag(data).target)
          case _                           => hash

      case _ => hash

  private val hexDigits: Set[Char] =
    Set('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f')

  private def isHex(text: Text): Boolean = text.s.forall(hexDigits.has(_))

  // The order in which git tries to read a short name as a ref.
  private def refCandidates(name: Text): List[Text] =
    List
      ( name,
        t"refs/$name",
        t"refs/tags/$name",
        t"refs/heads/$name",
        t"refs/remotes/$name",
        t"refs/remotes/$name/HEAD" )

  def resolve
    ( gitDir: Path on Linux, commonDir: Path on Linux, objects: Path on Linux, revision: Text )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Git.Hash] =

    def base(name: Text): Optional[Git.Hash] =
      if name == t"@" || name == t"HEAD" then Refstore.resolve(gitDir, commonDir, t"HEAD")
      else if name.length == 40 && isHex(name) then
        val hash = Git.Hash.unsafe(name)
        if Store.exists(objects, hash) then hash else Unset
      else
        def refs(candidates: List[Text]): Optional[Git.Hash] = candidates match
          case candidate :: rest => Refstore.resolve(gitDir, commonDir, candidate).or(refs(rest))
          case _                 => Unset

        refs(refCandidates(name)).or:
          if name.length >= 4 && isHex(name) then
            Store.all(objects).filter(_.text.starts(name)) match
              case List(only) => only
              case _          => Unset
          else
            Unset

    def parent(hash: Git.Hash, n: Int): Optional[Git.Hash] =
      val parents = commit(objects, peel(objects, hash)).parents
      if n == 0 then peel(objects, hash) else Refstore.nth(parents, n - 1)

    def ancestor(hash: Git.Hash, n: Int): Optional[Git.Hash] =
      if n == 0 then peel(objects, hash) else parent(hash, 1).let(ancestor(_, n - 1))

    def reflogEntry(name: Text, n: Int): Optional[Git.Hash] =
      val target = if name == t"@" then t"HEAD" else name
      val resolved = Refstore.dereference(gitDir, commonDir, target)
      Refstore.nth(Refstore.reflog(gitDir, commonDir, resolved), n).let(_.hash)

    // Suffix operators bind from the right: `main~2^{tree}` is `(main~2)^{tree}`.
    def recur(text: Text): Optional[Git.Hash] =
      val string = text.s

      if string.endsWith("^{}") then recur(text.skip(3, Rtl)).let(peel(objects, _))
      else if string.endsWith("^{commit}") then recur(text.skip(9, Rtl)).let(peel(objects, _))
      else if string.endsWith("^{tree}") then
        recur(text.skip(7, Rtl)).let: h => commit(objects, peel(objects, h)).tree
      else if string.endsWith("^{tag}") then
        recur(text.skip(6, Rtl))
      else if string.endsWith("}") && string.contains("@{") then
        val at = string.lastIndexOf("@{")
        val inner = string.substring(at + 2, string.length - 1).nn.tt
        val head = text.keep(at)
        safely(inner.as[Int]).let(reflogEntry(if head.length == 0 then t"HEAD" else head, _))
      else
        val tilde = string.lastIndexOf('~')
        val caret = string.lastIndexOf('^')
        val position = tilde.max(caret)

        if position <= 0 then base(text)
        else
          val count = text.skip(position + 1)
          val n = if count.length == 0 then 1 else safely(count.as[Int]).or(1)
          val head = text.keep(position)

          if position == tilde then recur(head).let(ancestor(_, n))
          else recur(head).let(parent(_, n))

    recur(revision.trim)

  // Commits reachable from `from` and not from `until`, newest committer date first.
  def walk
    ( objects: Path on Linux,
      from: Git.Hash,
      until: Optional[Git.Hash],
      limit: Optional[Int],
      paths: List[Text] )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   List[Git.Hash] =

    val excluded = until.lay(java.util.HashSet[Git.Hash]())(ancestors(objects, _))

    // Newest committer date first.
    val comparator: java.util.Comparator[(Long, Git.Hash)] =
      (left, right) => java.lang.Long.compare(right.nn(0), left.nn(0))

    val queue = java.util.PriorityQueue[(Long, Git.Hash)](comparator)
    val seen = java.util.HashSet[Git.Hash]()

    def enqueue(hash: Git.Hash): Unit =
      if !seen.contains(hash) && !excluded.contains(hash) then
        seen.add(hash)
        queue.add((commit(objects, hash).committer.timestamp.seconds, hash))

    enqueue(peel(objects, from))

    def touches(hash: Git.Hash): Boolean =
      if paths == Nil then true else
        val current = commit(objects, hash)
        val parentTree = current.parents.prim.let(commit(objects, _).tree)

        paths.exists: path =>
          Trees.lookup(objects, current.tree, path) != parentTree
          . let(Trees.lookup(objects, _, path))

    def recur(acc: List[Git.Hash], count: Int): List[Git.Hash] =
      if queue.isEmpty || limit.lay(false)(count >= _) then acc.reverse else
        val (_, hash) = queue.poll().nn
        commit(objects, hash).parents.each(enqueue(_))
        if touches(hash) then recur(hash :: acc, count + 1) else recur(acc, count)

    recur(Nil, 0)

  // Every commit reachable from `from`, itself included, as a set to test membership of.
  def ancestors(objects: Path on Linux, from: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   java.util.HashSet[Git.Hash] =

    val seen = java.util.HashSet[Git.Hash]()

    def recur(pending: List[Git.Hash]): Unit = pending match
      case hash :: rest =>
        if seen.contains(hash) then recur(rest)
        else
          seen.add(hash)
          recur(commit(objects, hash).parents + rest)

      case _ => ()

    recur(List(peel(objects, from)))
    seen

  def isAncestor(objects: Path on Linux, ancestor: Git.Hash, descendant: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Boolean =

    ancestors(objects, descendant).contains(peel(objects, ancestor))

  // The best common ancestor: a common ancestor which is not an ancestor of another.
  def mergeBase(objects: Path on Linux, left: Git.Hash, right: Git.Hash)
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Git.Hash] =

    val leftAncestors = ancestors(objects, left)
    val seen = java.util.HashSet[Git.Hash]()

    def candidates(pending: List[Git.Hash], found: List[Git.Hash]): List[Git.Hash] = pending match
      case hash :: rest =>
        if seen.contains(hash) then candidates(rest, found)
        else
          seen.add(hash)

          if leftAncestors.contains(hash) then candidates(rest, hash :: found)
          else candidates(commit(objects, hash).parents + rest, found)

      case _ => found

    val found = candidates(List(peel(objects, right)), Nil)

    def dominated(candidate: Git.Hash): Boolean =
      found.exists: other => other != candidate && ancestors(objects, other).contains(candidate)

    def age(hash: Git.Hash): Long = -commit(objects, hash).committer.timestamp.seconds

    found.filter(!dominated(_)).order(age(_)).prim

// Trees: looking up entries by path and writing trees.
private[octogenarian] object Trees:
  import Git.Error.Reason as Reason

  def entries(objects: Path on Linux, tree: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   List[Git.Object.Tree.Entry] =

    Store.readOr(objects, tree) match
      case (Git.Object.Kind.Tree, data) => Objects.parseTree(data)
      case _                            => abort(Git.Error(Reason.NoSuchObject(tree.text)))

  // The hash at a slash-separated path within a tree, if any.
  def lookup
    ( objects: Path on Linux, tree: Git.Hash, path: Text )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Optional[Git.Hash] =

    def recur(tree: Git.Hash, segments: List[Text]): Optional[Git.Hash] = segments match
      case segment :: rest =>
        entries(objects, tree).reap { case entry if entry.name == segment => entry }.let: entry =>
          if rest == Nil then entry.hash else recur(entry.hash, rest)

      case _ => tree

    recur(tree, path.cut(t"/").filter(_.length > 0))

  // Every blob under a tree, as path → hash.
  def flatten(objects: Path on Linux, tree: Git.Hash)(using Tactic[Git.Error], Tactic[Io.Error])
  :   List[(Text, Git.Hash)] =

    def recur(tree: Git.Hash, prefix: Text): List[(Text, Git.Hash)] =
      def visit(entry: Git.Object.Tree.Entry): List[(Text, Git.Hash)] = entry.mode match
        case Git.Object.Tree.Mode.Directory => recur(entry.hash, t"$prefix${entry.name}/")
        case _                              => List((t"$prefix${entry.name}", entry.hash))

      entries(objects, tree).bind(visit(_))

    recur(tree, t"")

  def write
    ( objects: Path on Linux, entries: List[Git.Object.Tree.Entry] )
    ( using Tactic[Git.Error], Tactic[Io.Error] )
  :   Git.Hash =

    Store.write(objects, Git.Object.Kind.Tree, Objects.serializeTree(entries))
