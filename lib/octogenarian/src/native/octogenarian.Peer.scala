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

import filesystemBackends.javaBaseFilesystem

// A ref to change on a peer: what it is expected to hold now, and what it should hold.
case class RefUpdate(ref: Text, expected: Git.Backend.Expectation, to: Optional[Git.Hash])

// The other end of a fetch or push. A peer lists its refs, sends the objects reachable from the
// ones we want (stopping at what we have), and receives objects together with ref updates, which
// it applies one by one and reports on. How that happens — direct file access for a repository
// on this machine, the pack protocol for one elsewhere — is the peer's concern.
trait Peer:
  // Every ref with its hash, `HEAD` included, as `ls-remote` would list them.
  def refs()(using Tactic[Git.Error]): List[(Text, Git.Hash)]

  // What `HEAD` is a symbolic ref to, if the peer says.
  def symbolicHead()(using Tactic[Git.Error]): Optional[Text]

  // Sends every object reachable from `wants` which `has` does not know to `store`; `haves` are
  // the commits we hold, for a peer which can only be told what we have rather than asked. With
  // a `depth`, commit history is cut that many commits deep and the commits at the boundary are
  // returned, for the `shallow` file.
  def download
    ( wants: List[Git.Hash], haves: List[Git.Hash], has: Git.Hash => Boolean, depth: Optional[Int],
      store: (Git.Object.Kind, Data) => Git.Hash, activity: Git.Activity^ )
    ( using Tactic[Git.Error] )
  :   List[Git.Hash]

  // Receives the objects named, read through `read`, then applies the updates; the result pairs
  // each update's ref with the reason it was rejected, if it was.
  def upload
    ( objects: List[Git.Hash], read: Git.Hash => (Git.Object.Kind, Data), updates: List[RefUpdate],
      activity: Git.Activity^ )
    ( using Tactic[Git.Error] )
  :   List[(Text, Optional[Text])]

// A way of reaching peers at some kind of URL: the `http` and `ssh` components supply these.
trait Transport:
  def accepts(url: Text): Boolean
  def connect(url: Text)(using Tactic[Git.Error]): Peer^

object Peer:
  import Git.Error.Reason as Reason

  // A repository on this filesystem, reached by reading and writing its files directly.
  class Local(location: Git.Backend.Location, identity: Git.Identity) extends Peer:
    private val objects: Path on Linux = location.commonDir / "objects"

    private def io[result](block: Tactic[Io.Error] ?=> result)(using Tactic[Git.Error]): result =
      try throwErrors[Io.Error](block)
      catch case error: Io.Error => abort(Git.Error(Reason.Unexpected, error.toString.tt))

    private def logging()(using Tactic[Git.Error], Tactic[Io.Error]): Boolean =
      val config = Gitconfig.effective(location.gitDir, location.commonDir)
      val bare = Operations.setting(config, Git.Config.Key(t"core", Unset, t"bare")).lay(false)(_.lower == t"true")
      Operations.setting(config, Git.Config.Key(t"core", Unset, t"logallrefupdates")).lay(!bare)(_.lower != t"false")

    // As `ls-remote` lists them: HEAD first, then every ref, an annotated tag followed by its
    // peeled target as `<name>^{}`.
    def refs()(using Tactic[Git.Error]): List[(Text, Git.Hash)] = io:
      def headEntry(hash: Git.Hash): List[(Text, Git.Hash)] = List((t"HEAD", hash))
      val head = Refstore.resolve(location.gitDir, location.commonDir, t"HEAD").let(headEntry(_)).or(Nil)

      def withPeeled(entry: (Text, Git.Hash)): List[(Text, Git.Hash)] =
        val (name, hash) = entry
        if !name.starts(t"refs/tags/") then List(entry) else
          val peeled = Revwalk.peel(objects, hash)
          if peeled == hash then List(entry) else List(entry, (t"$name^{}", peeled))

      head + Refstore.list(location.commonDir, Unset).bind(withPeeled(_))

    def symbolicHead()(using Tactic[Git.Error]): Optional[Text] = io:
      Refstore.read(location.gitDir, location.commonDir, t"HEAD") match
        case Git.Backend.Target.Symbolic(ref) => ref
        case _                                => Unset

    def download
      ( wants: List[Git.Hash], haves: List[Git.Hash], has: Git.Hash => Boolean, depth: Optional[Int],
        store: (Git.Object.Kind, Data) => Git.Hash, activity: Git.Activity^ )
      ( using Tactic[Git.Error] )
    :   List[Git.Hash] = io:

      activity.begin(Git.Activity.Phase.Receiving, true)
      Transfer.copy(hash => Store.read(objects, hash), wants, has, depth, store, activity)

    def upload
      ( objects: List[Git.Hash], read: Git.Hash => (Git.Object.Kind, Data), updates: List[RefUpdate],
        activity: Git.Activity^ )
      ( using Tactic[Git.Error] )
    :   List[(Text, Optional[Text])] = io:

      activity.begin(Git.Activity.Phase.Writing, true)
      activity.measure(Transfer.count(objects))

      def send(hash: Git.Hash): Unit =
        val (kind, data) = read(hash)
        Store.write(this.objects, kind, data)
        activity.advance()

      objects.each(send(_))

      // A branch checked out in the peer's working tree is not updated under it.
      val checkedOut: Optional[Text] = if location.worktree.absent then Unset else symbolicHead()
      val logged = logging()

      def apply(update: RefUpdate): (Text, Optional[Text]) =
        if checkedOut == Optional(update.ref) then (update.ref, t"branch is currently checked out")
        else
          val result: Optional[Text] =
            try
              throwErrors[Git.Error]:
                Refstore.update
                  ( location.gitDir, location.commonDir, update.ref, update.to, update.expected, t"push",
                    identity, logged )

              Unset
            catch
              case error: Git.Error => error.reason match
                case Reason.Moved(_, _, _) => t"failed to update ref"
                case Reason.Locked(_)      => t"failed to lock"
                case other                 => error.message.text

          (update.ref, result)

      updates.map(apply(_))
