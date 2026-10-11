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
import fulminate.*
import galilei.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import urticose.*
import vacuous.*

object Git:

  // A fully-qualified reference path, `refs/heads/main`, `refs/notes/commits`, …
  type Ref = Path on Git.Refs

  // ───────────────────────────────────────────── Errors ─────────────────────────────────────────

  object Error:
    enum Reason(val number: Int) extends Clarification:
      case CannotExecuteGit                                                 extends Reason(1)
      case NoRepository(path: Text)                                         extends Reason(2)
      case NoWorktree                                                       extends Reason(3)
      case Moved(ref: Text, expected: Optional[Git.Hash], actual: Optional[Git.Hash])
                                                                            extends Reason(4)
      case NoSuchRef(ref: Text)                                             extends Reason(5)
      case NoSuchObject(hash: Text)                                         extends Reason(6)
      case RefExists(ref: Text)                                             extends Reason(7)
      case Unmerged(branch: Git.Branch, tip: Git.Hash)                      extends Reason(8)
      case Conflict(paths: List[Text])                                      extends Reason(9)
      case NothingToCommit                                                  extends Reason(10)
      case Detached                                                         extends Reason(11)
      case Dirty(paths: List[Text])                                         extends Reason(12)
      case NotFastForward                                                   extends Reason(13)
      case OutsideWorktree(path: Text)                                      extends Reason(14)
      case Untracked(path: Text)                                            extends Reason(15)
      case NoOperationInProgress                                            extends Reason(16)
      case OperationInProgress(operation: Git.Operation)                    extends Reason(17)
      case NoSuchRemote(name: Text)                                         extends Reason(18)
      case RemoteExists(name: Text)                                         extends Reason(19)
      case Unauthorized(remote: Text)                                       extends Reason(20)
      case Unreachable(remote: Text)                                        extends Reason(21)
      case RemoteRejected(ref: Text, reason: Text)                          extends Reason(22)
      case LeaseFailed(ref: Text, expected: Optional[Git.Hash], actual: Optional[Git.Hash])
                                                                            extends Reason(23)
      case NoteNotFound                                                     extends Reason(24)
      case NoSuchConfig(key: Text)                                          extends Reason(25)
      case InvalidConfig(key: Text, value: Text)                            extends Reason(26)
      case BadSignature(detail: Text)                                       extends Reason(27)
      case SigningFailed(detail: Text)                                      extends Reason(28)
      case Unsupported(operation: Text)                                     extends Reason(29)
      case Unexpected                                                       extends Reason(30)
      case NoteExists(target: Git.Hash)                                     extends Reason(31)
      case NoSuchWorktree(path: Text)                                       extends Reason(32)
      case NoSuchStash                                                      extends Reason(33)

    private def hash(hash: Optional[Git.Hash]): Text = hash.lay(t"nothing")(_.text)

    given communicable: Reason is Communicable =
      case Reason.CannotExecuteGit         => m"the `git` command could not be executed"
      case Reason.NoRepository(path)       => m"$path is not a Git repository"
      case Reason.NoWorktree               => m"the repository has no working tree"
      case Reason.Moved(ref, expected, actual) =>
        m"$ref was ${hash(expected)} when read but is now ${hash(actual)}"
      case Reason.NoSuchRef(ref)           => m"the reference $ref does not exist"
      case Reason.NoSuchObject(hash)       => m"the object $hash does not exist"
      case Reason.RefExists(ref)           => m"the reference $ref already exists"
      case Reason.Unmerged(branch, tip)    => m"the branch $branch at $tip is not fully merged"
      case Reason.Conflict(paths)          => m"there are conflicts in ${paths.join(t", ")}"
      case Reason.NothingToCommit          => m"there is nothing to commit"
      case Reason.Detached                 => m"HEAD is detached"
      case Reason.Dirty(paths)             => m"local changes would be overwritten in ${paths.join(t", ")}"
      case Reason.NotFastForward           => m"the update is not a fast-forward"
      case Reason.OutsideWorktree(path)    => m"$path is outside the working tree"
      case Reason.Untracked(path)          => m"$path is not tracked"
      case Reason.NoOperationInProgress    => m"no merge, rebase, cherry-pick or revert is in progress"
      case Reason.OperationInProgress(op)  => m"an operation is already in progress"
      case Reason.NoSuchRemote(name)       => m"the remote $name does not exist"
      case Reason.RemoteExists(name)       => m"the remote $name already exists"
      case Reason.Unauthorized(remote)     => m"authentication with $remote failed"
      case Reason.Unreachable(remote)      => m"the remote $remote could not be reached"
      case Reason.RemoteRejected(ref, why) => m"the remote rejected $ref: $why"
      case Reason.LeaseFailed(ref, expected, actual) =>
        m"the remote's $ref was expected to be ${hash(expected)} but is ${hash(actual)}"
      case Reason.NoteNotFound             => m"no note was attached to the given object"
      case Reason.NoSuchConfig(key)        => m"the configuration key $key is not set"
      case Reason.InvalidConfig(key, value) => m"$value is not a valid value for $key"
      case Reason.BadSignature(detail)     => m"the signature could not be verified: $detail"
      case Reason.SigningFailed(detail)    => m"the object could not be signed: $detail"
      case Reason.Unsupported(operation)   => m"$operation is not supported by this backend"
      case Reason.Unexpected               => m"git reported an unexpected failure"
      case Reason.NoteExists(target)       => m"a note is already attached to $target"
      case Reason.NoSuchWorktree(path)     => m"$path is not a linked working tree"
      case Reason.NoSuchStash              => m"the stash entry does not exist"

  // `detail` is what git itself said, when a backend has it: its standard error, trimmed.
  case class Error(reason: Git.Error.Reason, detail: Optional[Text] = Unset)(using Diagnostics)
  extends fulminate.Error(685, reason.number)
    ( m"the Git operation could not be completed because $reason" )

  object RefError:
    enum Reason(val number: Int) extends Clarification:
      case LeadingOrTrailingDot extends Reason(1)
      case ReservedSuffix       extends Reason(2)
      case ReservedSequence     extends Reason(3)
      case DoubleDot            extends Reason(4)
      case EmptySegment         extends Reason(5)
      case InvalidCharacter     extends Reason(6)
      case BadHash              extends Reason(7)
      case BadRefspec           extends Reason(8)

    given communicable: Reason is Communicable =
      case Reason.LeadingOrTrailingDot => m"a path segment starts or ends with `.`"
      case Reason.ReservedSuffix       => m"a path segment ends with the reserved suffix `.lock`"
      case Reason.ReservedSequence     => m"the name contains the reserved sequence `@{`"
      case Reason.DoubleDot            => m"the name contains `..`"
      case Reason.EmptySegment         => m"the name contains an empty path segment"
      case Reason.InvalidCharacter     => m"the name contains a character forbidden by Git"
      case Reason.BadHash              => m"the value is not a 40-character hexadecimal hash"
      case Reason.BadRefspec           => m"the value is not a valid refspec"

  case class RefError(value: Text, reason: Git.RefError.Reason)(using Diagnostics)
  extends fulminate.Error(976, reason.number)
    ( m"$value is not a valid Git reference because $reason" )

  // ───────────────────────────────────────── Reference paths ──────────────────────────────────

  // The Serpentine path scheme for fully-qualified Git references, rooted at `refs/`. Component
  // validation reproduces `git check-ref-format`.
  object Refs extends Root(t"refs/"):
    type Plane = Git.Refs

    def notes(namespace: Text)(using Tactic[Git.RefError]): Git.Ref =
      validateSegment(namespace)
      Git.Refs / t"notes" / namespace

    def heads(branch: Text)(using Tactic[Git.RefError]): Git.Ref =
      validateSegment(branch)
      Git.Refs / t"heads" / branch

    def tags(tag: Text)(using Tactic[Git.RefError]): Git.Ref =
      validateSegment(tag)
      Git.Refs / t"tags" / tag

    def remotes(remote: Text, branch: Text)(using Tactic[Git.RefError]): Git.Ref =
      validateSegment(remote)
      validateSegment(branch)
      Git.Refs / t"remotes" / remote / branch

    val defaultNotes: Git.Ref = unsafely(Git.Refs.notes(t"commits"))
    val stash: Git.Ref = Git.Refs / t"stash"

    // Decodes a fully-qualified name such as `refs/heads/main`.
    def parse(text: Text)(using Tactic[Git.RefError]): Git.Ref =
      if !text.starts(t"refs/") then abort(Git.RefError(text, Git.RefError.Reason.InvalidCharacter))

      def recur(path: Git.Ref, segments: List[Text]): Git.Ref = segments match
        case Nil             => path
        case segment :: rest =>
          validateSegment(segment)
          recur(path / segment, rest)

      recur(Git.Refs, text.skip(5).cut(t"/"))

    def validateSegment(segment: Text)(using Tactic[Git.RefError]): Unit =
      def fail(reason: Git.RefError.Reason) = abort(Git.RefError(segment, reason))
      if segment.length == 0     then fail(Git.RefError.Reason.EmptySegment)
      if segment.starts(t".")    then fail(Git.RefError.Reason.LeadingOrTrailingDot)
      if segment.ends(t".")      then fail(Git.RefError.Reason.LeadingOrTrailingDot)
      if segment.ends(t".lock")  then fail(Git.RefError.Reason.ReservedSuffix)
      if segment.contains(t"@{") then fail(Git.RefError.Reason.ReservedSequence)
      if segment.contains(t"..") then fail(Git.RefError.Reason.DoubleDot)

      List('*', '[', '\\', ' ', '^', '~', ':', '?', '/').each: ch =>
        if segment.contains(ch) then fail(Git.RefError.Reason.InvalidCharacter)

    given admissible: [text <: Text] => text is Admissible on Git.Refs = _ => ()

    given filesystem: Git.Refs is Filesystem:
      val name: Text = t"Git.Refs"
      val separator: Text = t"/"
      val self: Text = t"@"
      val parent: Text = t".."

    // A validated ref path satisfies every rule git enforces, so it is a revision as it stands.
    given pathIsRevision: Conversion[Git.Ref, Git.Revision] = path => Git.Revision.unsafe(path.encode)

  trait Refs

  // ──────────────────────────────────────────── Revisions ─────────────────────────────────────

  // Anything git accepts where a revision is expected (`gitrevisions(7)`): a hash, a branch, a
  // tag, `HEAD~2`, `main..feature`. `Git.Hash`, `Git.Branch` and `Git.Tag` are revisions; other
  // expressions are constructed by `Revision.head` or, without validation, `Revision.unsafe`.
  object Revision:
    def head(n: Int = 0): Revision = if n == 0 then unsafe(t"HEAD") else unsafe(t"HEAD~$n")
    def unsafe(text: Text): Revision = Expression(text)

    // Validates a reference *name* (not an expression) against `check-ref-format`.
    def parse(text: Text)(using Tactic[Git.RefError]): Text =
      def fail(reason: Git.RefError.Reason): Text = abort(Git.RefError(text, reason))

      text.cut(t"/").each: part =>
        if part.starts(t".") || part.ends(t".") then fail(Git.RefError.Reason.LeadingOrTrailingDot)
        if part.ends(t".lock")                  then fail(Git.RefError.Reason.ReservedSuffix)
        if part.contains(t"@{")                 then fail(Git.RefError.Reason.ReservedSequence)
        if part.contains(t"..")                 then fail(Git.RefError.Reason.DoubleDot)
        if part.length == 0                     then fail(Git.RefError.Reason.EmptySegment)

        for char <- List('*', '[', '\\', ' ', '^', '~', ':', '?')
        do if part.contains(char) then fail(Git.RefError.Reason.InvalidCharacter)

      text

    given encodable: Revision is Encodable in Text = _.text
    given showable: Revision is Showable = _.text

    given inspectable: [revision <: Revision] => revision is Inspectable = revision =>
      t"Revision(${revision.text})"

    private case class Expression(text: Text) extends Revision

  trait Revision:
    def text: Text

  // A fetch or push refspec, `+refs/heads/*:refs/remotes/origin/*`. A refspec with no source
  // and a destination deletes the destination on push.
  object Refspec:
    def parse(text: Text)(using Tactic[Git.RefError]): Refspec =
      val force = text.starts(t"+")
      val body = if force then text.skip(1) else text

      body.cut(t":") match
        case List(source)              => Refspec(source, Unset, force)
        case List(t"", destination)    => Refspec(Unset, destination, force)
        case List(source, destination) => Refspec(source, destination, force)
        case _ => abort(Git.RefError(text, Git.RefError.Reason.BadRefspec))

    def apply(ref: Git.Ref): Refspec = Refspec(ref.encode, ref.encode, false)
    def apply(source: Git.Ref, destination: Git.Ref): Refspec =
      Refspec(source.encode, destination.encode, false)

    given encodable: Refspec is Encodable in Text = _.text
    given showable: Refspec is Showable = _.text

  case class Refspec(source: Optional[Text], destination: Optional[Text], force: Boolean = false):
    def text: Text =
      val prefix = if force then t"+" else t""
      destination.lay(t"$prefix${source.or(t"")}"): destination =>
        t"$prefix${source.or(t"")}:$destination"

  // ───────────────────────────────────────── Typed references ─────────────────────────────────

  object Tag:
    def unsafe(text: Text): Git.Tag = new Git.Tag(text)

    def parse(text: Text)(using Tactic[Git.RefError]): Git.Tag =
      new Git.Tag(Git.Revision.parse(text))

    given decoder: (tactic: Tactic[Git.RefError]) => ((Git.Tag is Decodable in Text)^{tactic}) =
      parse(_)

    given showable: Git.Tag is Showable = _.text
    given inspectable: [tag <: Git.Tag] => tag is Inspectable = tag => t"Tag(${tag.text})"

    // What a tag name points at: a commit directly, or an annotation object which in turn
    // points at a commit.
    enum Target:
      case Lightweight(commit: Git.Hash)
      case Annotated(annotation: Git.Hash, commit: Git.Hash)

      // The commit the tag ultimately names.
      def peeled: Git.Hash = this match
        case Lightweight(commit)   => commit
        case Annotated(_, commit)  => commit

    // The content of an annotated tag object.
    case class Annotation
      ( target:    Git.Hash,
        kind:      Git.Object.Kind,
        name:      Git.Tag,
        tagger:    Optional[Git.Signature],
        message:   Text,
        signature: Optional[Text] )

  case class Tag(text: Text) extends Git.Revision:
    def ref: Git.Ref = Git.Refs / t"tags" / text

  object Branch:
    def unsafe(text: Text): Git.Branch = new Git.Branch(text)

    def parse(text: Text)(using Tactic[Git.RefError]): Git.Branch =
      new Git.Branch(Git.Revision.parse(text))

    given decoder: (tactic: Tactic[Git.RefError]) => ((Git.Branch is Decodable in Text)^{tactic}) =
      parse(_)

    given showable: Git.Branch is Showable = _.text
    given inspectable: [branch <: Git.Branch] => branch is Inspectable = branch =>
      t"Branch(${branch.text})"

  case class Branch(text: Text) extends Git.Revision:
    def ref: Git.Ref = Git.Refs / t"heads" / text

  // `Git.Hash` extends Serpentine's `Root`, so a hash is the root of a notes path:
  // `hash / t"foo" / t"bar"` is a `NoteRef`. Equality is by hash text.
  object Hash:
    def apply(text: Text)(using Tactic[Git.RefError]): Git.Hash = text match
      case r"[a-f0-9]{40}" => new Git.Hash(text)
      case _               => abort(Git.RefError(text, Git.RefError.Reason.BadHash))

    def unsafe(text: Text): Git.Hash = new Git.Hash(text)

    // The all-zero hash, which git uses to mean "no object".
    val zero: Git.Hash = unsafe(t"0000000000000000000000000000000000000000")

    given decoder: (tactic: Tactic[Git.RefError]) => ((Git.Hash is Decodable in Text)^{tactic}) =
      apply(_)

    given showable: Git.Hash is Showable = _.text
    given inspectable: [hash <: Git.Hash] => hash is Inspectable = hash => t"Hash(${hash.text})"

  class Hash(val text: Text) extends Root(t"$text/"), Git.Revision:
    type Plane = Notes
    type Limit = Git.Hash

    override def hashCode: Int = text.hashCode

    override def equals(any: Any): Boolean = any match
      case other: Git.Hash => text == other.text
      case _               => false

  // ─────────────────────────────────────────── Identities ─────────────────────────────────────

  case class Identity(name: Text, email: Text):
    def text: Text = t"$name <$email>"

  // Git's own timestamp: seconds since the epoch and a UTC offset in minutes, rendered as
  // `1700000000 +0130`. Kept apart from any calendar type so that it round-trips byte-for-byte.
  object Timestamp:
    def now(): Timestamp =
      val now = java.time.ZonedDateTime.now().nn
      Timestamp(now.toEpochSecond, now.getOffset.nn.getTotalSeconds/60)

    def parse(seconds: Text, offset: Text): Optional[Timestamp] =
      val epoch: Optional[Long] = safely(seconds.as[Long])
      offset match
        case r"$sign([+-])$hours([0-9][0-9])$minutes([0-9][0-9])" =>
          val magnitude = safely(hours.as[Int]).or(0)*60 + safely(minutes.as[Int]).or(0)
          val signed = if sign == t"-" then -magnitude else magnitude
          epoch.let(Timestamp(_, signed))
        case _ =>
          Unset

  case class Timestamp(seconds: Long, offsetMinutes: Int):
    def offset: Text =
      val sign = if offsetMinutes < 0 then t"-" else t"+"
      val magnitude = if offsetMinutes < 0 then -offsetMinutes else offsetMinutes
      val hours = magnitude/60
      val minutes = magnitude%60
      val hoursText = if hours < 10 then t"0$hours" else t"$hours"
      val minutesText = if minutes < 10 then t"0$minutes" else t"$minutes"
      t"$sign$hoursText$minutesText"

    def text: Text = t"$seconds $offset"

  // An `author` or `committer` line: who, and when.
  object Signature:
    def parse(text: Text): Optional[Signature] = text match
      case r"$name(.*) <$email([^>]*)> $seconds([0-9]+) $offset([+-][0-9]{4})" =>
        val identity = Identity(name, email)
        Timestamp.parse(seconds, offset).let(Signature(identity, _))
      case _ =>
        Unset

  case class Signature(identity: Identity, timestamp: Timestamp):
    def text: Text = t"${identity.text} ${timestamp.text}"

  // ──────────────────────────────────────────── Objects ───────────────────────────────────────

  object Object:
    enum Kind:
      case Blob, Tree, Commit, Tag

      def text: Text = this match
        case Kind.Blob   => t"blob"
        case Kind.Tree   => t"tree"
        case Kind.Commit => t"commit"
        case Kind.Tag    => t"tag"

    object Kind:
      def parse(text: Text): Optional[Kind] = text match
        case t"blob"   => Kind.Blob
        case t"tree"   => Kind.Tree
        case t"commit" => Kind.Commit
        case t"tag"    => Kind.Tag
        case _         => Unset

    object Tree:
      enum Mode(val octal: Text):
        case Regular    extends Mode(t"100644")
        case Executable extends Mode(t"100755")
        case Symlink    extends Mode(t"120000")
        case Gitlink    extends Mode(t"160000")
        case Directory  extends Mode(t"40000")

      object Mode:
        def parse(text: Text): Optional[Mode] = text match
          case t"100644" | t"100664" => Regular
          case t"100755"             => Executable
          case t"120000"             => Symlink
          case t"160000"             => Gitlink
          case t"40000" | t"040000"  => Directory
          case _                     => Unset

      case class Entry(mode: Mode, name: Text, hash: Git.Hash):
        def kind: Kind = mode match
          case Mode.Directory => Kind.Tree
          case Mode.Gitlink   => Kind.Commit
          case _              => Kind.Blob

  enum Object:
    case Blob(data: Data)
    case Tree(entries: List[Git.Object.Tree.Entry])
    case Commit(commit: octogenarian.Commit)
    case Tag(annotation: Git.Tag.Annotation)

    def kind: Git.Object.Kind = this match
      case Object.Blob(_)   => Git.Object.Kind.Blob
      case Object.Tree(_)   => Git.Object.Kind.Tree
      case Object.Commit(_) => Git.Object.Kind.Commit
      case Object.Tag(_)    => Git.Object.Kind.Tag

  // ───────────────────────────────────────────── HEAD ─────────────────────────────────────────

  enum Head:
    // `tip` is `Unset` on an unborn branch: a fresh repository whose first commit has not been
    // made.
    case Attached(branch: Git.Branch, tip: Optional[Git.Hash])
    case Detached(at: Git.Hash)

    def commit: Optional[Git.Hash] = this match
      case Attached(_, tip) => tip
      case Detached(at)     => at

    // The branch checked out, if HEAD is attached to one.
    def current: Optional[Git.Branch] = this match
      case Attached(branch, _) => branch
      case Detached(_)         => Unset

  // ──────────────────────────────────────────── Status ────────────────────────────────────────

  enum Status:
    case Updated, Added, Deleted, Renamed, Copied, Unmerged, Untracked, Ignored, TypeChanged

  object Status:
    def parse(char: Char): Optional[Status] = char match
      case 'M' => Updated
      case 'A' => Added
      case 'D' => Deleted
      case 'R' => Renamed
      case 'C' => Copied
      case 'U' => Unmerged
      case '?' => Untracked
      case '!' => Ignored
      case 'T' => TypeChanged
      case _   => Unset

  // One line of `status`: the state of the index relative to HEAD, the state of the working
  // file relative to the index, the path, and, for a rename or copy, where it came from. Paths
  // are on the worktree handle's plane, so they cannot outlive the handle.
  case class PathStatus[plane]
    ( index:    Optional[Git.Status],
      worktree: Optional[Git.Status],
      path:     Path on plane,
      origin:   Optional[Path on plane] )

  // ──────────────────────────────────────────── Activity ──────────────────────────────────────

  object Activity:
    enum Phase:
      case Idle, Enumerating, Counting, Compressing, Writing, Receiving, Resolving, Unpacking,
          Checkout, Done

    object Phase:
      def parse(text: Text): Optional[Phase] = text match
        case t"Enumerating objects" => Enumerating
        case t"Counting objects"    => Counting
        case t"Compressing objects" => Compressing
        case t"Writing objects"     => Writing
        case t"Receiving objects"   => Receiving
        case t"Resolving deltas"    => Resolving
        case t"Unpacking objects"   => Unpacking
        case t"Updating files"      => Checkout
        case _                      => Unset

    // One observation of an activity: pure data, stale as soon as it is read. `remote` is set
    // while the phase is being reported by the other end of a transfer.
    case class Report
      ( phase: Phase, remote: Boolean, done: Long, total: Optional[Long], bytes: Long ):

      def complete: Optional[Double] = total.let: total =>
        if total == 0L then 1.0 else done.toDouble/total.toDouble

  // The progress of a long-running operation, shared between the thread doing the work and any
  // thread drawing it. It is the honest shape for shared mutation: a handful of atomic cells,
  // each updated indivisibly, with no intermediate state another thread can observe half-done.
  // Updating is allocation-free: a backend bumps the counters as a sideband line arrives or an
  // object is processed. A user interface polls `report()` at its own cadence, or, for a
  // reactive one, drains `updates`, which exists only once asked for, so a poll-only caller
  // pays nothing for it.
  class Activity() extends Durable:
    private val phase: Atomic.Ref[Activity.Phase] = Atomic(Activity.Phase.Idle)
    private val remote: Atomic.Bool = Atomic(false)
    private val done: Atomic.Long = Atomic(0L)
    private val total: Atomic.Long = Atomic(-1L)
    private val bytes: Atomic.Long = Atomic(0L)
    private val finished: Atomic.Bool = Atomic(false)
    private val relay: Atomic.Ref[Optional[Relay[Activity.Report]]] = Atomic(Unset)

    def report(): Stale[Activity.Report] =
      val totalValue = total()
      Stale:
        Activity.Report
          ( phase(), remote(), done(), if totalValue < 0L then Unset else totalValue, bytes() )

    def complete: Boolean = finished()

    // The push feed, created on first request and fed from then on; it ends when the activity
    // finishes.
    def updates: Relay[Activity.Report] = relay().or:
      val fresh: Relay[Activity.Report] = Relay()
      if relay.replace(Unset, fresh) then fresh else relay().or(fresh)

    private def publish(): Unit = relay().let(_.put(report().value))

    def begin(phase: Activity.Phase, remote: Boolean = false): Unit =
      this.phase() = phase
      this.remote() = remote
      done() = 0L
      total() = -1L
      publish()

    def measure(total: Long): Unit =
      this.total() = total
      publish()

    def advance(count: Long = 1L): Unit =
      done.since(_ + count)
      publish()

    def progress(done: Long, total: Long): Unit =
      this.done() = done
      this.total() = total
      publish()

    def transferred(count: Long): Unit =
      bytes.since(_ + count)

    def finish(): Unit =
      phase() = Activity.Phase.Done
      finished() = true
      publish()
      relay().let(_.stop())

  // ───────────────────────────────────────────── Config ───────────────────────────────────────

  object Config:
    enum Scope:
      case Local, Worktree, Global, System

    object Key:
      def parse(text: Text)(using Tactic[Git.Error]): Key =
        val first = text.s.indexOf('.')
        val last = text.s.lastIndexOf('.')
        if first < 1 || last >= text.length - 1 then abort(Git.Error(Git.Error.Reason.InvalidConfig(text, t"")))
        val section = text.keep(first).lower
        val name = text.skip(last + 1).lower
        val subsection: Optional[Text] = if first == last then Unset else text.s.substring(first + 1, last).nn.tt
        Key(section, subsection, name)

      given showable: Key is Showable = _.text

    case class Key(section: Text, subsection: Optional[Text], name: Text):
      def text: Text = subsection.lay(t"$section.$name") { sub => t"$section.$sub.$name" }

    // The configuration file a handle reads and writes: a repository's own, or a named file
    // (`~/.gitconfig`). `Effective` reads through every scope git would consult for the
    // repository, and cannot be written.
    enum Source:
      case Effective(gitDir: Path on Linux)
      case File(path: Path on Linux)
      case Repository(gitDir: Path on Linux, scope: Scope)

    object Handle:
      extension (config: (Git.Config.Handle & Granting[Grant.Read])^)
        def get[value: Decodable in Text](key: Key)(using Tactic[Git.Error]): Stale[Optional[value]] =
          Stale(config.backend.configRead(config.source).reap { case (`key`, text) => text }.let(_.as[value]))

        def all[value: Decodable in Text](key: Key)(using Tactic[Git.Error]): Stale[List[value]] =
          Stale(config.backend.configRead(config.source).sweep { case (`key`, text) => text.as[value] })

        def list()(using Tactic[Git.Error]): Stale[List[(Key, Text)]] =
          Stale(config.backend.configRead(config.source))

      extension (config: (Git.Config.Handle & Granting[Grant.Write])^)
        def set[value: Encodable in Text](key: Key, value: value)(using Tactic[Git.Error]): Unit =
          config.backend.configWrite(config.source, key, value.encode, false)

        def add[value: Encodable in Text](key: Key, value: value)(using Tactic[Git.Error]): Unit =
          config.backend.configWrite(config.source, key, value.encode, true)

        def unset(key: Key, all: Boolean = false)(using Tactic[Git.Error]): Unit =
          config.backend.configUnset(config.source, key, all)

    trait Handle extends caps.ExclusiveCapability:
      val source: Source
      private[octogenarian] val backend: Git.Backend^

    class Openable[path <: Path on Linux](using backend: Git.Backend^, tactic: Tactic[Git.Error])
    extends aperture.Openable:
      type Self = path
      type Form = Git.Config
      type Operand = Nothing
      type Result = Handle

      def open[grants <: Grant, result]
        ( value: path, mode: Mode granting grants, flags: List[Nothing] )
        ( block: ((Handle & Granting[grants])^) ?=> result )
      :   result =

        val handle =
          new Handle with Granting[grants]:
            val source: Source = Source.File(value)
            val backend: Git.Backend^ = Openable.this.backend

        block(using handle)

    given openable: [path <: Path on Linux]
    =>  ( backend: Git.Backend^, tactic: Tactic[Git.Error] )
    =>  ( Openable[path]^{backend, tactic, caps.any} ) =
      Openable[path]()

  trait Config

  // ───────────────────────────────────── In-progress operations ───────────────────────────────

  // What `.git` records about an operation that has stopped part-way: a merge, rebase,
  // cherry-pick or revert waiting for conflicts to be resolved, or a bisection.
  enum Operation:
    case Merging(merging: List[Git.Hash], conflicting: List[Text])
    case Rebasing(onto: Git.Hash, originalHead: Git.Hash, done: Int, remaining: Int, conflicting: List[Text])
    case CherryPicking(commit: Git.Hash, conflicting: List[Text])
    case Reverting(commit: Git.Hash, conflicting: List[Text])
    case Bisecting

    // The paths left in conflict, which must be resolved before the operation continues.
    def conflicts: List[Text] = this match
      case Merging(_, conflicting)             => conflicting
      case Rebasing(_, _, _, _, conflicting)   => conflicting
      case CherryPicking(_, conflicting)       => conflicting
      case Reverting(_, conflicting)           => conflicting
      case Bisecting                           => Nil

  object Sequence:
    enum Outcome:
      case Completed(head: Git.Hash)
      case Stopped(operation: Git.Operation)

    enum Verb:
      case Continue, Skip, Abort, Quit

  object Merge:
    enum Outcome:
      case FastForwarded(head: Git.Hash)
      case Merged(head: Git.Hash)
      case UpToDate
      case Stopped(operation: Git.Operation)

  object Rebase:
    enum Option:
      case Autosquash, KeepEmpty, NoRenames, Signoff, Autostash

  object Pull:
    enum Mode:
      case Merge, Rebase, FastForwardOnly

  object Push:
    enum Force:
      case Never
      case Always
      // `--force-with-lease`: the remote-side counterpart of a compare-and-swap, from values the
      // remote's refs were seen to have.
      case Lease(expectations: Map[Git.Ref, Stale[Optional[Git.Hash]]])

    enum Outcome:
      case Updated(from: Git.Hash, to: Git.Hash, forced: Boolean)
      case Created(to: Git.Hash)
      case Deleted(from: Git.Hash)
      case UpToDate
      case Rejected(reason: Text)

  object Stash:
    case class Entry(index: Int, commit: Git.Hash, message: Text)

  // ──────────────────────────────────────────── Signing ───────────────────────────────────────

  // Produces an ASCII-armoured signature over a commit's or tag's payload; the handle inserts it
  // as the `gpgsig` header of a commit or the trailing block of a tag.
  trait Signer:
    def sign(payload: Data)(using Tactic[Git.Error]): Text

  trait Verifier:
    def verify(payload: Data, signature: Text)(using Tactic[Git.Error]): Verification

  enum Verification:
    case Good(signer: Text)
    case Bad(detail: Text)
    case Unknown(detail: Text)

  // ──────────────────────────────────────────── Backend ───────────────────────────────────────

  object Backend:
    // Where a repository was found: its own git directory, the shared one (identical unless the
    // path is a linked worktree) and the working tree, if it has one.
    case class Location(gitDir: Path on Linux, commonDir: Path on Linux, worktree: Optional[Path on Linux])

    // What a ref is expected to hold when it is updated: anything, nothing, or a particular
    // object. (An `Optional[Optional[Hash]]` would collapse, `Optional` being a union.)
    object Expectation:
      def apply(hash: Optional[Git.Hash]): Expectation = hash.lay(Expectation.Absent)(Expectation.Exactly(_))

    enum Expectation:
      case Unconditional
      case Absent
      case Exactly(hash: Git.Hash)

      def expected: Optional[Git.Hash] = this match
        case Exactly(hash) => hash
        case _             => Unset

    // A ref's content: the object it names, or the ref it points to.
    enum Target:
      case Direct(hash: Git.Hash)
      case Symbolic(ref: Text)

    case class StatusLine(index: Optional[Git.Status], worktree: Optional[Git.Status], path: Text, origin: Optional[Text])

    enum NoteEdit:
      case Add(target: Git.Hash, body: Text, force: Boolean)
      case Append(target: Git.Hash, body: Text)
      case Remove(target: Git.Hash, ignoreMissing: Boolean)
      case Copy(source: Git.Hash, target: Git.Hash, force: Boolean)

    case class WorktreeEntry
      ( path:     Path on Linux,
        head:     Optional[Git.Hash],
        branch:   Optional[Git.Branch],
        bare:     Boolean,
        locked:   Optional[Text],
        prunable: Optional[Text] )

  // The implementation a repository handle drives. Two exist: one that runs the `git`
  // command (`gitBackends.commandlineGit`) and one that reads and writes the repository's files
  // itself (`gitBackends.soundnessGit`). The primitives on which the handle's own logic rests —
  // refs, objects, the index, configuration — are plumbing, so compare-and-swap, object assembly
  // and signing are written once; the algorithmic verbs are implemented by each backend in its
  // own way.
  trait Backend:
    def locate(path: Path on Linux)(using Tactic[Git.Error]): Optional[Backend.Location]

    def init(target: Path on Linux, initialBranch: Optional[Git.Branch], bare: Boolean)
      ( using Tactic[Git.Error] )
    :   Unit

    def clone
      ( source:    Text,
        target:    Path on Linux,
        branch:    Optional[Git.Branch],
        depth:     Optional[Int],
        bare:      Boolean,
        recursive: Boolean,
        activity:  Activity^ )
      ( using Internet, Tactic[Git.Error] )
    :   Unit

    // Refs
    def head(gitDir: Path on Linux)(using Tactic[Git.Error]): Git.Head
    def readRef(gitDir: Path on Linux, ref: Text)(using Tactic[Git.Error]): Optional[Backend.Target]
    def listRefs(gitDir: Path on Linux, prefix: Optional[Text])(using Tactic[Git.Error]): List[(Text, Git.Hash)]
    def peel(gitDir: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error]): Git.Hash

    def updateRef
      ( gitDir:   Path on Linux,
        ref:      Text,
        to:       Optional[Git.Hash],
        expected: Backend.Expectation,
        message:  Optional[Text] )
      ( using Tactic[Git.Error] )
    :   Unit

    def updateSymbolicRef(gitDir: Path on Linux, name: Text, target: Text)(using Tactic[Git.Error]): Unit
    def resolve(gitDir: Path on Linux, revision: Text)(using Tactic[Git.Error]): Optional[Git.Hash]

    def revList
      ( gitDir: Path on Linux, from: Text, until: Optional[Text], limit: Optional[Int], paths: List[Text] )
      ( using Tactic[Git.Error] )
    :   List[Git.Hash]

    def reflog(gitDir: Path on Linux, ref: Optional[Text])(using Tactic[Git.Error]): List[ReflogEntry]
    def mergeBase(gitDir: Path on Linux, left: Git.Hash, right: Git.Hash)(using Tactic[Git.Error]): Optional[Git.Hash]
    def isAncestor(gitDir: Path on Linux, ancestor: Git.Hash, descendant: Git.Hash)(using Tactic[Git.Error]): Boolean

    // Objects
    def readObject(gitDir: Path on Linux, hash: Git.Hash)(using Tactic[Git.Error]): Optional[(Git.Object.Kind, Data)]
    def readObjects(gitDir: Path on Linux, hashes: List[Git.Hash])(using Tactic[Git.Error]): List[(Git.Hash, Git.Object.Kind, Data)]
    def writeObject(gitDir: Path on Linux, kind: Git.Object.Kind, data: Data)(using Tactic[Git.Error]): Git.Hash
    def diffTree(gitDir: Path on Linux, left: Git.Hash, right: Git.Hash)(using Tactic[Git.Error]): List[FileDiff]

    // Index and working files
    def status(gitDir: Path on Linux, worktree: Path on Linux, ignored: Boolean, untracked: Boolean)
      ( using Tactic[Git.Error] )
    :   List[Backend.StatusLine]

    def add(gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], all: Boolean)(using Tactic[Git.Error]): Unit
    def remove(gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], cached: Boolean, force: Boolean)(using Tactic[Git.Error]): Unit
    def move(gitDir: Path on Linux, worktree: Path on Linux, from: Text, to: Text)(using Tactic[Git.Error]): Unit

    def restore
      ( gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], source: Optional[Text],
        staged: Boolean, workingFiles: Boolean )
      ( using Tactic[Git.Error] )
    :   Unit

    def clean(gitDir: Path on Linux, worktree: Path on Linux, paths: List[Text], directories: Boolean, ignored: Boolean)
      ( using Tactic[Git.Error] )
    :   List[Text]

    def writeTree(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): Git.Hash

    // Brings the index (and, for `Hard`, the working files) into line with `tree`, as `reset`
    // does once HEAD has moved.
    def readTree(gitDir: Path on Linux, worktree: Path on Linux, mode: ResetMode)(using Tactic[Git.Error]): Unit

    def diffWorktree(gitDir: Path on Linux, worktree: Path on Linux, staged: Boolean, against: Optional[Text])
      ( using Tactic[Git.Error] )
    :   List[FileDiff]

    def tracked(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): List[Text]
    def conflicts(gitDir: Path on Linux, worktree: Path on Linux)(using Tactic[Git.Error]): List[Text]

    // Porcelain: each backend in its own way
    def checkout(gitDir: Path on Linux, worktree: Path on Linux, revision: Text)(using Tactic[Git.Error]): Unit
    def switch(gitDir: Path on Linux, worktree: Path on Linux, branch: Git.Branch, create: Boolean)(using Tactic[Git.Error]): Unit

    def merge
      ( gitDir: Path on Linux, worktree: Path on Linux, revision: Text, ff: FastForward,
        message: Optional[Text], sign: Boolean )
      ( using Tactic[Git.Error] )
    :   Git.Merge.Outcome

    def cherryPick(gitDir: Path on Linux, worktree: Path on Linux, commits: List[Git.Hash], noCommit: Boolean)
      ( using Tactic[Git.Error] )
    :   Git.Sequence.Outcome

    def revert(gitDir: Path on Linux, worktree: Path on Linux, commits: List[Git.Hash], noCommit: Boolean)
      ( using Tactic[Git.Error] )
    :   Git.Sequence.Outcome

    def rebase
      ( gitDir: Path on Linux, worktree: Path on Linux, onto: Text, upstream: Optional[Text],
        options: List[Git.Rebase.Option] )
      ( using Tactic[Git.Error] )
    :   Git.Sequence.Outcome

    def sequence(gitDir: Path on Linux, worktree: Path on Linux, verb: Git.Sequence.Verb)(using Tactic[Git.Error]): Git.Sequence.Outcome
    def operation(gitDir: Path on Linux, worktree: Optional[Path on Linux])(using Tactic[Git.Error]): Optional[Git.Operation]

    def stash(gitDir: Path on Linux, worktree: Path on Linux, message: Optional[Text], includeUntracked: Boolean, keepIndex: Boolean)
      ( using Tactic[Git.Error] )
    :   Optional[Git.Hash]

    def stashes(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Git.Stash.Entry]

    def applyStash(gitDir: Path on Linux, worktree: Path on Linux, entry: Int, index: Boolean, drop: Boolean)
      ( using Tactic[Git.Error] )
    :   Git.Sequence.Outcome

    def dropStash(gitDir: Path on Linux, entry: Int)(using Tactic[Git.Error]): Unit

    // Notes. `editNotes` makes a new notes commit, whose parent is `base`, on no ref: the handle
    // then moves the notes ref to it by compare-and-swap.
    def readNote(gitDir: Path on Linux, ref: Text, target: Git.Hash)(using Tactic[Git.Error]): Optional[Text]
    def listNotes(gitDir: Path on Linux, ref: Text)(using Tactic[Git.Error]): List[(Git.Hash, Git.Hash)]
    def editNotes(gitDir: Path on Linux, base: Optional[Git.Hash], edit: Backend.NoteEdit)(using Tactic[Git.Error]): Git.Hash

    // Config
    def configRead(source: Git.Config.Source)(using Tactic[Git.Error]): List[(Git.Config.Key, Text)]
    def configWrite(source: Git.Config.Source, key: Git.Config.Key, value: Text, add: Boolean)(using Tactic[Git.Error]): Unit
    def configUnset(source: Git.Config.Source, key: Git.Config.Key, all: Boolean)(using Tactic[Git.Error]): Unit

    // Remotes and transport
    def remoteRefs(gitDir: Path on Linux, remote: Text)(using Internet, Tactic[Git.Error]): List[(Text, Git.Hash)]

    def fetch
      ( gitDir:   Path on Linux,
        remote:   Text,
        refspecs: List[Git.Refspec],
        depth:    Optional[Int],
        prune:    Boolean,
        tags:     Boolean,
        activity: Activity^ )
      ( using Internet, Tactic[Git.Error] )
    :   List[(Text, Git.Hash)]

    def push
      ( gitDir:      Path on Linux,
        remote:      Text,
        refspecs:    List[Git.Refspec],
        force:       Git.Push.Force,
        tags:        Boolean,
        setUpstream: Boolean,
        activity:    Activity^ )
      ( using Internet, Tactic[Git.Error] )
    :   List[(Text, Git.Push.Outcome)]

    // Linked worktrees
    def worktrees(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Backend.WorktreeEntry]

    def addWorktree
      ( gitDir: Path on Linux, target: Path on Linux, revision: Text, detach: Boolean,
        newBranch: Optional[Git.Branch] )
      ( using Tactic[Git.Error] )
    :   Unit

    def removeWorktree(gitDir: Path on Linux, path: Path on Linux, force: Boolean)(using Tactic[Git.Error]): Unit
    def lockWorktree(gitDir: Path on Linux, path: Path on Linux, reason: Optional[Text])(using Tactic[Git.Error]): Unit
    def unlockWorktree(gitDir: Path on Linux, path: Path on Linux)(using Tactic[Git.Error]): Unit
    def moveWorktree(gitDir: Path on Linux, path: Path on Linux, to: Path on Linux)(using Tactic[Git.Error]): Unit
    def pruneWorktrees(gitDir: Path on Linux)(using Tactic[Git.Error]): List[Path on Linux]

  // ────────────────────────────────────────── Open flags ──────────────────────────────────────

  enum OpenFlag:
    // Wait for a conflicting handle in this JVM to close, rather than failing with `Busy`.
    case Await

  // ──────────────────────────────────────── Repository handle ─────────────────────────────────

  object Repo:
    import Git.Error.Reason as Reason

    trait Handle extends caps.ExclusiveCapability:
      val gitDir: Path on Linux
      val commonDir: Path on Linux
      val worktree: Optional[Path on Linux]
      val atoms: Set[Mode]
      private[octogenarian] val backend: Git.Backend^

    object Handle:
      // Reads. A read keyed by a hash returns a plain value: objects are immutable and
      // content-addressed. A read that resolves a ref, HEAD, a revision expression, the reflog
      // or the remote list returns a `Stale` value: what it names may have moved since.
      extension (repo: (Git.Repo.Handle & Granting[Grant.Read])^)
        def head()(using Tactic[Git.Error]): Stale[Git.Head] = Stale(repo.backend.head(repo.gitDir))

        def ref(ref: Git.Ref)(using Tactic[Git.Error]): Stale[Optional[Git.Hash]] =
          Stale(Operations.resolveRef(repo.backend, repo.gitDir, ref.encode))

        def refs(prefix: Optional[Git.Ref] = Unset)(using Tactic[Git.Error]): Stale[Map[Git.Ref, Git.Hash]] =
          Stale(Operations.refMap(repo.backend.listRefs(repo.gitDir, prefix.let(_.encode))))

        def symbolicRef(name: Text)(using Tactic[Git.Error]): Stale[Optional[Text]] =
          Stale:
            repo.backend.readRef(repo.gitDir, name) match
              case Git.Backend.Target.Symbolic(ref) => ref
              case _                                => Unset

        def resolve(revision: Git.Revision)(using Tactic[Git.Error]): Stale[Optional[Git.Hash]] =
          Stale(repo.backend.resolve(repo.gitDir, revision.text))

        def branches()(using Tactic[Git.Error]): Stale[Map[Git.Branch, Git.Hash]] =
          val prefix = t"refs/heads/"
          Stale:
            repo.backend.listRefs(repo.gitDir, prefix).sweep:
              case (name, hash) if name.starts(prefix) => Git.Branch.unsafe(name.skip(11)) -> hash
            . to[Map]

        def tags()(using Tactic[Git.Error]): Stale[Map[Git.Tag, Git.Hash]] =
          val prefix = t"refs/tags/"
          Stale:
            repo.backend.listRefs(repo.gitDir, prefix).sweep:
              case (name, hash) if name.starts(prefix) =>
                Git.Tag.unsafe(name.skip(10)) -> repo.backend.peel(repo.gitDir, hash)
            . to[Map]

        def tag(tag: Git.Tag)(using Tactic[Git.Error]): Stale[Optional[Git.Tag.Target]] =
          Stale:
            Operations.resolveRef(repo.backend, repo.gitDir, tag.ref.encode).let: hash =>
              repo.backend.readObject(repo.gitDir, hash) match
                case (Git.Object.Kind.Tag, _) =>
                  Git.Tag.Target.Annotated(hash, repo.backend.peel(repo.gitDir, hash))
                case _ =>
                  Git.Tag.Target.Lightweight(hash)

        def log
          ( from:  Git.Revision    = Git.Revision.head(),
            limit: Optional[Int]   = Unset,
            paths: List[Text]      = Nil )
          ( using Tactic[Git.Error] )
        :   Stale[List[Commit]] =

          Stale:
            val hashes = repo.backend.revList(repo.gitDir, from.text, Unset, limit, paths)
            repo.backend.readObjects(repo.gitDir, hashes).sweep:
              case (hash, Git.Object.Kind.Commit, data) => Objects.parseCommit(hash, data)

        def revList(from: Git.Revision, until: Optional[Git.Revision] = Unset, limit: Optional[Int] = Unset)
          ( using Tactic[Git.Error] )
        :   Stale[List[Git.Hash]] =

          Stale(repo.backend.revList(repo.gitDir, from.text, until.let(_.text), limit, Nil))

        def reflog(ref: Optional[Git.Ref] = Unset)(using Tactic[Git.Error]): Stale[List[ReflogEntry]] =
          Stale(repo.backend.reflog(repo.gitDir, ref.let(_.encode)))

        def mergeBase(left: Git.Hash, right: Git.Hash)(using Tactic[Git.Error]): Optional[Git.Hash] =
          repo.backend.mergeBase(repo.gitDir, left, right)

        def isAncestor(ancestor: Git.Hash, descendant: Git.Hash)(using Tactic[Git.Error]): Boolean =
          repo.backend.isAncestor(repo.gitDir, ancestor, descendant)

        def exists(hash: Git.Hash)(using Tactic[Git.Error]): Boolean =
          repo.backend.readObject(repo.gitDir, hash).present

        def show(hash: Git.Hash)(using Tactic[Git.Error]): Git.Object =
          Objects.parse(hash, Operations.readObject(repo.backend, repo.gitDir, hash))

        def commit(hash: Git.Hash)(using Tactic[Git.Error]): Commit = show(hash) match
          case Git.Object.Commit(commit) => commit
          case _ => abort(Git.Error(Reason.NoSuchObject(hash.text)))

        def tree(hash: Git.Hash)(using Tactic[Git.Error]): List[Git.Object.Tree.Entry] = show(hash) match
          case Git.Object.Tree(entries) => entries
          case _ => abort(Git.Error(Reason.NoSuchObject(hash.text)))

        def blob(hash: Git.Hash)(using Tactic[Git.Error]): Data = show(hash) match
          case Git.Object.Blob(data) => data
          case _ => abort(Git.Error(Reason.NoSuchObject(hash.text)))

        def annotation(hash: Git.Hash)(using Tactic[Git.Error]): Git.Tag.Annotation = show(hash) match
          case Git.Object.Tag(annotation) => annotation
          case _ => abort(Git.Error(Reason.NoSuchObject(hash.text)))

        def verifyCommit(hash: Git.Hash)(using verifier: Git.Verifier, tactic: Tactic[Git.Error]): Git.Verification =
          val (kind, data) = Operations.readObject(repo.backend, repo.gitDir, hash)
          if kind != Git.Object.Kind.Commit then abort(Git.Error(Reason.NoSuchObject(hash.text)))
          Objects.commitSignature(data).lay(Git.Verification.Unknown(t"the commit is not signed")): (payload, signature) =>
            verifier.verify(payload, signature)

        def verifyTag(tag: Git.Tag)(using verifier: Git.Verifier, tactic: Tactic[Git.Error]): Stale[Git.Verification] =
          val target = repo.tag(tag)
          Stale:
            target.value match
              case Git.Tag.Target.Annotated(hash, _) =>
                val (_, data) = Operations.readObject(repo.backend, repo.gitDir, hash)
                Objects.tagSignature(data).lay(Git.Verification.Unknown(t"the tag is not signed")): (payload, signature) =>
                  verifier.verify(payload, signature)
              case _ =>
                Git.Verification.Unknown(t"the tag is not annotated")

        // Lends the repository's own configuration for the block, read-only. Global and system
        // configuration are opened on their files: `(Home / ".gitconfig").open[Git.Config](Read)`.
        def config[result](block: ((Git.Config.Handle & Granting[Grant.Read])^) ?=> result): result =
          val source = Git.Config.Source.Repository(repo.gitDir, Git.Config.Scope.Local)
          Operations.config[Grant.Read, result](repo.backend, source)(block)

        // Every scope git would consult for this repository, read-only.
        def effectiveConfig[result](block: ((Git.Config.Handle & Granting[Grant.Read])^) ?=> result)
        :   result =

          Operations.config[Grant.Read, result](repo.backend, Git.Config.Source.Effective(repo.gitDir))(block)

        def note(target: Git.Hash, ref: Git.Ref = Git.Refs.defaultNotes)(using Tactic[Git.Error]): Stale[Optional[Text]] =
          Stale(Operations.readNote(repo.backend, repo.gitDir, target, ref))

        def notes(ref: Git.Ref = Git.Refs.defaultNotes)(using Tactic[Git.Error]): Stale[Map[Git.Hash, Git.Hash]] =
          Stale(Operations.listNotes(repo.backend, repo.gitDir, ref))

        def remotes()(using Tactic[Git.Error]): Stale[List[Remote]] =
          Stale(Operations.listRemotes(repo.backend, repo.gitDir))

        def remote(name: Text)(using Tactic[Git.Error]): Stale[Optional[Remote]] =
          Stale(Operations.listRemotes(repo.backend, repo.gitDir).reap { case remote if remote.name == name => remote })

        def remoteRefs(remote: Text)(using Internet, Tactic[Git.Error]): Stale[Map[Git.Ref, Git.Hash]] =
          Stale(Operations.refMap(repo.backend.remoteRefs(repo.gitDir, remote)))

        def upstream(branch: Git.Branch)(using Tactic[Git.Error]): Stale[Optional[(Text, Git.Branch)]] =
          Stale(Operations.upstream(repo.backend, repo.gitDir, branch))

        def worktrees()(using Tactic[Git.Error]): Stale[List[Git.Worktree.Entry]] =
          Stale:
            repo.backend.worktrees(repo.gitDir).map: entry =>
              Git.Worktree.Entry(entry.path, entry.head, entry.branch, entry.bare, entry.locked, entry.prunable)

        def operation()(using Tactic[Git.Error]): Stale[Optional[Git.Operation]] =
          Stale(repo.backend.operation(repo.gitDir, repo.worktree))

      // Writes. Every operation which moves a ref takes the stale value it is conditional on
      // and fails with `Moved` if the ref no longer holds it: a compare-and-swap, which is the
      // only defence against another process writing the same repository.
      extension (repo: (Git.Repo.Handle & Granting[Grant.Write])^)
        def updateRef
          ( ref:     Git.Ref,
            to:      Optional[Git.Hash],
            from:    Stale[Optional[Git.Hash]],
            message: Optional[Text] = Unset )
          ( using Tactic[Git.Error] )
        :   Unit =

          repo.backend.updateRef(repo.gitDir, ref.encode, to, Git.Backend.Expectation(from.value), message)

        // Unconditional: overwrites whatever the ref holds.
        def forceRef(ref: Git.Ref, to: Optional[Git.Hash], message: Optional[Text] = Unset)
          ( using Tactic[Git.Error] )
        :   Unit =

          repo.backend.updateRef(repo.gitDir, ref.encode, to, Git.Backend.Expectation.Unconditional, message)

        def updateSymbolicRef(name: Text, to: Git.Ref)(using Tactic[Git.Error]): Unit =
          repo.backend.updateSymbolicRef(repo.gitDir, name, to.encode)

        def writeObject(kind: Git.Object.Kind, data: Data)(using Tactic[Git.Error]): Git.Hash =
          repo.backend.writeObject(repo.gitDir, kind, data)

        // Creating a branch is conditional on its absence, so no stale value is taken.
        def makeBranch(branch: Git.Branch, at: Git.Hash)(using Tactic[Git.Error]): Git.Branch =
          repo.backend.updateRef(repo.gitDir, branch.ref.encode, at, Git.Backend.Expectation.Absent, t"branch: Created from ${at.text}")
          branch

        def deleteBranch(branch: Git.Branch, at: Stale[Git.Hash], force: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Unit =

          if !force then
            val head = repo.backend.head(repo.gitDir)
            head.commit.let: tip =>
              if !repo.backend.isAncestor(repo.gitDir, at.value, tip)
              then abort(Git.Error(Reason.Unmerged(branch, at.value)))

          repo.backend.updateRef(repo.gitDir, branch.ref.encode, Unset, Git.Backend.Expectation.Exactly(at.value), Unset)

        def renameBranch(from: Git.Branch, to: Git.Branch, at: Stale[Git.Hash])(using Tactic[Git.Error]): Unit =
          repo.backend.updateRef(repo.gitDir, to.ref.encode, at.value, Git.Backend.Expectation.Absent, t"branch: renamed ${from.ref.encode} to ${to.ref.encode}")
          repo.backend.updateRef(repo.gitDir, from.ref.encode, Unset, Git.Backend.Expectation.Exactly(at.value), Unset)

          repo.backend.head(repo.gitDir) match
            case Git.Head.Attached(`from`, _) => repo.backend.updateSymbolicRef(repo.gitDir, t"HEAD", to.ref.encode)
            case _                            => ()

        // A lightweight tag; creation is conditional on its absence.
        def makeTag(tag: Git.Tag, at: Git.Hash)(using Tactic[Git.Error]): Git.Tag =
          repo.backend.updateRef(repo.gitDir, tag.ref.encode, at, Git.Backend.Expectation.Absent, Unset)
          tag

        def annotatedTag
          ( tag:     Git.Tag,
            at:      Git.Hash,
            message: Text,
            tagger:  Optional[Git.Identity] = Unset,
            signer:  Optional[Git.Signer] = Unset )
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          val kind = Operations.readObject(repo.backend, repo.gitDir, at)(0)
          val identity = tagger.or(Operations.identity(repo.backend, repo.gitDir))
          val payload = Objects.serializeTag(at, kind, tag, Git.Signature(identity, Git.Timestamp.now()), message)
          val signed = signer.lay(payload) { signer => Objects.signTag(payload, signer.sign(payload)) }
          val hash = repo.backend.writeObject(repo.gitDir, Git.Object.Kind.Tag, signed)
          repo.backend.updateRef(repo.gitDir, tag.ref.encode, hash, Git.Backend.Expectation.Absent, Unset)
          hash

        def deleteTag(tag: Git.Tag, at: Stale[Git.Hash])(using Tactic[Git.Error]): Unit =
          // `at` is the tag's own object, annotated or not, as `tags()` and `tag()` report it.
          val current = Operations.resolveRef(repo.backend, repo.gitDir, tag.ref.encode)
          val expected: Optional[Git.Hash] = current.let: current =>
            if repo.backend.peel(repo.gitDir, current) == at.value then current else at.value
          repo.backend.updateRef(repo.gitDir, tag.ref.encode, Unset, Git.Backend.Expectation(expected), Unset)

        // Lends the repository's own configuration for the block, for reading and writing.
        def configure[result]
          ( block: ((Git.Config.Handle & Granting[Grant.Read & Grant.Write])^) ?=> result )
        :   result =

          val source = Git.Config.Source.Repository(repo.gitDir, Git.Config.Scope.Local)
          Operations.config[Grant.Read & Grant.Write, result](repo.backend, source)(block)

        def addNote
          ( target: Git.Hash,
            body:   Text,
            from:   Stale[Optional[Git.Hash]],
            ref:    Git.Ref = Git.Refs.defaultNotes,
            force:  Boolean = false )
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          Operations.writeNote(repo.backend, repo.gitDir, ref, from.value, Git.Backend.NoteEdit.Add(target, body, force))

        def appendNote(target: Git.Hash, body: Text, from: Stale[Optional[Git.Hash]], ref: Git.Ref = Git.Refs.defaultNotes)
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          Operations.writeNote(repo.backend, repo.gitDir, ref, from.value, Git.Backend.NoteEdit.Append(target, body))

        def removeNote(target: Git.Hash, from: Stale[Optional[Git.Hash]], ref: Git.Ref = Git.Refs.defaultNotes, ignoreMissing: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          Operations.writeNote(repo.backend, repo.gitDir, ref, from.value, Git.Backend.NoteEdit.Remove(target, ignoreMissing))

        def copyNote(source: Git.Hash, target: Git.Hash, from: Stale[Optional[Git.Hash]], ref: Git.Ref = Git.Refs.defaultNotes, force: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          Operations.writeNote(repo.backend, repo.gitDir, ref, from.value, Git.Backend.NoteEdit.Copy(source, target, force))

        def addRemote(name: Text, url: Text, fetch: Optional[Git.Refspec] = Unset)(using Tactic[Git.Error]): Remote =
          Operations.addRemote(repo.backend, repo.gitDir, name, url, fetch)

        def removeRemote(name: Text)(using Tactic[Git.Error]): Unit =
          Operations.removeRemote(repo.backend, repo.gitDir, name)

        def renameRemote(from: Text, to: Text)(using Tactic[Git.Error]): Unit =
          Operations.renameRemote(repo.backend, repo.gitDir, from, to)

        def setRemoteUrl(name: Text, url: Text, push: Boolean = false)(using Tactic[Git.Error]): Unit =
          Operations.setRemoteUrl(repo.backend, repo.gitDir, name, url, push)

        def fetch
          ( remote:   Text,
            refspecs: List[Git.Refspec] = Nil,
            depth:    Optional[Int]     = Unset,
            prune:    Boolean           = false,
            tags:     Boolean           = false,
            activity: Activity^ = Activity() )
          ( using Internet, Tactic[Git.Error] )
        :   Stale[Map[Git.Ref, Git.Hash]] =

          Stale(Operations.refMap(repo.backend.fetch(repo.gitDir, remote, refspecs, depth, prune, tags, activity)))

        def push
          ( remote:      Text,
            refspecs:    List[Git.Refspec] = Nil,
            force:       Git.Push.Force    = Git.Push.Force.Never,
            tags:        Boolean           = false,
            setUpstream: Boolean           = false,
            activity:    Activity^ = Activity() )
          ( using Internet, Tactic[Git.Error] )
        :   Stale[Map[Git.Ref, Git.Push.Outcome]] =

          Stale(Operations.refMap(repo.backend.push(repo.gitDir, remote, refspecs, force, tags, setUpstream, activity)))

        def addWorktree
          [ path: Abstractable across Paths to Text ]
          ( target: path, revision: Git.Revision, detach: Boolean = false, newBranch: Optional[Git.Branch] = Unset )
          ( using Tactic[Git.Error] )
        :   Path on Linux =

          val targetPath = Operations.path(target.generic)
          repo.backend.addWorktree(repo.gitDir, targetPath, revision.text, detach, newBranch)
          targetPath

        def removeWorktree(entry: Stale[Git.Worktree.Entry], force: Boolean = false)(using Tactic[Git.Error]): Unit =
          repo.backend.removeWorktree(repo.gitDir, entry.value.path, force)

        def lockWorktree(entry: Stale[Git.Worktree.Entry], reason: Optional[Text] = Unset)(using Tactic[Git.Error]): Unit =
          repo.backend.lockWorktree(repo.gitDir, entry.value.path, reason)

        def unlockWorktree(entry: Stale[Git.Worktree.Entry])(using Tactic[Git.Error]): Unit =
          repo.backend.unlockWorktree(repo.gitDir, entry.value.path)

        def moveWorktree[path: Abstractable across Paths to Text](entry: Stale[Git.Worktree.Entry], to: path)
          ( using Tactic[Git.Error] )
        :   Path on Linux =

          val targetPath = Operations.path(to.generic)
          repo.backend.moveWorktree(repo.gitDir, entry.value.path, targetPath)
          targetPath

        def pruneWorktrees()(using Tactic[Git.Error]): List[Path on Linux] =
          repo.backend.pruneWorktrees(repo.gitDir)

    // Opens a path as a repository: a bare repository, a `.git` directory, a working tree
    // containing one, or a linked worktree's `.git` file. Opening verifies that a repository is
    // there, so a handle always denoted one when it was granted; what happens to it afterwards
    // is another process's business, which is why every read is `Stale`.
    //
    // `Exclusive` arbitrates between handles within this JVM only, through the same register
    // as galilei's directory handles. A `git` process started by anyone else does not know this
    // handle exists and will not wait for it. Cross-process safety comes from compare-and-swap:
    // every ref-moving operation takes the `Stale` value it is conditional on, and fails rather
    // than overwrite a ref that moved.
    class Openable[path <: Path on Linux](using backend: Git.Backend^, tactic: Tactic[Git.Error])
    extends aperture.Openable:
      type Self = path
      type Form = Git.Repo
      type Operand = Git.OpenFlag
      type Result = Handle

      def open[grants <: Grant, result]
        ( value: path, mode: Mode granting grants, flags: List[Git.OpenFlag] )
        ( block: ((Handle & Granting[grants])^) ?=> result )
      :   result =

        val location = backend.locate(value).or(abort(Git.Error(Reason.NoRepository(value.encode))))

        Operations.registered(location.commonDir, mode, flags):
          val handle =
            new Handle with Granting[grants]:
              val gitDir: Path on Linux = location.gitDir
              val commonDir: Path on Linux = location.commonDir
              val worktree: Optional[Path on Linux] = location.worktree
              val atoms: Set[Mode] = mode.atoms
              val backend: Git.Backend^ = Openable.this.backend

          block(using handle)

    given openable: [path <: Path on Linux]
    =>  ( backend: Git.Backend^, tactic: Tactic[Git.Error] )
    =>  ( Openable[path]^{backend, tactic, caps.any} ) =
      Openable[path]()

  trait Repo

  // ──────────────────────────────────────── Worktree handle ───────────────────────────────────

  object Worktree:
    import Git.Error.Reason as Reason

    case class Entry
      ( path:     Path on Linux,
        head:     Optional[Git.Hash],
        branch:   Optional[Git.Branch],
        bare:     Boolean,
        locked:   Optional[Text],
        prunable: Optional[Text] )

    // A checked-out tree: the index and the working files, over the repository they belong to.
    // It is a repository handle, so every repository operation applies without a hop; and it is
    // a directory handle, so paths formed from it (`worktree / "src" / "main.scala"`) are typed
    // to this handle alone, cannot escape it, cannot express `..`, and may be read and written
    // with galilei's subtree operations under the same grants.
    trait Handle extends Git.Repo.Handle, Directory.Handle:
      type Under = Linux
      private[octogenarian] def root: Path on Linux = stem

    object Handle:
      extension (worktree: (Git.Worktree.Handle & Granting[Grant.Read])^)
        def status(ignored: Boolean = false, untracked: Boolean = true)(using Tactic[Git.Error])
        :   Stale[List[Git.PathStatus[worktree.Plane]]] =

          Stale:
            worktree.backend.status(worktree.gitDir, worktree.root, ignored, untracked).map: line =>
              Git.PathStatus
                ( line.index,
                  line.worktree,
                  Operations.planePath(worktree, line.path),
                  line.origin.let(Operations.planePath(worktree, _)) )

        def diff()(using Tactic[Git.Error]): Stale[List[FileDiff]] =
          Stale(worktree.backend.diffWorktree(worktree.gitDir, worktree.root, false, Unset))

        def diffStaged()(using Tactic[Git.Error]): Stale[List[FileDiff]] =
          Stale(worktree.backend.diffWorktree(worktree.gitDir, worktree.root, true, Unset))

        def diffAgainst(revision: Git.Revision)(using Tactic[Git.Error]): Stale[List[FileDiff]] =
          Stale(worktree.backend.diffWorktree(worktree.gitDir, worktree.root, false, revision.text))

        def diffBetween(left: Git.Hash, right: Git.Hash)(using Tactic[Git.Error]): List[FileDiff] =
          worktree.backend.diffTree(worktree.gitDir, left, right)

        def tracked()(using Tactic[Git.Error]): Stale[List[Path on worktree.Plane]] =
          Stale(worktree.backend.tracked(worktree.gitDir, worktree.root).map(Operations.planePath(worktree, _)))

        def conflicts()(using Tactic[Git.Error]): Stale[List[Path on worktree.Plane]] =
          Stale(worktree.backend.conflicts(worktree.gitDir, worktree.root).map(Operations.planePath(worktree, _)))

        def currentBranch()(using Tactic[Git.Error]): Stale[Optional[Git.Branch]] =
          Stale(worktree.backend.head(worktree.gitDir).current)

        def stashes()(using Tactic[Git.Error]): Stale[List[Git.Stash.Entry]] =
          Stale(worktree.backend.stashes(worktree.gitDir))

      extension (worktree: (Git.Worktree.Handle & Granting[Grant.Write])^)
        def add(paths: Path on worktree.Plane*)(using Tactic[Git.Error]): Unit =
          worktree.backend.add(worktree.gitDir, worktree.root, paths.to(List).map(Operations.relative(_)), false)

        def addAll()(using Tactic[Git.Error]): Unit =
          worktree.backend.add(worktree.gitDir, worktree.root, Nil, true)

        def rm(paths: Path on worktree.Plane*)(cached: Boolean = false, force: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Unit =

          worktree.backend.remove(worktree.gitDir, worktree.root, paths.to(List).map(Operations.relative(_)), cached, force)

        def mv(from: Path on worktree.Plane, to: Path on worktree.Plane)(using Tactic[Git.Error]): Unit =
          worktree.backend.move(worktree.gitDir, worktree.root, Operations.relative(from), Operations.relative(to))

        def unstage(paths: Path on worktree.Plane*)(using Tactic[Git.Error]): Unit =
          worktree.backend.restore(worktree.gitDir, worktree.root, paths.to(List).map(Operations.relative(_)), Unset, true, false)

        def restore(paths: Path on worktree.Plane*)(source: Optional[Git.Revision] = Unset, staged: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Unit =

          worktree.backend.restore(worktree.gitDir, worktree.root, paths.to(List).map(Operations.relative(_)), source.let(_.text), staged, true)

        def clean(paths: List[Path on worktree.Plane] = Nil, directories: Boolean = false, ignored: Boolean = false)
          ( using Tactic[Git.Error] )
        :   List[Path on worktree.Plane] =

          worktree.backend.clean(worktree.gitDir, worktree.root, paths.map(Operations.relative(_)), directories, ignored)
          . map(Operations.planePath(worktree, _))

        // A commit is conditional on HEAD being where it was seen: the tree is written, the
        // commit object assembled here and stored, and the ref moved by compare-and-swap. The
        // object is assembled rather than made by `git commit`, so that signing never leaves
        // this process's control and so that the same code serves both backends; hooks are not
        // run.
        def commit
          ( message:    Text,
            head:       Stale[Git.Head],
            author:     Optional[Git.Identity] = Unset,
            signer:     Optional[Git.Signer]   = Unset,
            allowEmpty: Boolean                = false )
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          val parents = head.value.commit.let(List(_)).or(Nil)
          Operations.commit(worktree.backend, worktree.gitDir, worktree.root, message, head.value, parents, author.let(Git.Signature(_, Git.Timestamp.now())), signer, allowEmpty, t"commit")

        def amend
          ( message: Optional[Text] = Unset,
            head:    Stale[Git.Head],
            signer:  Optional[Git.Signer] = Unset )
          ( using Tactic[Git.Error] )
        :   Git.Hash =

          val current = head.value.commit.or(abort(Git.Error(Reason.NoSuchRef(t"HEAD"))))
          val previous = Objects.parseCommit(current, Operations.readObject(worktree.backend, worktree.gitDir, current)(1))
          Operations.commit(worktree.backend, worktree.gitDir, worktree.root, message.or(previous.message), head.value, previous.parents, previous.author, signer, true, t"commit (amend)")

        def reset(mode: ResetMode, to: Git.Revision, head: Stale[Git.Head])(using Tactic[Git.Error]): Git.Hash =
          val target = worktree.backend.resolve(worktree.gitDir, to.text).or(abort(Git.Error(Reason.NoSuchRef(to.text))))
          worktree.backend.updateRef(worktree.gitDir, t"HEAD", target, Git.Backend.Expectation(head.value.commit), t"reset: moving to ${to.text}")
          worktree.backend.readTree(worktree.gitDir, worktree.root, mode)
          target

        def checkout(revision: Git.Revision, head: Stale[Git.Head])(using Tactic[Git.Error]): Stale[Git.Head] =
          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          worktree.backend.checkout(worktree.gitDir, worktree.root, revision.text)
          Stale(worktree.backend.head(worktree.gitDir))

        def switch(branch: Git.Branch, head: Stale[Git.Head], create: Boolean = false)(using Tactic[Git.Error]): Stale[Git.Head] =
          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          worktree.backend.switch(worktree.gitDir, worktree.root, branch, create)
          Stale(worktree.backend.head(worktree.gitDir))

        def merge
          ( revision: Git.Revision,
            head:     Stale[Git.Head],
            ff:       FastForward        = FastForward.Auto,
            message:  Optional[Text]     = Unset,
            sign:     Boolean            = false )
          ( using Tactic[Git.Error] )
        :   Git.Merge.Outcome =

          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          worktree.backend.merge(worktree.gitDir, worktree.root, revision.text, ff, message, sign)

        def cherryPick(commits: List[Git.Hash], head: Stale[Git.Head], noCommit: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Git.Sequence.Outcome =

          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          worktree.backend.cherryPick(worktree.gitDir, worktree.root, commits, noCommit)

        def revert(commits: List[Git.Hash], head: Stale[Git.Head], noCommit: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Git.Sequence.Outcome =

          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          worktree.backend.revert(worktree.gitDir, worktree.root, commits, noCommit)

        def rebase
          ( onto:     Git.Revision,
            head:     Stale[Git.Head],
            upstream: Optional[Git.Revision]   = Unset,
            options:  List[Git.Rebase.Option]  = Nil )
          ( using Tactic[Git.Error] )
        :   Git.Sequence.Outcome =

          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          worktree.backend.rebase(worktree.gitDir, worktree.root, onto.text, upstream.let(_.text), options)

        def pull
          ( remote:   Text,
            head:     Stale[Git.Head],
            branch:   Optional[Git.Branch] = Unset,
            mode:     Git.Pull.Mode        = Git.Pull.Mode.Merge,
            activity: Activity^   = Activity() )
          ( using Internet, Tactic[Git.Error] )
        :   Git.Merge.Outcome =

          Operations.expectHead(worktree.backend, worktree.gitDir, head)
          val current = head.value.current.or(abort(Git.Error(Reason.Detached)))
          val target = branch.or(current)
          val refspec = Git.Refspec(t"refs/heads/${target.text}", Unset, false)
          worktree.backend.fetch(worktree.gitDir, remote, List(refspec), Unset, false, false, activity)

          mode match
            case Git.Pull.Mode.Merge =>
              worktree.backend.merge(worktree.gitDir, worktree.root, t"FETCH_HEAD", FastForward.Auto, Unset, false)
            case Git.Pull.Mode.FastForwardOnly =>
              worktree.backend.merge(worktree.gitDir, worktree.root, t"FETCH_HEAD", FastForward.Only, Unset, false)
            case Git.Pull.Mode.Rebase =>
              worktree.backend.rebase(worktree.gitDir, worktree.root, t"FETCH_HEAD", Unset, Nil) match
                case Git.Sequence.Outcome.Completed(hash)   => Git.Merge.Outcome.Merged(hash)
                case Git.Sequence.Outcome.Stopped(operation) => Git.Merge.Outcome.Stopped(operation)

        def continue()(using Tactic[Git.Error]): Git.Sequence.Outcome =
          worktree.backend.sequence(worktree.gitDir, worktree.root, Git.Sequence.Verb.Continue)

        def skip()(using Tactic[Git.Error]): Git.Sequence.Outcome =
          worktree.backend.sequence(worktree.gitDir, worktree.root, Git.Sequence.Verb.Skip)

        // Abandons the operation in progress, restoring HEAD, the index and the working files to
        // where they were when it began (git's `--abort`).
        def abandon()(using Tactic[Git.Error]): Stale[Git.Head] =
          worktree.backend.sequence(worktree.gitDir, worktree.root, Git.Sequence.Verb.Abort)
          Stale(worktree.backend.head(worktree.gitDir))

        def quit()(using Tactic[Git.Error]): Stale[Git.Head] =
          worktree.backend.sequence(worktree.gitDir, worktree.root, Git.Sequence.Verb.Quit)
          Stale(worktree.backend.head(worktree.gitDir))

        def stash(message: Optional[Text] = Unset, includeUntracked: Boolean = false, keepIndex: Boolean = false)
          ( using Tactic[Git.Error] )
        :   Optional[Git.Hash] =

          worktree.backend.stash(worktree.gitDir, worktree.root, message, includeUntracked, keepIndex)

        def popStash(entry: Stale[Git.Stash.Entry], index: Boolean = false)(using Tactic[Git.Error]): Git.Sequence.Outcome =
          Operations.expectStash(worktree.backend, worktree.gitDir, entry)
          worktree.backend.applyStash(worktree.gitDir, worktree.root, entry.value.index, index, true)

        def applyStash(entry: Stale[Git.Stash.Entry], index: Boolean = false)(using Tactic[Git.Error]): Git.Sequence.Outcome =
          Operations.expectStash(worktree.backend, worktree.gitDir, entry)
          worktree.backend.applyStash(worktree.gitDir, worktree.root, entry.value.index, index, false)

        def dropStash(entry: Stale[Git.Stash.Entry])(using Tactic[Git.Error]): Unit =
          Operations.expectStash(worktree.backend, worktree.gitDir, entry)
          worktree.backend.dropStash(worktree.gitDir, entry.value.index)

    class Openable[path <: Path on Linux](using backend: Git.Backend^, tactic: Tactic[Git.Error])
    extends aperture.Openable:
      type Self = path
      type Form = Git.Worktree
      type Operand = Git.OpenFlag
      type Result = Handle

      def open[grants <: Grant, result]
        ( value: path, mode: Mode granting grants, flags: List[Git.OpenFlag] )
        ( block: ((Handle & Granting[grants])^) ?=> result )
      :   result =

        val location = backend.locate(value).or(abort(Git.Error(Reason.NoRepository(value.encode))))
        val treeRoot = location.worktree.or(abort(Git.Error(Reason.NoWorktree)))

        Operations.registered(location.commonDir, mode, flags):
          Operations.registered(treeRoot, mode, flags):
            val handle =
              new Handle with Granting[grants]:
                val gitDir: Path on Linux = location.gitDir
                val commonDir: Path on Linux = location.commonDir
                val worktree: Optional[Path on Linux] = treeRoot
                val stem: Path on Linux = treeRoot
                val atoms: Set[Mode] = mode.atoms
                val backend: Git.Backend^ = Openable.this.backend

            block(using handle)

    given openable: [path <: Path on Linux]
    =>  ( backend: Git.Backend^, tactic: Tactic[Git.Error] )
    =>  ( Openable[path]^{backend, tactic, caps.any} ) =
      Openable[path]()

  trait Worktree

  // ───────────────────────────────────────────── Creation ─────────────────────────────────────

  def init[path: Abstractable across Paths to Text]
    ( target: path, initialBranch: Optional[Git.Branch] = Unset, bare: Boolean = false )
    ( using backend: Git.Backend^, tactic: Tactic[Git.Error] )
  :   Unit =

    backend.init(Operations.path(target.generic), initialBranch, bare)

  inline def clone[source <: Matchable, path: Abstractable across Paths to Text]
    ( source:    source,
      target:    path,
      branch:    Optional[Git.Branch] = Unset,
      depth:     Optional[Int]        = Unset,
      bare:      Boolean              = false,
      recursive: Boolean              = false,
      activity:  Activity^            = Activity() )
    ( using Internet, Git.Backend^, Tactic[Git.Error] )
  :   Unit =

    val sourceText: Text = inline source match
      case source: SshUrl => source.text

      case other =>
        scala.compiletime.summonFrom:
          case given (`source` is Abstractable across Urls to Text)  => source.generic
          case given (`source` is Abstractable across Paths to Text) => source.generic

    Operations.clone(sourceText, Operations.path(target.generic), branch, depth, bare, recursive, activity)

  // Clones exactly one commit: a shallow fetch of it into a fresh repository, checked out.
  inline def cloneCommit[source <: Matchable, path: Abstractable across Paths to Text]
    ( source: source, target: path, commit: Git.Hash, activity: Activity^ = Activity() )
    ( using Internet, Git.Backend^, Tactic[Git.Error] )
  :   Unit =

    val sourceText: Text = inline source match
      case source: SshUrl => source.text

      case other =>
        scala.compiletime.summonFrom:
          case given (`source` is Abstractable across Urls to Text)  => source.generic
          case given (`source` is Abstractable across Paths to Text) => source.generic

    Operations.cloneCommit(sourceText, Operations.path(target.generic), commit, activity)
