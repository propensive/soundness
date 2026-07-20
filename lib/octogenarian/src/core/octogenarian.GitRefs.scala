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
┃    Soundness, version 0.63.0.                                                                    ┃
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
import gossamer.*
import prepositional.*
import serpentine.*

// `GitRefs` is the Serpentine path scheme for Git references. Every fully-
// qualified reference (`refs/heads/main`, `refs/notes/commits`, …) is rooted
// at `refs/` and is slash-separated, so it maps cleanly onto a Serpentine
// path. Component-level validation reproduces the rules `git check-ref-format`
// enforces, expressed as an `Admissible` typeclass.
object GitRefs extends Root(t"refs/"):
  type Plane = GitRefs

  // Construct a notes-namespace ref path of the form `refs/notes/<namespace>`.
  // The namespace is validated against `git check-ref-format`'s per-segment
  // rules; an invalid namespace raises `GitRefError`.
  def notes(namespace: Text)(using Tactic[GitRefError]): Path on GitRefs =
    validateSegment(namespace)
    GitRefs / t"notes" / namespace

  // Construct a branch ref path of the form `refs/heads/<branch>`.
  def heads(branch: Text)(using Tactic[GitRefError]): Path on GitRefs =
    validateSegment(branch)
    GitRefs / t"heads" / branch

  // Construct a tag ref path of the form `refs/tags/<tag>`.
  def tags(tag: Text)(using Tactic[GitRefError]): Path on GitRefs =
    validateSegment(tag)
    GitRefs / t"tags" / tag

  // The default notes namespace used by `git notes` when no `--ref` is given.
  val defaultNotes: Path on GitRefs = unsafely(GitRefs.notes(t"commits"))

  // Serpentine's `/` operator does not invoke an `Admissible`'s `check` at
  // construction time, so the per-segment rules live here and are invoked
  // explicitly from the typed constructors above.
  def validateSegment(segment: Text)(using Tactic[GitRefError]): Unit =
    def fail(reason: GitRefError.Reason) = abort(GitRefError(segment, reason))
    if segment.length == 0     then fail(GitRefError.Reason.EmptySegment)
    if segment.starts(t".")    then fail(GitRefError.Reason.LeadingOrTrailingDot)
    if segment.ends(t".")      then fail(GitRefError.Reason.LeadingOrTrailingDot)
    if segment.ends(t".lock")  then fail(GitRefError.Reason.ReservedSuffix)
    if segment.contains(t"@{") then fail(GitRefError.Reason.ReservedSequence)
    if segment.contains(t"..") then fail(GitRefError.Reason.DoubleDot)

    for ch <- List('*', '[', '\\', ' ', '^', '~', ':', '?', '/').stdlib
    do if segment.contains(ch) then fail(GitRefError.Reason.InvalidCharacter)

  // The default `text is Admissible on filesystem` from Serpentine already
  // satisfies the typeclass slot for any plane; the marker here is for the
  // benefit of `summonFrom` in Path's `/` operator, which treats the absence
  // of an `Admissible` as evidence that the result should be unplatformed.
  given admissible: [text <: Text] => text is Admissible on GitRefs = _ => ()

  given filesystem: GitRefs is Filesystem:
    val name: Text = t"GitRefs"
    val separator: Text = t"/"
    val self: Text = t"@"
    val parent: Text = t".."

  // A validated `Path on GitRefs` already satisfies every rule git enforces,
  // so it is safe to expose it as an opaque `Refspec` for any operation that
  // accepts one.
  given pathIsRefspec: Conversion[Path on GitRefs, Refspec] =
    path => Refspec.unsafe(path.encode)

trait GitRefs
