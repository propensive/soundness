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
package revolution

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import spectacular.*
import symbolism.*
import proscenium.compat.*

import rudiments.*
import vacuous.*

import errorDiagnostics.stackTracesDiagnostics

object Semver:
  given encodable: Semver is Encodable in Text =
    semver =>
      extension (element: Long | Text) def text: Text = element.absolve match
        case text: Text => text
        case long: Long => long.show

      val prerelease =
        if semver.prerelease.nil then t"" else t"-"+semver.prerelease.map(_.text).join(t".")

      val build = if semver.build.nil then t"" else t"+"+semver.build.map(_.text).join(t".")

      t"${semver.major}.${semver.minor}.${semver.patch}$prerelease$build"

  given showable: Semver is Showable = encodable.encoded(_)

  given decodable: (tactic: Tactic[Semver.Error]) => ((Semver is Decodable in Text)^{tactic}) =
    text =>
      text match
        case r"$major([0-9]+)\.$minor([0-9]+)\.$patch([0-9]+)$prerelease(-[^\+]+)?$build(\+.+)?" =>
          val prerelease2: List[Text] = prerelease.let(_.skip(1).cut(t".")).or(Nil)
          val build2: List[Text] = build.let(_.skip(1).cut(t".")).or(Nil)

          if prerelease == t"-" || build == t"+" then
            raise(Semver.Error(text, Semver.Error.Reason.EmptyIdentifier))

          for
            extra   <- List(prerelease2, build2).stdlib.compact
            element <- extra
          do element match
            case r"0[0-9]+"       => raise(Semver.Error(text, Semver.Error.Reason.LeadingZero))
            case r"[0-9A-Za-z-]+" => ()
            case _                => raise(Semver.Error(text, Semver.Error.Reason.InvalidCharacter))

          val prerelease3: List[Text | Long] = prerelease2.map: element =>
            safely(element.as[Long]).or(element)

          val build3: List[Text | Long] = build2.map: element =>
            safely(element.as[Long]).or(element)

          mitigate:
            case NumberError(_, _, _) => Semver.Error(text, Semver.Error.Reason.BadFormat)

          . protect:
              val major2 = major.as[Long]

              if major.starts(t"0") && major2 != 0
              then raise(Semver.Error(text, Semver.Error.Reason.LeadingZero))

              val minor2 = minor.as[Long]

              if minor.starts(t"0") && minor2 != 0
              then raise(Semver.Error(text, Semver.Error.Reason.LeadingZero))

              val patch2 = patch.as[Long]

              if patch.starts(t"0") && patch2 != 0
              then raise(Semver.Error(text, Semver.Error.Reason.LeadingZero))

              Semver(major2, minor2, patch2, prerelease3, build3)

        case _ =>
          abort(Semver.Error(text, Semver.Error.Reason.BadFormat))

  given ordering: Ordering[Semver] = Ordering.fromLessThan: (left, right) =>
    def compare(left0: List[Long | Text], right0: List[Long | Text]): Boolean =
      val left = left0.stdlib
      val right = right0.stdlib
      if left.isEmpty && right.isEmpty then false
      else if left.isEmpty then true
      else if right.isEmpty then false
      else
        val lh = left.head
        val rh = right.head
        val lts = List.of(left.tail)
        val rts = List.of(right.tail)
        lh.absolve match
          case lh: Text => rh.absolve match
            case rh: Long => false
            case rh: Text => if lh == rh then compare(lts, rts) else lh.s.compareTo(rh.s) < 0

          case lh: Long => rh.absolve match
            case rh: Text => true
            case rh: Long => if lh == rh then compare(lts, rts) else lh < rh

    if left.major == right.major then
      if left.minor == right.minor then
        if left.patch == right.patch then
          if left.prerelease.nil then false
          else if right.prerelease.nil then true
          else compare(left.prerelease, right.prerelease)
        else
          left.patch < right.patch
      else
        left.minor < right.minor
    else
      left.major < right.major

  // SemverError → Semver.Error
  object Error:
    enum Reason(val number: Int) extends Clarification:
      case BadFormat        extends Reason(1)
      case EmptyIdentifier  extends Reason(2)
      case LeadingZero      extends Reason(3)
      case InvalidCharacter extends Reason(4)

    given communicable: Reason is Communicable =
      case Reason.BadFormat        => m"the string did not match the SemVer 2.0 grammar"
      case Reason.EmptyIdentifier  => m"a prerelease or build identifier was empty"
      case Reason.LeadingZero      => m"a numeric identifier had a leading zero"
      case Reason.InvalidCharacter => m"an identifier contained a character outside `[0-9A-Za-z-]`"

  case class Error(version: Text, reason: Semver.Error.Reason)(using Diagnostics)
  extends fulminate.Error(250, reason.number)
    ( m"$version is not a valid semantic version because $reason" )

case class Semver
  ( major:      Long,
    minor:      Long,
    patch:      Long,
    prerelease: List[Long | Text] = Nil,
    build:      List[Long | Text] = Nil ):

  def development: Boolean = major == 0
  def release: Semver = Semver(major, minor, patch, Nil, Nil)

  override def equals(that: Any): Boolean = that match
    case that: Semver =>
      major         == that.major &&
        minor      == that.minor &&
        patch      == that.patch &&
        prerelease == that.prerelease

    case _ =>
      false

  override def hashCode: Int = (major, minor, patch, prerelease).hashCode

  def compatibility(right: Semver): Compatibility =
    if !prerelease.nil || !right.prerelease.nil then Compatibility.Breaking
    else if major == 0 || right.major == 0 || major != right.major then Compatibility.Breaking
    else if minor < right.minor then Compatibility.Additions
    else if minor > right.minor then Compatibility.Breaking
    else if patch != right.patch then Compatibility.Internal
    else Compatibility.Unchanged

  def next(api: Compatibility): Semver =
    import Compatibility.*

    api match
      case Breaking  => if major == 0 then Semver(0, minor + 1, 0) else Semver(major + 1, 0, 0)
      case Additions => Semver(major, minor + 1, 0)
      case Internal  => Semver(major, minor, patch + 1)
      case Unchanged => Semver(major, minor, patch + 1)
