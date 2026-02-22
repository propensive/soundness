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
package revolution

import scala.collection as sc

import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import fulminate.*
import gossamer.*
import kaleidoscope.*
import prepositional.*
import proscenium.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.stackTraces

object Semver:
  given encodable: Semver is Encodable in Text =
    semver =>
      extension (element: Long | Text) def text: Text = element.absolve match
        case text: Text => text
        case long: Long => long.show

      val prerelease = if semver.prerelease.nil then t""
                       else t"-"+semver.prerelease.map(_.text).join(t".")

      val build = if semver.build.nil then t"" else t"+"+semver.build.map(_.text).join(t".")

      t"${semver.major}.${semver.minor}.${semver.patch}$prerelease$build"

  given showable: Semver is Showable = encodable.encoded(_)

  given decodable: Tactic[SemverError] => Semver is Decodable in Text =
    text =>
      text match
        case r"$major([0-9]+)\.$minor([0-9]+)\.$patch([0-9]+)$prerelease(-[^\+]+)?$build(\+.+)?" =>
          val prerelease2: List[Text] = prerelease.let(_.skip(1).cut(t".")).or(Nil)
          val build2: List[Text] = build.let(_.skip(1).cut(t".")).or(Nil)

          if prerelease == t"-" || build == t"+" then raise(SemverError(text))

          for extra   <- List(prerelease2, build2).compact
              element <- extra
          do element match
            case r"0[0-9]+"       => raise(SemverError(text))
            case r"[0-9A-Za-z-]+" => ()
            case _                => raise(SemverError(text))

          val prerelease3: List[Text | Long] = prerelease2.map: element =>
            safely(element.decode[Long]).or(element)

          val build3: List[Text | Long] = build2.map: element =>
            safely(element.decode[Long]).or(element)

          mitigate:
            case NumberError(_, _) => SemverError(text)

          . within:
              val major2 = major.decode[Long]
              if major.starts(t"0") && major2 != 0 then raise(SemverError(text))
              val minor2 = minor.decode[Long]
              if minor.starts(t"0") && minor2 != 0 then raise(SemverError(text))
              val patch2 = patch.decode[Long]
              if patch.starts(t"0") && patch2 != 0 then raise(SemverError(text))
              Semver(major2, minor2, patch2, prerelease3, build3)

        case _ =>
          abort(SemverError(text))



  given ordering: Ordering[Semver] = Ordering.fromLessThan: (left, right) =>
    def compare(left: List[Long | Text], right: List[Long | Text]): Boolean =
      (left, right).absolve match
        case (Nil, Nil)                       => false
        case (Nil, _)                         => true
        case (_, Nil)                         => false
        case (left :: lefts, right :: rights) => left.absolve match
          case left: Text => right.absolve match
            case right: Long => false
            case right: Text => if left == right then compare(lefts, rights)
                                else left.s.compareTo(right.s) < 0
          case left: Long => right.absolve match
            case right: Text => true
            case right: Long => if left == right then compare(lefts, rights) else left < right

    if left.major == right.major then
      if left.minor == right.minor then
        if left.patch == right.patch then
          right.prerelease.nil || compare(left.prerelease, right.prerelease)
        else left.patch < right.patch
      else left.minor < right.minor
    else left.major < right.major


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
      major         == that.major
      && minor      == that.minor
      && patch      == that.patch
      && prerelease == that.prerelease

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
