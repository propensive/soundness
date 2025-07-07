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
┃    Soundness, version 0.32.0.                                                                    ┃
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
import distillate.*
import fulminate.*
import gossamer.*
import hypotenuse.*
import kaleidoscope.*
import prepositional.*
import spectacular.*
import symbolism.*
import vacuous.*

import errorDiagnostics.stackTraces

object Semver:
  given encodable: Semver is Encodable in Text =
    semver =>
      extension (element: Long | Text) def text: Text = element match
        case text: Text => text
        case long: Long => long.show

      val suffix = if semver.suffix.isEmpty then t"" else t"-"+semver.suffix.map(_.text).join(t".")
      val build = if semver.build.isEmpty then t"" else t"+"+semver.build.map(_.text).join(t".")

      t"${semver.major}.${semver.minor}.${semver.patch}$suffix$build"

  given showable: Semver is Showable = encodable.encoded(_)

  given decodable: Tactic[SemverError] => Semver is Decodable in Text =
    text =>
      text match
        case r"$major([0-9]+)\.$minor([0-9]+)\.$patch([0-9]+)$suffix(-[^\+]+)?$build(\+.+)?" =>
          val suffix2: List[Text] = suffix.let(_.skip(1).cut(t".")).or(Nil)
          val build2: List[Text] = build.let(_.skip(1).cut(t".")).or(Nil)

          if suffix == t"-" || build == t"+" then raise(SemverError(text))

          for extra   <- List(suffix2, build2).compact
              element <- extra
          do element match
            case r"0[0-9]+"       => raise(SemverError(text))
            case r"[0-9A-Za-z-]+" => ()
            case _                => raise(SemverError(text))

          val suffix3: List[Text | Long] = suffix2.map: element =>
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
              Semver(major2, minor2, patch2, suffix3, build3)



  given ordering: Ordering[Semver] = Ordering.fromLessThan: (left, right) =>
    def compare(left: List[Long | Text], right: List[Long | Text]): Boolean = (left, right) match
      case (Nil, Nil)                             => false
      case (Nil, _)                               => true
      case (_, Nil)                               => false
      case ((left: Text) :: _, (right: Long) :: _) => false
      case ((left: Long) :: _, (right: Text) :: _) => true

      case ((left: Long) :: lefts, (right: Long) :: rights) =>
        if left == right then compare(lefts, rights) else left < right

      case ((left: Text) :: lefts, (right: Text) :: rights) =>
        if left == right then compare(lefts, rights) else left.s.compareTo(right.s) < 0

    if left.major == right.major then
      if left.minor == right.minor then
        if left.patch == right.patch then
          right.suffix.isEmpty || compare(left.suffix, right.suffix)
        else left.patch < right.patch
      else left.minor < right.minor
    else left.major < right.major


case class Semver
            (major:  Long,
             minor:  Long,
             patch:  Long,
             suffix: List[Long | Text],
             build:  List[Long | Text]):

  def development: Boolean = major == 0

  override def equals(that: Any): Boolean = that match
    case that: Semver =>
      major == that.major && minor == that.minor && patch == that.patch && suffix == that.suffix

    case _ =>
      false

  override def hashCode: Int = (major, minor, patch, suffix).hashCode
