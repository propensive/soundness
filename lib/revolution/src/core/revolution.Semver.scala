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
import kaleidoscope.*
import prepositional.*
import symbolism.*
import vacuous.*

import errorDiagnostics.stackTraces

object Semver:
  given encodable: Semver is Encodable in Text =
    semver =>
      val suffix = semver.suffix.lay(t"")(t"-"+_.join(t"."))
      val build = semver.build.lay(t"")(t"+"+_.join(t"."))
      t"${semver.major}.${semver.minor}.${semver.patch}$suffix$build"

  given decodable: Tactic[SemverError] => Semver is Decodable in Text =
    text =>
      text match
        case r"$major([0-9]+)\.$minor([0-9]+)\.$patch([0-9]+)$suffix(-[^\+]+)?$build(\+.+)?" =>
          val suffix2 = suffix.let(_.skip(1).cut(t"."))
          val build2 = build.let(_.skip(1).cut(t"."))

          for extra <- List(suffix2, build2).compact do
            if extra.isEmpty then raise(SemverError(text))
            for element <- extra do element match
              case r"0[0-9]+"       => raise(SemverError(element))
              case r"[0-9A-Za-z-]+" => ()
              case _                => raise(SemverError(element))

          mitigate:
            case NumberError(_, _) => SemverError(text)

          . within:
              val major2 = major.decode[Long]
              if major.starts(t"0") && major2 != 0 then raise(SemverError(text))
              val minor2 = minor.decode[Long]
              if minor.starts(t"0") && minor2 != 0 then raise(SemverError(text))
              val patch2 = patch.decode[Long]
              if patch.starts(t"0") && patch2 != 0 then raise(SemverError(text))
              Semver(major2, minor2, patch2, suffix2, build2)

case class Semver
            (major:  Long,
             minor:  Long,
             patch:  Long,
             suffix: Optional[List[Text]],
             build:  Optional[List[Text]]):

  def development: Boolean = major == 0
