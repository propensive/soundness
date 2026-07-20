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
package revolution

import soundness.*

object Tests extends Suite(m"Revolution Tests"):
  def run(): Unit =
    suite(m"Semantic version parsing tests"):
      val valid =
        List
          ( t"0.0.4",
            t"1.2.3",
            t"10.20.30",
            t"1.1.2-prerelease+meta",
            t"1.1.2+meta",
            t"1.1.2+meta-valid",
            t"1.0.0-alpha",
            t"1.0.0-beta",
            t"1.0.0-alpha.beta",
            t"1.0.0-alpha.beta.1",
            t"1.0.0-alpha.1",
            t"1.0.0-alpha0.valid",
            t"1.0.0-alpha.0valid",
            t"1.0.0-alpha-a.b-c-somethinglong+build.1-aef.1-its-okay",
            t"1.0.0-rc.1+build.1",
            t"2.0.0-rc.1+build.123",
            t"1.2.3-beta",
            t"10.2.3-DEV-SNAPSHOT",
            t"1.2.3-SNAPSHOT-123",
            t"1.0.0",
            t"2.0.0",
            t"1.1.7",
            t"2.0.0+build.1848",
            t"2.0.1-alpha.1227",
            t"1.0.0-alpha+beta",
            t"1.2.3----RC-SNAPSHOT.12.9.1--.12+788",
            t"1.2.3----R-S.12.9.1--.12+meta",
            t"1.2.3----RC-SNAPSHOT.12.9.1--.12",
            t"1.0.0+0.build.1-rc.10000aaa-kk-0.1",
            t"999999999999999999.999999999999999999.999999999999999999",
            t"1.0.0-0A.is.legal" )

      for version <- valid do
        test(m"Roundtrip $version"):
          import strategies.throwUnsafely
          version.as[Semver].encode
        . assert(_ == version)

      val invalid =
        List
          ( t"1",
            t"1.2",
            t"1.2.3-0123",
            t"1.2.3-0123.0123",
            t"1.1.2+.123",
            t"+invalid",
            t"-invalid",
            t"-invalid+invalid",
            t"-invalid.01",
            t"alpha",
            t"alpha.beta",
            t"alpha.beta.1",
            t"alpha.1",
            t"alpha+beta",
            t"alpha_beta",
            t"alpha.",
            t"alpha..",
            t"beta",
            t"1.0.0-alpha_beta",
            t"-alpha.",
            t"1.0.0-alpha..",
            t"1.0.0-alpha..1",
            t"1.0.0-alpha...1",
            t"1.0.0-alpha....1",
            t"1.0.0-alpha.....1",
            t"1.0.0-alpha......1",
            t"1.0.0-alpha.......1",
            t"01.1.1",
            t"1.01.1",
            t"1.1.01",
            t"1.2",
            t"1.2.3.DEV",
            t"1.2-SNAPSHOT",
            t"1.2.31.2.3----RC-SNAPSHOT.12.09.1--..12+788",
            t"1.2-RC-SNAPSHOT",
            t"-1.0.3-gamma+b7718",
            t"+justmeta",
            t"9.8.7+meta+meta",
            t"9.8.7-whatever+meta+meta",
            t"99999999999999999999999.999999999999999999.99999999999999999----RC-SNAPSHOT.12.09.1--------------------------------..12" )

      for version <- invalid do
        test(m"Parser rejects ${version.keep(20)}"):
          safely(version.as[Semver])
        . assert(_.absent)

    suite(m"Semantic version precedence"):
      test(m"Check that 1.0.0-alpha < 1.0.0-alpha.1"):
        v"1.0.0-alpha" < v"1.0.0-alpha.1"
      . assert(identity(_))

      test(m"Check that 1.0.0-alpha.1 < 1.0.0-alpha.beta"):
        v"1.0.0-alpha.1" < v"1.0.0-alpha.beta"
      . assert(identity(_))

      test(m"Check that 1.0.0-alpha.beta < 1.0.0-beta"):
        v"1.0.0-alpha.beta" < v"1.0.0-beta"
      . assert(identity(_))

      test(m"Check that 1.0.0-beta < 1.0.0-beta.2"):
        v"1.0.0-beta" < v"1.0.0-beta.2"
      . assert(identity(_))

      test(m"Check that 1.0.0-beta.2 < 1.0.0-beta.11"):
        v"1.0.0-beta.2" < v"1.0.0-beta.11"
      . assert(identity(_))

      test(m"Check that 1.0.0-beta.11 < 1.0.0-rc.1"):
        v"1.0.0-beta.11" < v"1.0.0-rc.1"
      . assert(identity(_))

      test(m"Check that 1.0.0-rc.1 < 1.0.0"):
        v"1.0.0-rc.1" < v"1.0.0"
      . assert(identity(_))

      test(m"Equal versions are not less than each other"):
        v"1.0.0" < v"1.0.0"
      . assert(_ == false)

      test(m"Equal prerelease versions are not less than each other"):
        v"1.0.0-alpha" < v"1.0.0-alpha"
      . assert(_ == false)

      test(m"Release is not less than its prerelease"):
        v"1.0.0" < v"1.0.0-alpha"
      . assert(_ == false)
