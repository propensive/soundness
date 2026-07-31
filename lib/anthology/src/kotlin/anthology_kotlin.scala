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
package anthology

import anticipation.*
import gossamer.*
import spectacular.*

object kotlincOptions:
  val javaParameters = Kotlinc.Option[Kotlinc.Versions](t"-java-parameters")
  val noJdk = Kotlinc.Option[Kotlinc.Versions](t"-no-jdk")
  val progressive = Kotlinc.Option[Kotlinc.Versions](t"-progressive")
  val verbose = Kotlinc.Option[Kotlinc.Versions](t"-verbose")

  def moduleName(name: Text) = Kotlinc.Option[Kotlinc.Versions](t"-module-name", name)
  def jvmTarget(version: Int) = Kotlinc.Option[Kotlinc.Versions](t"-jvm-target", version.show)
  def optIn(marker: Text) = Kotlinc.Option[Kotlinc.Versions](t"-opt-in", marker)

  object warnings:
    val none = Kotlinc.Option[Kotlinc.Versions](t"-nowarn")
    val asErrors = Kotlinc.Option[Kotlinc.Versions](t"-Werror")
    val extra = Kotlinc.Option[2.0 | 2.1 | 2.2 | 2.3 | 2.4](t"-Wextra")

  object compatibility:
    def api(version: Kotlinc.Versions) =
      Kotlinc.Option[Kotlinc.Versions](t"-api-version", version.toString.tt)

    def language(version: Kotlinc.Versions) =
      Kotlinc.Option[Kotlinc.Versions](t"-language-version", version.toString.tt)

    def jdkRelease(version: Int) =
      Kotlinc.Option[Kotlinc.Versions](t"-Xjdk-release=${version.show}")

  object explicitApi:
    val strict = Kotlinc.Option[Kotlinc.Versions](t"-Xexplicit-api=strict")
    val warning = Kotlinc.Option[Kotlinc.Versions](t"-Xexplicit-api=warning")

  object advanced:
    val noCallAssertions = Kotlinc.Option[Kotlinc.Versions](t"-Xno-call-assertions")
    val noParameterAssertions = Kotlinc.Option[Kotlinc.Versions](t"-Xno-param-assertions")
