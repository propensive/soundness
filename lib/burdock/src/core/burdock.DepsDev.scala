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
package burdock

import anticipation.*
import contingency.*
import distillate.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import jacinta.*
import monotonous.*, alphabets.base64Standard, alphabets.hexLowerCase
import telekinesis.*
import urticose.*
import vacuous.*

import internetAccess.online

// Resolves a dependency's content hash to its public download URL using Google's
// deps.dev ("Open Source Insights") Query API. Returns `Unset` for anything
// deps.dev does not know as a Maven artifact — the caller then inlines the
// dependency's classes from the local Burdock cache instead.
object DepsDev:
  case class VersionKey(system: Text, name: Text, version: Text)
  case class Version(versionKey: VersionKey)
  case class Result(version: Version)
  case class QueryResult(results: List[Result])

  case class Unresolved()(using Diagnostics) extends Error(m"not a known Maven artifact")

  def mavenUrl(sha256Hex: Text): Optional[HttpUrl] = safely:
    // deps.dev expects the hash base64-encoded (not hex).
    val base64: Text = sha256Hex.deserialize[Hex].serialize[Base64]
    val query: HttpUrl = url"https://api.deps.dev/v3/query?hash.type=SHA256&hash.value=$base64"
    val result: QueryResult = mute[HttpEvent](query.fetch().receive[Json]).as[QueryResult]

    val key: VersionKey =
      result.results.map(_.version.versionKey).find(_.system == t"MAVEN")
      . getOrElse(abort(Unresolved()))

    // `name` is `group:artifact`; the Maven Central path uses `/` for the group.
    val parts: List[Text] = key.name.cut(t":")
    val group: Text = parts.head.cut(t".").join(t"/")
    val artifact: Text = parts.last
    val version: Text = key.version
    val jar: Text = t"$artifact-$version.jar"

    t"https://repo1.maven.org/maven2/$group/$artifact/$version/$jar".decode[HttpUrl]
