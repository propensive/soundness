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

import java.nio.file as jnf
import java.security as js
import java.util as ju

import scala.quoted.*

import anticipation.*
import gossamer.*

// The compile-time half of the Burdock redesign. `dependencyHashes` captures the
// build's classpath, computes each dependency JAR's SHA-256, hard-links it into
// the Burdock cache (`~/.cache/burdock/<sha256>.jar`) so its bytes stay
// retrievable by hash locally, and embeds the SHA-256 list as compiled-in
// literals. The published-vs-unpublished decision is deferred to repackage (where
// deps.dev is queried); the cache covers anything deps.dev cannot resolve.
object Embed:
  inline def dependencyHashes: List[Text] = ${Embed.dependencyHashesMacro}

  def dependencyHashesMacro(using Quotes): Expr[List[Text]] =
    val classpath: String = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        import dotty.tools.dotc.config.Settings.Setting.value
        value(quotes.ctx.settings.classpath)(using quotes.ctx)

    val hashes: List[String] = hashAndCache(classpath)

    '{ ${Expr(hashes)}.map(_.tt) }

  // Runs at compile time, in the compiler JVM: pure java.* so it needs nothing
  // from the soundness runtime. Hard-links fall back to a copy across filesystems.
  private def hashAndCache(classpath: String): List[String] =
    val home: String = System.getProperty("user.home").nn
    val cacheDir: jnf.Path = jnf.Paths.get(home, ".cache", "burdock").nn
    jnf.Files.createDirectories(cacheDir)

    val entries: Array[String | Null] = classpath.split(java.io.File.pathSeparator).nn
    val hashes = List.newBuilder[String]
    var i = 0

    while i < entries.length do
      val entry: String = entries(i).nn
      i += 1
      val path: jnf.Path = jnf.Paths.get(entry).nn

      if entry.endsWith(".jar") && jnf.Files.isRegularFile(path) then
        val bytes: Array[Byte] = jnf.Files.readAllBytes(path).nn
        val digest: Array[Byte] = js.MessageDigest.getInstance("SHA-256").nn.digest(bytes).nn
        val hex: String = ju.HexFormat.of().nn.formatHex(digest).nn
        val target: jnf.Path = cacheDir.resolve(hex+".jar").nn

        if !jnf.Files.exists(target) then
          try jnf.Files.createLink(target, path)
          catch case _: Throwable => jnf.Files.copy(path, target)

        hashes += hex

    hashes.result()
