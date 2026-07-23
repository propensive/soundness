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
package burdock

import java.nio.file as jnf
import java.security as js
import java.util as ju

import scala.quoted.*

// The compile-time half of Burdock. The `externalize` macro captures the build's
// classpath, computes each dependency JAR's SHA-256, hard-links it into the Burdock
// cache (`~/.cache/burdock/<sha256>.jar`) so its bytes stay retrievable by hash
// locally, and embeds the SHA-256 list as the `META-INF/burdock.deps` resource. The
// published-vs-unpublished decision is deferred to repackage (where deps.dev is
// queried); the cache covers anything deps.dev cannot resolve.
object internal:
  // Resource path (within the compiled JAR) holding the newline-separated dependency
  // SHA-256 hashes; read by the repackager and, in turn, the runtime bootstrap.
  final val ResourcePath: String = "META-INF/burdock.deps"

  // Performs the compile-time side effects (hash + cache + embed the resource) and
  // returns `block` unchanged, so `externalize` runs the application's body at runtime.
  def externalize[result: Type](block: Expr[result])(using Quotes): Expr[result] =
    val (classpath, outputDir) = quotes.absolve match
      case quotes: runtime.impl.QuotesImpl =>
        import dotty.tools.dotc.config.Settings.Setting.value
        val ctx = quotes.ctx
        val classpath0: String = value(ctx.settings.classpath)(using ctx)
        val outputDir0: jnf.Path = value(ctx.settings.outputDir)(using ctx).jpath.nn
        (classpath0, outputDir0)

    val hashes: List[String] = hashAndCache(classpath)
    writeResource(outputDir, hashes)

    block

  // Writes the hash list into the compile output as `META-INF/burdock.deps`, so it is
  // packaged into the JAR as an ordinary resource.
  private def writeResource(outputDir: jnf.Path, hashes: List[String]): Unit =
    val metaInf: jnf.Path = outputDir.resolve("META-INF").nn
    jnf.Files.createDirectories(metaInf)
    val content: String = hashes.stdlib.mkString("\n")
    jnf.Files.write(metaInf.resolve("burdock.deps").nn, content.getBytes("UTF-8").nn)

  // Runs at compile time, in the compiler JVM: pure java.* so it needs nothing from the
  // soundness runtime. Hard-links fall back to a copy across filesystems.
  private def hashAndCache(classpath: String): List[String] =
    val home: String = System.getProperty("user.home").nn
    val cacheDir: jnf.Path = jnf.Paths.get(home, ".cache", "burdock").nn
    jnf.Files.createDirectories(cacheDir)

    val entries: Array[String | Null] = classpath.split(java.io.File.pathSeparator).nn
    val hashes = scala.collection.immutable.List.newBuilder[String]
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

    List.of(hashes.result())
