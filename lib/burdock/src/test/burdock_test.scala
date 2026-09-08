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

import java.util.concurrent as juc
import juc.atomic as juca

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.javaBaseWorkingDirectory
import logging.silentLogging
import threading.platformThreading
import probates.awaitProbate

object Tests extends Suite(m"Burdock Tests"):
  def run(): Unit =
    suite(m"Externalizing macro"):
      val home: Text = _root_.java.lang.System.getProperty("user.home").nn.tt

      // Invoking `externalize` embeds `META-INF/burdock.deps` into this module's compiled
      // output and hard-links each dependency JAR into the Burdock cache.
      externalize(())

      val hashes: List[Text] =
        val stream = getClass.nn.getResourceAsStream("/META-INF/burdock.deps").nn
        val content: Text = _root_.java.lang.String(stream.readAllBytes().nn, "UTF-8").tt
        content.cut(t"\n").filter(_ != t"")

      test(m"embeds a non-empty set of dependency hashes as a resource"):
        hashes.stdlib.length
      .assert(_ > 0)

      test(m"every hash is 64-character SHA-256 hex"):
        hashes.all { hash => hash.length == 64 && hash.lower == hash }
      .assert(_ == true)

      test(m"every hash is hard-linked into the Burdock cache"):
        hashes.all: hash =>
          val jar: Text = t"$home/.cache/burdock/$hash.jar"
          _root_.java.nio.file.Files.exists(_root_.java.nio.file.Paths.get(jar.s).nn)
      .assert(_ == true)

    suite(m"Repackager partition"):
      val published = url"https://repo1.maven.org/maven2/g/a/1/a-1.jar"
      val resolve: Text => Optional[HttpUrl] = h => if h == t"aaa" then published else Unset
      val classEntry = Zip.Entry(t"pkg/X.class".as[Path on Zip], t"bytes".in[Data])
      val cached: Repackager.CacheReader =
        h => if h == t"bbb" then (List(classEntry): List[Zip.Entry]) else Unset

      test(m"a published hash becomes a remote requirement"):
        val (requirements, inlined) = Repackager.partition(List(t"aaa"), resolve, cached)
        (requirements.stdlib.length, inlined.stdlib.length)
      .assert(_ == (1, 0))

      test(m"an unpublished but cached hash is inlined"):
        val (requirements, inlined) = Repackager.partition(List(t"bbb"), resolve, cached)
        (requirements.stdlib.length, inlined.stdlib.length)
      .assert(_ == (0, 1))

      test(m"a hash that is neither published nor cached is rejected"):
        capture[Repackager.RepackageError](Repackager.partition(List(t"ccc"), resolve, cached))
      .assert(_ => true)

      test(m"deps.dev lookups run concurrently, not one at a time"):
        // Each lookup blocks on a shared latch that only releases once every lookup has arrived.
        // If the lookups ran sequentially the first would wait alone until it timed out (recording
        // failure); running concurrently, all arrive, the latch releases, and every `await` returns
        // `true`. `cached` returns an empty entry list so an unresolved hash is neither an error nor
        // any inlined output.
        val count = 4
        val latch = juc.CountDownLatch(count)
        val concurrent = juca.AtomicBoolean(true)

        val slowResolve: Repackager.Resolver = _ =>
          latch.countDown()
          if !latch.await(5, juc.TimeUnit.SECONDS) then concurrent.set(false)
          Unset

        val emptyCache: Repackager.CacheReader = _ => (List[Zip.Entry](): List[Zip.Entry])
        val hashes: List[Text] = List(t"h0", t"h1", t"h2", t"h3")
        Repackager.partition(hashes, slowResolve, emptyCache)
        concurrent.get
      .assert(_ == true)

      test(m"progress advances monotonically to the total"):
        val emptyCache: Repackager.CacheReader = _ => (List[Zip.Entry](): List[Zip.Entry])
        val hashes: List[Text] = List(t"h0", t"h1", t"h2")
        val last = juca.AtomicInteger(0)
        val monotonic = juca.AtomicBoolean(true)

        val progress: Repackager.Progress = (done, total) =>
          if done < last.get || total != hashes.stdlib.length then monotonic.set(false)
          last.set(done)

        Repackager.partition(hashes, _ => Unset, emptyCache, progress)
        (monotonic.get, last.get)
      .assert(_ == (true, 3))

    suite(m"Progress bar"):
      test(m"an empty bar is all spaces"):
        ProgressBar.render(0.0).plain
      .assert(_ == t" "*40)

      test(m"a full bar is all full blocks"):
        ProgressBar.render(1.0).plain
      .assert(_ == t"█"*40)

      test(m"a half bar is twenty blocks then twenty spaces"):
        ProgressBar.render(0.5).plain
      .assert(_ == t"█"*20 + t" "*20)

      test(m"a sub-cell fraction renders one partial block"):
        ProgressBar.render(4.0/320).plain
      .assert(_ == t"▌" + t" "*39)

      test(m"the bar is always forty cells wide"):
        List(0.0, 0.1, 0.333, 0.5, 0.9, 1.0).all(ProgressBar.render(_).plain.length == 40)
      .assert(_ == true)

    suite(m"GitHub release index"):
      val hex: Text = t"A"*64
      val lower: Text = t"a"*64
      val other: Text = t"b"*64
      val jarUrl: Text = t"https://github.com/o/r/releases/download/v1/lib-1.jar"
      val oldUrl: Text = t"https://github.com/o/r/releases/download/v0/lib-1.jar"
      val mavenUrl = url"https://repo1.maven.org/maven2/g/a/1/a-1.jar"

      def asset(name: Text, url: Text, digest: Optional[Text]): GitHub.Asset =
        GitHub.Asset(name, url, digest)

      test(m"a .jar asset with a sha256 digest is indexed by lowercase hex"):
        val release = GitHub.Release(List(asset(t"lib-1.jar", jarUrl, t"sha256:$hex")))
        GitHub.indexReleases(List(release))(lower).let(_.show)
      .assert(_ == jarUrl)

      test(m"an asset without a digest is skipped"):
        val release = GitHub.Release(List(asset(t"lib-1.jar", jarUrl, Unset)))
        GitHub.indexReleases(List(release)).size
      .assert(_ == 0)

      test(m"an asset with a non-sha256 digest is skipped"):
        val release = GitHub.Release(List(asset(t"lib-1.jar", jarUrl, t"md5:$hex")))
        GitHub.indexReleases(List(release)).size
      .assert(_ == 0)

      test(m"an asset that is not a .jar is skipped"):
        val release = GitHub.Release(List(asset(t"lib-1.tar.gz", jarUrl, t"sha256:$hex")))
        GitHub.indexReleases(List(release)).size
      .assert(_ == 0)

      test(m"the same hash in two releases keeps the first (newest) release's URL"):
        val newest = GitHub.Release(List(asset(t"lib-1.jar", jarUrl, t"sha256:$hex")))
        val oldest = GitHub.Release(List(asset(t"lib-1.jar", oldUrl, t"sha256:$hex")))
        GitHub.indexReleases(List(newest, oldest))(lower).let(_.show)
      .assert(_ == jarUrl)

      test(m"owner/repo parses into its two parts"):
        GitHub.Repository.parse(t"propensive/soundness")
      .assert(_ == GitHub.Repository(t"propensive", t"soundness"))

      test(m"malformed repository coordinates are rejected"):
        List(t"owner", t"owner/", t"/repo", t"a/b/c").all: text =>
          capture[Repackager.UserError](GitHub.Repository.parse(text)) match
            case Repackager.UserError(_) => true
      .assert(_ == true)

      test(m"the releases JSON decodes, renaming browser_download_url and reading null digests"):
        val json: Text =
          t"""[{"tag_name": "v1", "assets": [
                {"name": "lib-1.jar", "browser_download_url": "$jarUrl",
                 "digest": "sha256:$hex", "size": 12},
                {"name": "lib-1.pom", "browser_download_url": "$oldUrl", "digest": null}]}]"""

        json.read[Json].as[List[GitHub.Release]]
      .assert(_ == List(GitHub.Release(List(asset(t"lib-1.jar", jarUrl, t"sha256:$hex"),
                                            asset(t"lib-1.pom", oldUrl, Unset)))))

      test(m"a hinted repository takes precedence over deps.dev"):
        val index: Map[Text, HttpUrl] = Map(lower -> jarUrl.as[HttpUrl])
        val depsDev: Repackager.Resolver =
          hash => if hash == lower || hash == other then mavenUrl else Unset

        val resolve: Repackager.Resolver = hash => index(hash).or(depsDev(hash))
        (resolve(lower).let(_.show), resolve(other).let(_.show))
      .assert(_ == (jarUrl, mavenUrl.show))

    suite(m"Repackager (end-to-end)"):
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      Zipfile.write(inputJar):
        ( Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], manifestText.in[Data])
          #:: Zip.Entry(t"META-INF/burdock.deps".as[Path on Zip], t"aaa\nbbb".in[Data])
          #:: Zip.Entry(t"com/example/Main.class".as[Path on Zip], t"main".in[Data])
          #:: Chain() ).to[List]

      val resolve: Repackager.Resolver =
        h => if h == t"aaa" then url"https://repo1.maven.org/maven2/g/a/1/a-1.jar" else Unset

      val cached: Repackager.CacheReader =
        h => if h == t"bbb" then (List(Zip.Entry(t"dep/Lib.class".as[Path on Zip], t"lib".in[Data])): List[Zip.Entry])
             else Unset

      Repackager.repackage(inputJar, outputJar, resolve, cached, t"bootstrap-bytes".in[Data])

      val names: List[Text] = Zipfile.read(outputJar).entries.stdlib.map(_.ref.show).to(List)

      val manifest: Text =
        Zipfile.read(outputJar).entries.stdlib.find(_.ref.show == t"META-INF/MANIFEST.MF").get.read[Data].utf8

      test(m"keeps the application's own class"):
        names.has(t"com/example/Main.class")
      .assert(_ == true)

      test(m"inlines the unpublished cached dependency"):
        names.has(t"dep/Lib.class")
      .assert(_ == true)

      test(m"force-includes the bootstrap class"):
        names.has(t"burdock/Bootstrap.class")
      .assert(_ == true)

      test(m"sets Main-Class to the burdock bootstrap"):
        manifest.contains(t"burdock.Bootstrap")
      .assert(_ == true)

      test(m"preserves the original entry point as Burdock-Main"):
        manifest.contains(t"com.example.Main")
      .assert(_ == true)

      test(m"records the published dependency as a requirement"):
        manifest.contains(t"aaa")
      .assert(_ == true)

    suite(m"Repackager (directory entries)"):
      // A fat assembly contains directory entries (e.g. `com/example/`). The zeppelin reader
      // strips their trailing slash and flags them as directories; if copied through as plain
      // entries they would be re-emitted as zero-byte *files* (`com/example`), colliding with
      // the real directory on extraction ("exists but is not directory"). They must be dropped.
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-dir-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-dir-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      Zipfile.write(inputJar):
        ( Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], manifestText.in[Data])
          #:: Zip.Entry(t"META-INF/burdock.deps".as[Path on Zip], t"".in[Data])
          #:: Zip.Entry(t"com/example".as[Path on Zip], t"".in[Data]).asDirectory
          #:: Zip.Entry(t"com/example/Main.class".as[Path on Zip], t"main".in[Data])
          #:: Chain() ).to[List]

      val resolve: Repackager.Resolver = _ => Unset
      val cached: Repackager.CacheReader = _ => Unset

      val summary =
        Repackager.repackage(inputJar, outputJar, resolve, cached, t"bootstrap".in[Data])

      val entries = Zipfile.read(outputJar).entries.stdlib.to(List)
      val names: List[Text] = entries.map(_.ref.show).to(List)

      test(m"the output contains no directory entries"):
        entries.all(!_.directory)
      .assert(_ == true)

      test(m"no zero-byte slash-less package entry remains"):
        names.has(t"com/example")
      .assert(_ == false)

      test(m"the application's own class is kept"):
        names.has(t"com/example/Main.class")
      .assert(_ == true)

      test(m"the summary records the skipped directory entry"):
        summary.directoriesSkipped
      .assert(_ == 1)

    suite(m"Repackager (duplicate-safety)"):
      // A real assembly bundles burdock (its `Main-Class` is `burdock.Bootstrap`), so the
      // input already contains `burdock/Bootstrap.class`; a cached dependency may be a class
      // already present among the application's own entries; and an inlined (unpublished, e.g.
      // locally-published) burdock dependency's cached JAR also carries `burdock/Bootstrap.class`.
      // None of these must produce a duplicate entry in the output (which fails as a
      // `Zip.Error`). See issue #1333.
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-dup-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-dup-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      Zipfile.write(inputJar):
        ( Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], manifestText.in[Data])
          #:: Zip.Entry(t"META-INF/burdock.deps".as[Path on Zip], t"bbb".in[Data])
          #:: Zip.Entry(t"com/example/Main.class".as[Path on Zip], t"main".in[Data])
          #:: Zip.Entry(t"burdock/Bootstrap.class".as[Path on Zip], t"stale-bootstrap".in[Data])
          #:: Zip.Entry(t"dep/Lib.class".as[Path on Zip], t"bundled-lib".in[Data])
          #:: Chain() ).to[List]

      val resolve: Repackager.Resolver = _ => Unset

      val cached: Repackager.CacheReader = h =>
        if h == t"bbb"
        then proscenium.List
             ( Zip.Entry(t"dep/Lib.class".as[Path on Zip], t"cached-lib".in[Data]),
               Zip.Entry(t"burdock/Bootstrap.class".as[Path on Zip], t"cached-bootstrap".in[Data]) )
        else Unset

      Repackager.repackage(inputJar, outputJar, resolve, cached, t"real-bootstrap".in[Data])

      val names: List[Text] = Zipfile.read(outputJar).entries.stdlib.map(_.ref.show).to(List)

      test(m"a bundled bootstrap class is not duplicated"):
        names.stdlib.count(_ == t"burdock/Bootstrap.class")
      .assert(_ == 1)

      test(m"the force-included bootstrap bytes win over the bundled copy"):
        Zipfile.read(outputJar).entries.stdlib.find(_.ref.show == t"burdock/Bootstrap.class").get
        . read[Data].utf8
      .assert(_ == t"real-bootstrap")

      test(m"a cached class already bundled is not duplicated"):
        names.stdlib.count(_ == t"dep/Lib.class")
      .assert(_ == 1)

    suite(m"Repackager (slimming)"):
      // A fat assembly bundles its dependencies' classes. A published dependency is fetched
      // at runtime (a `Burdock-Require` reference), so its bundled classes must be stripped
      // from the repackaged JAR; the application's own classes (and an unpublished, cached
      // dependency's classes) are kept.
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-slim-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-slim-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      Zipfile.write(inputJar):
        ( Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], manifestText.in[Data])
          #:: Zip.Entry(t"META-INF/burdock.deps".as[Path on Zip], t"pub\nunpub".in[Data])
          #:: Zip.Entry(t"com/example/Main.class".as[Path on Zip], t"main".in[Data])
          #:: Zip.Entry(t"published/Lib.class".as[Path on Zip], t"published-bytes".in[Data])
          #:: Zip.Entry(t"unpublished/Lib.class".as[Path on Zip], t"unpublished-bytes".in[Data])
          #:: Chain() ).to[List]

      val published = url"https://repo1.maven.org/maven2/g/a/1/a-1.jar"
      val resolve: Repackager.Resolver = h => if h == t"pub" then published else Unset

      // The published dep's cached JAR lists the class bundled in the assembly (so it can be
      // identified and stripped); the unpublished dep stays bundled.
      val pubEntry = Zip.Entry(t"published/Lib.class".as[Path on Zip], t"x".in[Data])
      val unpubEntry = Zip.Entry(t"unpublished/Lib.class".as[Path on Zip], t"y".in[Data])

      val cached: Repackager.CacheReader = h =>
        if h == t"pub" then (List(pubEntry): List[Zip.Entry])
        else if h == t"unpub" then (List(unpubEntry): List[Zip.Entry])
        else Unset

      Repackager.repackage(inputJar, outputJar, resolve, cached, t"bootstrap".in[Data])

      val names: List[Text] = Zipfile.read(outputJar).entries.stdlib.map(_.ref.show).to(List)

      val manifest: Text =
        Zipfile.read(outputJar).entries.stdlib.find(_.ref.show == t"META-INF/MANIFEST.MF").get
        . read[Data].utf8

      test(m"strips a published dependency's bundled class"):
        names.has(t"published/Lib.class")
      .assert(_ == false)

      test(m"keeps the application's own class"):
        names.has(t"com/example/Main.class")
      .assert(_ == true)

      test(m"keeps an unpublished dependency's bundled class"):
        names.has(t"unpublished/Lib.class")
      .assert(_ == true)

      test(m"records the published dependency as a requirement"):
        manifest.contains(t"pub")
      .assert(_ == true)

    suite(m"Repackager (verbatim copy)"):
      // Entries copied from the input JAR must be passed through byte-for-byte — the same
      // compression method, CRC and payload — never inflated and re-deflated. A `Stored` input
      // entry is the tell: were it recompressed under the default `Deflate` policy, it would come
      // out as `Deflate`.
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-verbatim-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-verbatim-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      // A `Stored` entry (kept uncompressed) and a `Deflate` entry (a compressible payload, so the
      // deflate actually wins and the method is recorded as `Deflate`).
      val storedEntry: Zip.Entry =
        given Zip.Compression = Zip.Compression.Stored
        Zip.Entry(t"pkg/Stored.class".as[Path on Zip], t"stored-payload".in[Data])

      val deflateEntry: Zip.Entry =
        given Zip.Compression = Zip.Compression.Deflate(-1)
        Zip.Entry(t"pkg/Deflated.class".as[Path on Zip], (t"a"*2000).in[Data])

      Zipfile.write(inputJar):
        ( Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], manifestText.in[Data])
          #:: Zip.Entry(t"META-INF/burdock.deps".as[Path on Zip], t"".in[Data])
          #:: storedEntry
          #:: deflateEntry
          #:: Chain() ).to[List]

      Repackager.repackage(inputJar, outputJar, _ => Unset, _ => Unset, t"bootstrap".in[Data])

      def entry(jar: Path on Linux, name: Text): Zip.Entry =
        Zipfile.read(jar).entries.stdlib.find(_.ref.show == name).get

      test(m"a Stored input entry stays Stored (no re-deflate)"):
        entry(outputJar, t"pkg/Stored.class").method
      .assert(_ == Zip.Method.Stored)

      test(m"a Deflate input entry stays Deflate"):
        entry(outputJar, t"pkg/Deflated.class").method
      .assert(_ == Zip.Method.Deflate)

      test(m"the CRC-32 is carried through unchanged"):
        entry(outputJar, t"pkg/Deflated.class").crc32
      .assert(_ == entry(inputJar, t"pkg/Deflated.class").crc32)

      test(m"the compressed size is carried through unchanged"):
        entry(outputJar, t"pkg/Deflated.class").compressedSize
      .assert(_ == entry(inputJar, t"pkg/Deflated.class").compressedSize)

      test(m"the decompressed Stored payload is preserved"):
        entry(outputJar, t"pkg/Stored.class").read[Data].utf8
      .assert(_ == t"stored-payload")

      test(m"the decompressed Deflate payload is preserved"):
        entry(outputJar, t"pkg/Deflated.class").read[Data].utf8
      .assert(_ == t"a"*2000)

    // The bootstrap itself, run as a real JVM against a loopback server: the JAR under test
    // carries `burdock.Bootstrap`, a `Burdock-Main` probe class, and two requirements whose
    // "jars" are arbitrary bytes (they are never class-loaded; only their hashes matter).
    suite(m"Bootstrap (end-to-end)"):
      import java.net.InetSocketAddress
      import java.nio.file as jnf
      import java.security as js
      import java.util as ju
      import java.util.jar as juj
      import java.io as ji
      import com.sun.net.httpserver as csnh

      val javaBinary: Text = t"${_root_.java.lang.System.getProperty("java.home").nn}/bin/java"
      val root: Path on Linux = temporaryDirectory[Path on Linux]/t"burdock-boot-${Uuid().show}"
      jnf.Files.createDirectories(jnf.Paths.get(root.show.s))

      def data(bytes: scala.Array[Byte]): Data = Data.fill(bytes.length)(bytes(_))

      def resource(name: String): scala.Array[Byte] =
        getClass.nn.getResourceAsStream(name).nn.readAllBytes().nn

      def sha256(bytes: scala.Array[Byte]): Text =
        ju.HexFormat.of().nn.formatHex(js.MessageDigest.getInstance("SHA-256").nn.digest(bytes).nn).nn.tt

      def exists(path: Text): Boolean = jnf.Files.exists(jnf.Paths.get(path.s))

      def leftovers(cache: Text): Int =
        val dir = jnf.Paths.get(cache.s, "burdock")
        if !jnf.Files.isDirectory(dir) then 0
        else jnf.Files.list(dir).nn.filter(_.nn.getFileName.nn.toString.nn.endsWith(".tmp")).nn.count().toInt

      // Requests are served on a pool (the default executor is one dispatcher thread, which would
      // serialize them and hide the bootstrap's parallelism), each after a configurable delay,
      // while counting the peak number in flight.
      val server = csnh.HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0).nn
      server.setExecutor(juc.Executors.newCachedThreadPool())
      val inFlight = juca.AtomicInteger()
      val peak = juca.AtomicInteger()
      val requests = juca.AtomicInteger()
      val delayOne = juca.AtomicLong(300)
      val delayTwo = juca.AtomicLong(300)

      def serve(name: String, bytes: scala.Array[Byte], delay: juca.AtomicLong): Unit =
        server.createContext(("/": String)+name, { exchange =>
          requests.incrementAndGet()
          peak.accumulateAndGet(inFlight.incrementAndGet(), (a, b) => Math.max(a, b))
          try
            Thread.sleep(delay.get())
            exchange.nn.sendResponseHeaders(200, bytes.length)
            exchange.nn.getResponseBody.nn.write(bytes)
            exchange.nn.close()
          finally inFlight.decrementAndGet() })

      val one: scala.Array[Byte] = "dependency one".s.getBytes("UTF-8").nn
      val two: scala.Array[Byte] = "dependency two".s.getBytes("UTF-8").nn
      serve("one.jar", one, delayOne)
      serve("two.jar", two, delayTwo)
      server.start()
      val port: Int = server.getAddress.nn.getPort
      val hashOne: Text = sha256(one)
      val hashTwo: Text = sha256(two)

      val requirements: Text =
        t"$hashOne:http://127.0.0.1:$port/one.jar $hashTwo:http://127.0.0.1:$port/two.jar"

      def appJar(name: Text, require: Text): Path on Linux =
        val manifest = juj.Manifest()
        val attributes = manifest.getMainAttributes.nn
        attributes.put(juj.Attributes.Name.MANIFEST_VERSION, "1.0")
        attributes.putValue("Main-Class", "burdock.Bootstrap")
        attributes.putValue("Burdock-Main", "burdock.Probe")
        attributes.putValue("Burdock-Verbosity", "error")
        attributes.putValue("Burdock-Require", require.s)
        val out = ji.ByteArrayOutputStream()
        manifest.write(out)
        val jar: Path on Linux = root/name

        Zipfile.write(jar):
          ( Zip.Entry(t"META-INF/MANIFEST.MF".as[Path on Zip], data(out.toByteArray.nn))
            #:: Zip.Entry(t"burdock/Bootstrap.class".as[Path on Zip], data(resource("/burdock/Bootstrap.class")))
            #:: Zip.Entry(t"burdock/Probe.class".as[Path on Zip], data(resource("/burdock/Probe.class")))
            #:: Chain() ).to[List]

        jar

      val jar: Path on Linux = appJar(t"app.jar", requirements)
      val cache: Text = t"${root.show}/cache"
      val progress: Text = t"${root.show}/progress"

      def launch(jar: Path on Linux, cache: Text): Text =
        sh"env XDG_CACHE_HOME=$cache $javaBinary -Dburdock.progress=$progress -jar $jar".exec[Text]().trim

      def status(jar: Path on Linux, cache: Text): Exit =
        sh"env XDG_CACHE_HOME=$cache $javaBinary -Dburdock.progress=$progress -jar $jar".exec[Exit]()

      val coldOutput: Text = launch(jar, cache)
      val coldRequests: Int = requests.get()

      test(m"the application's main class runs after a cold fetch"):
        coldOutput
      .assert(_ == t"probe")

      test(m"both requirements are fetched once"):
        coldRequests
      .assert(_ == 2)

      test(m"the requirements are cached under XDG_CACHE_HOME"):
        (exists(t"$cache/burdock/$hashOne.jar"), exists(t"$cache/burdock/$hashTwo.jar"))
      .assert(_ == (true, true))

      test(m"the downloads overlap"):
        peak.get()
      .assert(_ >= 2)

      test(m"no temporary file is left in the cache"):
        leftovers(cache)
      .assert(_ == 0)

      test(m"the progress file is removed once fetching completes"):
        exists(progress)
      .assert(_ == false)

      val warmOutput: Text = launch(jar, cache)

      test(m"a warm cache makes no requests"):
        (warmOutput, requests.get() - coldRequests)
      .assert(_ == (t"probe", 0))

      jnf.Files.write(jnf.Paths.get(t"$cache/burdock/$hashOne.jar".s), ("corrupted": String).getBytes("UTF-8").nn)

      test(m"a corrupted cached requirement is rejected with status 1"):
        status(jar, cache)
      .assert(_ == Exit.Fail(1))

      test(m"a malformed requirement exits with status 2"):
        status(appJar(t"malformed.jar", t"not-a-requirement"), cache)
      .assert(_ == Exit.Fail(2))

      // With the server slowed down, the progress file can be watched from outside while the
      // bootstrap runs, which is exactly what Ethereal's launcher does. The two downloads are
      // staggered so that the position after the first completes persists long enough to be seen.
      val slowCache: Text = t"${root.show}/slow-cache"
      delayOne.set(500)
      delayTwo.set(2500)

      val observed: List[Text] =
        supervise:
          val task = async(status(jar, slowCache))
          var seen: List[Text] = Nil
          while !task.ready do
            if exists(progress) then
              val line: Text = jnf.Files.readString(jnf.Paths.get(progress.s)).nn.tt.trim
              if line != t"" && !seen.has(line) then seen = line :: seen
            snooze(0.05*Second)
          seen.reverse

      test(m"the progress file reports the requirements before any download completes"):
        observed match
          case first :: _ => first
          case _          => t""
      .assert(_ == t"0 2 0")

      test(m"the progress file reports the first requirement's completion"):
        observed.exists(_.starts(t"1 2 "))
      .assert(_ == true)

      server.stop(0)
