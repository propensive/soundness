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

import soundness.*

import strategies.throwUnsafely
import errorDiagnostics.stackTracesDiagnostics
import charEncoders.utf8Encoder
import systems.javaSystem
import temporaryDirectories.systemTemporaryDirectory

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
        hashes.length
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
      val classEntry = Zip.Entry(t"pkg/X.class".decode[Path on Zip], t"bytes".data)
      val cached: Repackager.CacheReader =
        h => if h == t"bbb" then List(classEntry) else Unset

      test(m"a published hash becomes a remote requirement"):
        val (requirements, inlined) = Repackager.partition(List(t"aaa"), resolve, cached)
        (requirements.length, inlined.length)
      .assert(_ == (1, 0))

      test(m"an unpublished but cached hash is inlined"):
        val (requirements, inlined) = Repackager.partition(List(t"bbb"), resolve, cached)
        (requirements.length, inlined.length)
      .assert(_ == (0, 1))

      test(m"a hash that is neither published nor cached is rejected"):
        capture[Repackager.RepackageError](Repackager.partition(List(t"ccc"), resolve, cached))
      .assert(_ => true)

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

    suite(m"Repackager (end-to-end)"):
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      Zipfile.write(inputJar):
        Zip.Entry(t"META-INF/MANIFEST.MF".decode[Path on Zip], manifestText.data)
        #:: Zip.Entry(t"META-INF/burdock.deps".decode[Path on Zip], t"aaa\nbbb".data)
        #:: Zip.Entry(t"com/example/Main.class".decode[Path on Zip], t"main".data)
        #:: LazyList()

      val resolve: Repackager.Resolver =
        h => if h == t"aaa" then url"https://repo1.maven.org/maven2/g/a/1/a-1.jar" else Unset

      val cached: Repackager.CacheReader =
        h => if h == t"bbb" then List(Zip.Entry(t"dep/Lib.class".decode[Path on Zip], t"lib".data))
             else Unset

      Repackager.repackage(inputJar, outputJar, resolve, cached, t"bootstrap-bytes".data)

      val names: List[Text] = Zipfile.read(outputJar).entries.map(_.ref.show).to(List)

      val manifest: Text =
        Zipfile.read(outputJar).entries.find(_.ref.show == t"META-INF/MANIFEST.MF").get.read[Data].utf8

      test(m"keeps the application's own class"):
        names.contains(t"com/example/Main.class")
      .assert(_ == true)

      test(m"inlines the unpublished cached dependency"):
        names.contains(t"dep/Lib.class")
      .assert(_ == true)

      test(m"force-includes the bootstrap class"):
        names.contains(t"burdock/Bootstrap.class")
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
        Zip.Entry(t"META-INF/MANIFEST.MF".decode[Path on Zip], manifestText.data)
        #:: Zip.Entry(t"META-INF/burdock.deps".decode[Path on Zip], t"".data)
        #:: Zip.Entry(t"com/example".decode[Path on Zip], t"".data).copy(directory = true)
        #:: Zip.Entry(t"com/example/Main.class".decode[Path on Zip], t"main".data)
        #:: LazyList()

      val resolve: Repackager.Resolver = _ => Unset
      val cached: Repackager.CacheReader = _ => Unset

      val summary =
        Repackager.repackage(inputJar, outputJar, resolve, cached, t"bootstrap".data)

      val entries = Zipfile.read(outputJar).entries.to(List)
      val names: List[Text] = entries.map(_.ref.show)

      test(m"the output contains no directory entries"):
        entries.all(!_.directory)
      .assert(_ == true)

      test(m"no zero-byte slash-less package entry remains"):
        names.contains(t"com/example")
      .assert(_ == false)

      test(m"the application's own class is kept"):
        names.contains(t"com/example/Main.class")
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
      // `ZipError`). See issue #1333.
      val tmp: Path on Linux = temporaryDirectory[Path on Linux]
      val inputJar: Path on Linux = tmp/t"burdock-dup-in-${Uuid().show}.jar"
      val outputJar: Path on Linux = tmp/t"burdock-dup-out-${Uuid().show}.jar"

      val manifestText: Text = t"Manifest-Version: 1.0\nMain-Class: com.example.Main\n\n"

      Zipfile.write(inputJar):
        Zip.Entry(t"META-INF/MANIFEST.MF".decode[Path on Zip], manifestText.data)
        #:: Zip.Entry(t"META-INF/burdock.deps".decode[Path on Zip], t"bbb".data)
        #:: Zip.Entry(t"com/example/Main.class".decode[Path on Zip], t"main".data)
        #:: Zip.Entry(t"burdock/Bootstrap.class".decode[Path on Zip], t"stale-bootstrap".data)
        #:: Zip.Entry(t"dep/Lib.class".decode[Path on Zip], t"bundled-lib".data)
        #:: LazyList()

      val resolve: Repackager.Resolver = _ => Unset

      val cached: Repackager.CacheReader = h =>
        if h == t"bbb"
        then Zip.Entry(t"dep/Lib.class".decode[Path on Zip], t"cached-lib".data)
             :: Zip.Entry(t"burdock/Bootstrap.class".decode[Path on Zip], t"cached-bootstrap".data)
             :: Nil
        else Unset

      Repackager.repackage(inputJar, outputJar, resolve, cached, t"real-bootstrap".data)

      val names: List[Text] = Zipfile.read(outputJar).entries.map(_.ref.show).to(List)

      test(m"a bundled bootstrap class is not duplicated"):
        names.count(_ == t"burdock/Bootstrap.class")
      .assert(_ == 1)

      test(m"the force-included bootstrap bytes win over the bundled copy"):
        Zipfile.read(outputJar).entries.find(_.ref.show == t"burdock/Bootstrap.class").get
        . read[Data].utf8
      .assert(_ == t"real-bootstrap")

      test(m"a cached class already bundled is not duplicated"):
        names.count(_ == t"dep/Lib.class")
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
        Zip.Entry(t"META-INF/MANIFEST.MF".decode[Path on Zip], manifestText.data)
        #:: Zip.Entry(t"META-INF/burdock.deps".decode[Path on Zip], t"pub\nunpub".data)
        #:: Zip.Entry(t"com/example/Main.class".decode[Path on Zip], t"main".data)
        #:: Zip.Entry(t"published/Lib.class".decode[Path on Zip], t"published-bytes".data)
        #:: Zip.Entry(t"unpublished/Lib.class".decode[Path on Zip], t"unpublished-bytes".data)
        #:: LazyList()

      val published = url"https://repo1.maven.org/maven2/g/a/1/a-1.jar"
      val resolve: Repackager.Resolver = h => if h == t"pub" then published else Unset

      // The published dep's cached JAR lists the class bundled in the assembly (so it can be
      // identified and stripped); the unpublished dep stays bundled.
      val pubEntry = Zip.Entry(t"published/Lib.class".decode[Path on Zip], t"x".data)
      val unpubEntry = Zip.Entry(t"unpublished/Lib.class".decode[Path on Zip], t"y".data)

      val cached: Repackager.CacheReader = h =>
        if h == t"pub" then List(pubEntry)
        else if h == t"unpub" then List(unpubEntry)
        else Unset

      Repackager.repackage(inputJar, outputJar, resolve, cached, t"bootstrap".data)

      val names: List[Text] = Zipfile.read(outputJar).entries.map(_.ref.show).to(List)

      val manifest: Text =
        Zipfile.read(outputJar).entries.find(_.ref.show == t"META-INF/MANIFEST.MF").get
        . read[Data].utf8

      test(m"strips a published dependency's bundled class"):
        names.contains(t"published/Lib.class")
      .assert(_ == false)

      test(m"keeps the application's own class"):
        names.contains(t"com/example/Main.class")
      .assert(_ == true)

      test(m"keeps an unpublished dependency's bundled class"):
        names.contains(t"unpublished/Lib.class")
      .assert(_ == true)

      test(m"records the published dependency as a requirement"):
        manifest.contains(t"pub")
      .assert(_ == true)
