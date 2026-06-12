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
import errorDiagnostics.stackTraces
import charEncoders.utf8
import systems.java
import temporaryDirectories.system

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
      val classEntry = Bootstrapper.Entry(t"pkg/X.class", t"bytes".data)
      val cached: Text => Optional[List[Bootstrapper.Entry]] =
        h => if h == t"bbb" then List(classEntry) else Unset

      test(m"a published hash becomes a remote requirement"):
        val (requirements, inlined) = Repackage.partition(List(t"aaa"), resolve, cached)
        (requirements.length, inlined.length)
      .assert(_ == (1, 0))

      test(m"an unpublished but cached hash is inlined"):
        val (requirements, inlined) = Repackage.partition(List(t"bbb"), resolve, cached)
        (requirements.length, inlined.length)
      .assert(_ == (0, 1))

      test(m"a hash that is neither published nor cached is rejected"):
        capture[Repackage.RepackageError](Repackage.partition(List(t"ccc"), resolve, cached))
      .assert(_ => true)

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

      val resolve: Repackage.Resolver =
        h => if h == t"aaa" then url"https://repo1.maven.org/maven2/g/a/1/a-1.jar" else Unset

      val cached: Repackage.CacheReader =
        h => if h == t"bbb" then List(Bootstrapper.Entry(t"dep/Lib.class", t"lib".data)) else Unset

      Repackage.repackage(inputJar, outputJar, resolve, cached, t"bootstrap-bytes".data)

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

    suite(m"Repackager (duplicate-safety)"):
      // A real assembly bundles burdock (its `Main-Class` is `burdock.Bootstrap`), so the
      // input already contains `burdock/Bootstrap.class`; and a cached dependency may be a
      // class already present among the application's own entries. Neither must produce a
      // duplicate entry in the output (which would fail as a `ZipError`). See issue #1333.
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

      val resolve: Repackage.Resolver = _ => Unset

      val cached: Repackage.CacheReader =
        h => if h == t"bbb" then List(Bootstrapper.Entry(t"dep/Lib.class", t"cached-lib".data))
             else Unset

      Repackage.repackage(inputJar, outputJar, resolve, cached, t"real-bootstrap".data)

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
