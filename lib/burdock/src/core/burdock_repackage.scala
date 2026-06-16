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

import java.net as jn
import java.nio.file as jnf

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import exoskeleton.*
import fulminate.*
import galilei.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import turbulence.*
import vacuous.*
import zeppelin.*

import Repackager.UserError
import backstops.stackTraceBackstop
import environments.javaEnvironment
import executives.directExecutive
import filesystemOptions.dereferenceSymlinks.enabled
import interpreters.posixInterpreter
import stdios.virtualMachineStdio
import systems.javaSystem
import termcaps.environmentTermcap

// The repackager's command-line logic, launched by the `soundness.repackage` entry point.
// It takes no arguments — it self-locates the application JAR it is running from and
// rewrites it in place (see `Repackager.repackage`).
def repackage(): Unit = application(Nil):
  whereas:
    case error: Error =>
      Err.println(error.message)
      Exit.Fail(1)

  . recover:
      val loader: ClassLoader = summon[Classloader].java

      // Self-locate the application JAR: the classpath entry holding the
      // `META-INF/burdock.deps` resource the build-time macro wrote. (Locating by
      // the `burdock.Bootstrap` class would find burdock's own JAR, not the app's,
      // once the app JAR is slim.)
      val resourcePath: String = burdock.internal.ResourcePath

      val depsResource: jn.URL =
        Optional(loader.getResource(resourcePath)).lest:
          UserError(m"this JAR was not built with Burdock (no ${resourcePath.tt})")

      val connection: jn.URLConnection = depsResource.openConnection().nn

      val inputJar: Path on Linux = connection match
        case jar: jn.JarURLConnection =>
          jnf.Paths.get(jar.getJarFileURL.nn.toURI.nn).nn.toString.nn.tt.decode[Path on Linux]

        case _ =>
          abort(UserError(m"Burdock can only repackage a JAR file, not a directory"))

      Out.println(m"Repackaging $inputJar")

      // The bootstrap loader cannot be downloaded — it does the downloading — so
      // its bytes are force-included from burdock's own (boot) JAR on the classpath.
      val bootstrapClass: Data = (Classpath/"burdock"/"Bootstrap.class").read[Data]

      // Unpublished dependencies are reconstructed from the build-time hard-links in
      // `~/.cache/burdock/<sha256>.jar` (see `externalize`); published ones resolve via
      // deps.dev and are referenced by URL rather than inlined.
      val home: Text = _root_.java.lang.System.getProperty("user.home").nn.tt
      val cacheDir: Path on Linux = t"$home/.cache/burdock".decode[Path on Linux]

      def cached(hash: Text): Optional[List[Zip.Entry]] =
        val cacheJar: Path on Linux = cacheDir/t"$hash.jar"

        if !cacheJar.exists() then Unset else
          Zipfile.read(cacheJar).entries.filter: entry =>
            !entry.directory && entry.ref.show != t"META-INF/MANIFEST.MF"

          . to(List)

      val tmpFile: Path on Linux = inputJar.parent.vouch/t"${inputJar.name}.tmp"
      Repackager.repackage(inputJar, tmpFile, DepsDev.mavenUrl, cached, bootstrapClass)

      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.disabled
      import filesystemOptions.moveAtomically.enabled
      import filesystemOptions.createNonexistentParents.disabled

      tmpFile.moveTo(inputJar)
      Out.println(m"Repackaged $inputJar")

      Exit.Ok
