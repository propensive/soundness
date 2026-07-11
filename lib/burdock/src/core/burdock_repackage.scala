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
package burdock

import java.net as jn
import java.nio.file as jnf

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import escapade.*
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

import filesystemBackends.virtualMachine

// The repackager's command-line logic, launched by the `soundness.repackage` entry point.
// It takes no arguments — it self-locates the application JAR it is running from and
// rewrites it in place (see `Repackager.repackage`).
def repackage(): Unit = application(Nil):
  recover:
    case error: Error =>
      Err.println(error.message)
      Exit.Fail(1)

  . protect:
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
      val bootstrapClass: Data = (Classpath/"burdock"/"Bootstrap.class").stream[Data].read[Data]

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

      // Only animate on a real terminal; when stdout is redirected the carriage-return redraws
      // and cursor escapes would garble the output, so we suppress them and let the final summary
      // stand on its own.
      val animate: Boolean = summon[Stdio].termcap.ansi

      // Redraw a fixed-width progress bar in place (a leading carriage-return returns to column 0;
      // a trailing erase-to-end-of-line wipes any residue from a longer previous frame) as each
      // dependency is resolved and externalized or inlined.
      def onProgress(done: Int, total: Int): Unit =
        if animate && total > 0 then
          Out.print(e"\r${ProgressBar.render(done.toDouble/total)} $done/$total${csi.el()}")

      if animate then Out.print(csi.dectcem(false))

      val summary =
        Repackager.repackage
          ( inputJar, tmpFile, DepsDev.mavenUrl, cached, bootstrapClass, onProgress )

      // Move off the bar's line and restore the cursor before printing the summary.
      if animate then
        Out.print(csi.dectcem(true))
        Out.println()

      import filesystemOptions.overwritePreexisting.enabled
      import filesystemOptions.deleteRecursively.disabled
      import filesystemOptions.moveAtomically.enabled
      import filesystemOptions.createNonexistentParents.disabled

      tmpFile.moveTo(inputJar)

      val bytes: Long = jnf.Files.size(jnf.Paths.get(inputJar.show.s).nn)
      Out.println(m"Repackaged $inputJar")
      Out.println(m"  input entries:          ${summary.inputEntries}")
      Out.println(m"  directory entries skipped: ${summary.directoriesSkipped}")
      Out.println(m"  application classes kept:  ${summary.ownKept}")
      Out.println(m"  dependencies externalized: ${summary.externalized.length}")

      summary.externalized.each: requirement =>
        Out.println(m"    - ${requirement.text}")

      Out.println(m"  dependency classes inlined: ${summary.inlined}")
      Out.println(m"  bundled classes stripped:  ${summary.stripped}")
      Out.println(m"  output entries:         ${summary.outputEntries}")
      Out.println(m"  output size (bytes):    $bytes")

      Exit.Ok
