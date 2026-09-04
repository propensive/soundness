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

import java.net as jn
import java.nio.file as jnf

import ambience.*
import anticipation.*
import contingency.*
import distillate.*
import escapade.*
import exoskeleton.*
import fulminate.*
// `Message`'s `Printable` instance now lives in `fulminate.print`, outside `Message`'s implicit
// scope, so printing a `Message` needs it imported by name.
import fulminate.printables.messagePrintable
import galilei.*
import gossamer.*
import hellenism.*, classloaders.threadContextClassloader
import prepositional.*
import rudiments.*
import serpentine.*
import spectacular.*
import symbolism.*
import turbulence.*
import urticose.*
import vacuous.*
import zeppelin.*

import Repackager.UserError
import backstops.stackTraceBackstop
import environments.javaBaseEnvironment
import executives.directExecutive
import filesystemOptions.dereferenceSymlinks
import interpreters.posixInterpreter
import stdios.fileDescriptorStdio
import systems.javaBaseSystem
import termcaps.environmentTermcap

import filesystemBackends.javaBaseFilesystem

// `linearSize`: the externalized-dependency count is reported once, at the end of a repackage
// that has already walked every entry in the JAR.
import denominative.size
import denominative.dysasymptotics.linearSize

// The repackager's command-line logic, launched by the `soundness.repackage` entry point.
// It self-locates the application JAR it is running from and rewrites it in place (see
// `Repackager.repackage`). Its only arguments are hints: `--github owner/repo` (repeatable,
// or comma-separated) names repositories whose release assets are a download source.
def repackage(arguments: List[Text]): Unit = application(arguments):
  recover:
    case error: Error =>
      Err.println(error.message)
      Exit.Fail(1)

  . protect:
      val loader: ClassLoader = summon[Classloader].java
      val repositories: List[GitHub.Repository] = parseArguments(summon[Cli].arguments.map(_()))

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
          jnf.Paths.get(jar.getJarFileURL.nn.toURI.nn).nn.toString.nn.tt.as[Path on Linux]

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
      val cacheDir: Path on Linux = t"$home/.cache/burdock".as[Path on Linux]

      def cached(hash: Text): Optional[List[Zip.Entry]] =
        val cacheJar: Path on Linux = cacheDir/t"$hash.jar"

        if !cacheJar.existent() then Unset else
          Zipfile.read(cacheJar).entries.filter: entry =>
            !entry.directory && entry.ref.show != t"META-INF/MANIFEST.MF"

          . stdlib.to(List)

      val tmpFile: Path on Linux =
        inputJar.parent.or(panic(m"a jar file always has a parent directory"))
        / t"${inputJar.name}.tmp"

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

      // The GitHub index is a few requests per repository rather than one per dependency, so
      // it is built once up front; the hinted repositories are consulted before deps.dev,
      // since a hint the user typed is meant to take precedence over the global index.
      val token: Optional[Text] = safely(Environment[Text](t"GITHUB_TOKEN"))
      val index: Map[Text, HttpUrl] = GitHub.index(repositories, token)

      val resolve: Repackager.Resolver = hash =>
        index(hash).let: url =>
          Log.info(DepsEvent.Resolved(hash, url.show))
          url

        . or(DepsDev.mavenUrl(hash))

      if animate then Out.print(csi.dectcem(false))

      val summary =
        Repackager.repackage(inputJar, tmpFile, resolve, cached, bootstrapClass, onProgress)

      // Move off the bar's line and restore the cursor before printing the summary.
      if animate then
        Out.print(csi.dectcem(true))
        Out.println()

      import filesystemOptions.overwritePreexisting
      import filesystemOptions.deleteOnlyEmpty
      import filesystemOptions.moveAtomically
      import filesystemOptions.requireParents

      tmpFile.moveTo(inputJar)

      val bytes: Long = jnf.Files.size(jnf.Paths.get(inputJar.show.s).nn)
      Out.println(m"Repackaged $inputJar")
      Out.println(m"  input entries:          ${summary.inputEntries}")
      Out.println(m"  directory entries skipped: ${summary.directoriesSkipped}")
      Out.println(m"  application classes kept:  ${summary.ownKept}")
      Out.println(m"  github repositories:       ${repositories.size}")
      Out.println(m"  github assets indexed:     ${index.size}")
      Out.println(m"  dependencies externalized: ${summary.externalized.size}")

      summary.externalized.each: requirement => Out.println(m"    - ${requirement.text}")

      Out.println(m"  dependency classes inlined: ${summary.inlined}")
      Out.println(m"  bundled classes stripped:  ${summary.stripped}")
      Out.println(m"  output entries:         ${summary.outputEntries}")
      Out.println(m"  output size (bytes):    $bytes")

      Exit.Ok

private val usage: Message = m"usage: soundness.repackage [--github owner/repo]..."

// The flag vocabulary is a single repeatable flag, so it is parsed by hand rather than
// depending on `exoskeleton.args`. Accepts `--github owner/repo`, `--github=owner/repo`, and
// comma-separated lists in either form.
def parseArguments(arguments: List[Text]): List[GitHub.Repository] raises UserError =
  def repositories(operand: Text): List[GitHub.Repository] =
    operand.cut(t",").filter(_ != t"").map(GitHub.Repository.parse(_))

  def recur(arguments: List[Text]): List[GitHub.Repository] = arguments match
    case t"--github" :: operand :: rest => repositories(operand) + recur(rest)
    case t"--github" :: Nil             => abort(UserError(m"--github needs a repository; $usage"))

    case argument :: rest =>
      if argument.starts(t"--github=") then repositories(argument.skip(9)) + recur(rest)
      else abort(UserError(m"unrecognized argument $argument; $usage"))

    case _ => Nil

  recur(arguments)
