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
package ziggurat

import soundness.*

import systems.javaSystem
import environments.javaEnvironment
import temporaryDirectories.systemTemporaryDirectory
import workingDirectories.defaultWorkingDirectory
import logging.silentLogging
import stdios.virtualMachineStdio
import termcaps.environmentTermcap

import strategies.throwUnsafely
import charEncoders.utf8Encoder
import providers.javaStdlibProvider
import alphabets.hexLowerCase
import errorDiagnostics.stackTracesDiagnostics

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled

import filesystemBackends.virtualMachine

object Tests extends Suite(m"Ziggurat tests"):
  def run(): Unit =
    if Ci() then Out.println(t"Running in CI; skipping Ziggurat tests")
    else runTests()

  def runTests(): Unit =
    val tempDirs = scala.collection.mutable.ListBuffer.empty[Path on Linux]

    def tempDir(): Path on Linux =
      val dir: Path on Linux = temporaryDirectory[Path on Linux] / Uuid().show
      dir.create[Directory]()
      tempDirs += dir
      dir

    try runTestsBody(tempDir) finally tempDirs.foreach: dir =>
      safely(dir.delete())

  private def runTestsBody(tempDir: () => Path on Linux): Unit =
    val labels = List(t"linux-x64", t"linux-arm64", t"macos-x64", t"macos-arm64")

    val payloads = labels.map: label =>
      val src = t"#!/bin/sh\necho 'hello from $label'\n"
      Payload(label, src.data, gzip = true)

    val bundleBytes: Data = Xeq.installer(payloads)

    def stage(): (Path on Linux, Path on Linux) =
      val dir = tempDir()
      val script = dir / t"hello"
      script.create[File]()
      script.open: handle =>
        handle.write(LazyList(bundleBytes))
      script.executable() = true
      (dir, script)

    val dockerOk = safely(sh"docker --version".exec[Exit]()) == Exit.Ok

    val hostLabel = sh"uname -m".exec[Text]().trim match
      case t"arm64" | t"aarch64" => t"macos-arm64"
      case _                     => t"macos-x64"

    suite(m"bundle()"):
      test(m"output starts with bash shebang"):
        bundleBytes.utf8.starts(t"#!/usr/bin/env bash")
      .assert(_ == true)

      test(m"index line contains every label"):
        val text = bundleBytes.utf8
        labels.forall: label =>
          text.contains(t"$label=")
      .assert(_ == true)

    suite(m"native macOS exec"):
      test(m"selects host platform payload"):
        val (_, script) = stage()
        sh"$script".exec[Text]().trim
      .assert(_ == t"hello from $hostLabel")

    def stageDownloader(jar: Data, entries: List[(Text, Text, Text)]): Path on Linux =
      val dir = tempDir()
      val script = dir / t"fetch"
      script.create[File]()
      script.open: handle =>
        handle.write(LazyList(Xeq.onlineLauncher(jar, entries)))
      script.executable() = true
      script

    // Serve the binaries as `file://` URLs so the generated downloader's curl
    // path is exercised without standing up an HTTP server.
    def fileEntry(dir: Path on Linux, label: Text, body: Text, hash: Optional[Text] = Unset)
    :   (Text, Text, Text) =
      val bin = dir / t"bin-$label"
      bin.create[File]()
      val bytes = body.data
      bin.open: handle =>
        handle.write(LazyList(bytes))
      (label, t"file://$bin", hash.or(bytes.digest[Sha2[256]].serialize[Hex]))

    suite(m"onlineLauncher()"):
      val entries = labels.map(fileEntry(tempDir(), _, t"#!/bin/sh\n"))
      val script: Data = Xeq.onlineLauncher(t"JAR".data, entries)

      test(m"output starts with bash shebang"):
        script.utf8.starts(t"#!/usr/bin/env bash")
      .assert(_ == true)

      test(m"assets line contains every label"):
        val text = script.utf8
        labels.forall { label => text.contains(t"$label=") }
      .assert(_ == true)

      test(m"embeds the JAR once as the data payload"):
        script.utf8.contains(t"index:data=1")
      .assert(_ == true)

    suite(m"native macOS download"):
      // The "stub" served here exits before the appended JAR bytes are reached, so the
      // launcher's download → verify → append-embedded-JAR → exec path runs end-to-end.
      test(m"downloads, verifies, assembles and runs host binary"):
        val dir = tempDir()
        val entries = labels.map: label =>
          fileEntry(dir, label, t"#!/bin/sh\necho 'fetched $label'\nexit 0\n")
        sh"${stageDownloader(t"JAR".data, entries)}".exec[Text]().trim
      .assert(_ == t"fetched $hostLabel")

      test(m"rejects a binary whose hash does not match"):
        val dir = tempDir()
        val badHash = t"0"*64
        val entries = labels.map: label =>
          fileEntry(dir, label, t"#!/bin/sh\necho oops\nexit 0\n", badHash)
        sh"${stageDownloader(t"JAR".data, entries)}".exec[Exit]()
      .assert(_ != Exit.Ok)

    suite(m"Packager validation"):
      // These configurations are rejected before any assembly or I/O happens, so
      // the (non-existent) jar paths are never touched.
      def config
         (delivery:     Packaging.Delivery,
          dependencies: Packaging.Dependencies,
          runnerSource: Packaging.RunnerSource = Packaging.RunnerSource.Remote(t"https://x.test/", Map()),
          targets:      List[Text]             = List(t"linux-x64"))
      :   Packaging =
        val dir = tempDir()
        Packaging
         (name         = t"hello",
          targets      = targets,
          delivery     = delivery,
          dependencies = dependencies,
          output       = dir/t"hello",
          runnerSource = runnerSource)

      val fatJar: Packaging.Dependencies = Packaging.Dependencies.FatJar(tempDir()/t"app.jar")

      test(m"Burdock remote dependencies are rejected"):
        val dependencies = Packaging.Dependencies.BurdockRemote(tempDir()/t"app.jar")
        capture[PackageError](Packager.pack(config(Packaging.Delivery.EmbedAll, dependencies)))
      .assert(_ => true)

      test(m"remote runner with no hash for the target is rejected"):
        // Fails on the missing-hash check before any download is attempted.
        val remote = Packaging.RunnerSource.Remote(t"https://example.invalid/", Map())
        capture[PackageError]:
          Packager.pack(config(Packaging.Delivery.Native, fatJar, remote))
      .assert(_ => true)

      test(m"native delivery with multiple targets is rejected"):
        capture[PackageError]:
          Packager.pack(config(Packaging.Delivery.Native, fatJar, targets = labels))
      .assert(_ => true)

    suite(m"Packager assembly (offline, local stubs)"):
      // A fake bare stub carrying the ETHRCFG marker so `Assembler.patch` can patch it,
      // followed by enough zero bytes for the 24-byte metadata and public-key regions.
      def writeStub(dir: Path on Linux, label: Text): Unit =
        val file: Path on Linux = t"$dir/runner-$label".as[Path on Linux]

        val bytes: Array[Byte] =
          Array.fill(64)(0.toByte)
          ++ ethereal.Assembler.MagicMarker
          ++ Array.fill(64 + ethereal.Assembler.PublicKeyLength)(0.toByte)

        file.create[File]()
        file.open(LazyList(bytes.immutable(using Unsafe): Data).writeTo(_))

      test(m"EmbedAll bundles the JAR once and every patched stub"):
        val dir: Path on Linux = tempDir()
        labels.each(writeStub(dir, _))

        val jar: Path on Linux = dir/t"app.jar"
        jar.create[File]()
        jar.open(LazyList(t"JARBYTES".data).writeTo(_))

        val out: Path on Linux = dir/t"hello"

        val packaging: Packaging =
          Packaging
            ( name         = t"hello",
              targets      = labels,
              delivery     = Packaging.Delivery.EmbedAll,
              dependencies = Packaging.Dependencies.FatJar(jar),
              output       = out,
              runnerSource = Packaging.RunnerSource.Local(dir) )

        Packager.pack(packaging)
        val text: Text = out.open(_.read[Data]).utf8

        text.starts(t"#!/usr/bin/env bash") && text.contains(t"data=")
        && labels.all { label => text.contains(t"$label=") }
      .assert(_ == true)

      test(m"Download embeds the JAR once and an asset row per target"):
        val dir: Path on Linux = tempDir()

        val jar: Path on Linux = dir/t"app.jar"
        jar.create[File]()
        jar.open(LazyList(t"JARBYTES".data).writeTo(_))

        val out: Path on Linux = dir/t"hello"
        val hashes: Map[Text, Text] = labels.map(_ -> t"0"*64).to(Map)

        val packaging: Packaging =
          Packaging
            ( name         = t"hello",
              targets      = labels,
              delivery     = Packaging.Delivery.Download,
              dependencies = Packaging.Dependencies.FatJar(jar),
              output       = out,
              runnerSource = Packaging.RunnerSource.Remote(t"https://r.test/", hashes) )

        Packager.pack(packaging)
        val text: Text = out.open(_.read[Data]).utf8

        text.contains(t"index:data=1")
        && labels.all { label => text.contains(t"$label=https://r.test/runner-$label") }
      .assert(_ == true)

    // Docker on macOS cannot run macOS containers, so macOS coverage is host-native only.
    if !dockerOk then Out.println(t"Docker unavailable; skipping Linux container tests")
    else
      suite(m"docker linux/amd64"):
        test(m"selects linux-x64 payload"):
          val (dir, _) = stage()
          val mount = t"$dir:/work"
          sh"docker run --rm --platform linux/amd64 -v $mount -w /work ubuntu:24.04 ./hello"
            .exec[Text]().trim
        .assert(_ == t"hello from linux-x64")

      suite(m"docker linux/arm64"):
        test(m"selects linux-arm64 payload"):
          val (dir, _) = stage()
          val mount = t"$dir:/work"
          sh"docker run --rm --platform linux/arm64 -v $mount -w /work ubuntu:24.04 ./hello"
            .exec[Text]().trim
        .assert(_ == t"hello from linux-arm64")

    val winHost: Optional[Text] = safely(Environment.windowsHost[Text])

    if winHost.present then
      val host = winHost.vouch
      val winSshOk =
        safely(sh"ssh -o BatchMode=yes -o ConnectTimeout=5 $host echo ok".exec[Exit]()) == Exit.Ok

      if !winSshOk
      then Out.println(t"Windows host $host unreachable via SSH; skipping Windows tests")
      else
        val workDir: Path on Linux = tempDir()

        val compilePs = workDir / t"compile.ps1"
        compilePs.create[File]()
        val psContent = t"""$$src = @'
class Hello { static void Main() { System.Console.WriteLine("hello from windows-arm64"); } }
'@
Add-Type -TypeDefinition $$src -OutputAssembly ziggurat-test-hello.exe -OutputType ConsoleApplication
"""
        compilePs.open: handle =>
          LazyList(psContent.data).writeTo(handle)

        val localExe = workDir / t"hello.exe"

        val bootstrapped =
          (safely(sh"scp -q $compilePs $host:ziggurat-test-compile.ps1".exec[Exit]()) == Exit.Ok)
          && (safely(sh"ssh $host powershell -ExecutionPolicy Bypass -File ziggurat-test-compile.ps1"
            .exec[Exit]()) == Exit.Ok)
          && (safely(sh"scp -q $host:ziggurat-test-hello.exe $localExe".exec[Exit]()) == Exit.Ok)

        if !bootstrapped then
          Out.println(t"Failed to bootstrap hello.exe on $host; skipping Windows tests")
          safely(sh"ssh $host del /q ziggurat-test-*".exec[Exit]())
        else
          val winArm64Bytes: Data = localExe.open(_.read[Data])
          val winBundle = Xeq.installer:
            payloads :+ Payload(t"windows-arm64", winArm64Bytes, gzip = false)

          def stageAndCopy(extension: Text): Text =
            val script = workDir / t"hello-${Uuid().show}.$extension"
            script.create[File]()
            script.open: handle =>
              LazyList(winBundle).writeTo(handle)
            val remote = t"ziggurat-test-${Uuid().show}.$extension"
            sh"scp -q $script $host:$remote".exec[Exit]()
            remote

          try
            suite(m"windows-arm64 via cmd.exe"):
              test(m"selects windows-arm64 payload"):
                val name = stageAndCopy(t"bat")
                sh"ssh $host cmd /c $name".exec[Text]()
              .assert(_.contains(t"hello from windows-arm64"))

            suite(m"windows-arm64 via PowerShell"):
              test(m"selects windows-arm64 payload"):
                val name = stageAndCopy(t"ps1")
                sh"ssh $host powershell -ExecutionPolicy Bypass -File $name".exec[Text]()
              .assert(_.contains(t"hello from windows-arm64"))
          finally
            safely(sh"ssh $host del /q ziggurat-test-*".exec[Exit]())
