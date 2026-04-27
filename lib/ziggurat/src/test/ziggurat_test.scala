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
package ziggurat

import soundness.*

import systems.java
import environments.java
import temporaryDirectories.system
import workingDirectories.default
import logging.silent
import stdios.virtualMachine.ansi

import strategies.throwUnsafely
import charEncoders.utf8

import filesystemOptions.dereferenceSymlinks.enabled
import filesystemOptions.readAccess.enabled
import filesystemOptions.writeAccess.enabled
import filesystemOptions.overwritePreexisting.enabled
import filesystemOptions.createNonexistent.enabled
import filesystemOptions.createNonexistentParents.enabled
import filesystemOptions.deleteRecursively.enabled

object Tests extends Suite(m"Ziggurat tests"):
  def run(): Unit =
    if Ci() then Out.println(t"Running in CI; skipping Ziggurat tests")
    else runTests()

  def runTests(): Unit =
    val labels = List(t"linux-x64", t"linux-arm64", t"macos-x64", t"macos-arm64")

    val payloads = labels.map: label =>
      val src = t"#!/bin/sh\necho 'hello from $label'\n"
      Payload(label, src.data, gzip = true)

    val bundleBytes: Data = PolyglotInstaller.bundle(payloads)

    def stage(): (Path on Linux, Path on Linux) =
      val dir: Path on Linux = temporaryDirectory[Path on Linux] / Uuid().show
      dir.create[Directory]()
      val script = dir / t"hello"
      script.create[File]()
      script.open: handle =>
        Stream(bundleBytes).writeTo(handle)
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
        val workDir: Path on Linux = temporaryDirectory[Path on Linux] / Uuid().show
        workDir.create[Directory]()

        val compilePs = workDir / t"compile.ps1"
        compilePs.create[File]()
        val psContent = t"""$$src = @'
class Hello { static void Main() { System.Console.WriteLine("hello from windows-arm64"); } }
'@
Add-Type -TypeDefinition $$src -OutputAssembly ziggurat-test-hello.exe -OutputType ConsoleApplication
"""
        compilePs.open: handle =>
          Stream(psContent.data).writeTo(handle)

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
          val winBundle = PolyglotInstaller.bundle:
            payloads :+ Payload(t"windows-arm64", winArm64Bytes, gzip = false)

          def stageAndCopy(extension: Text): Text =
            val script = workDir / t"hello-${Uuid().show}.$extension"
            script.create[File]()
            script.open: handle =>
              Stream(winBundle).writeTo(handle)
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
