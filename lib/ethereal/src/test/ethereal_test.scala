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
package ethereal

import java.lang as jl
import java.io as ji

import soundness.*

import classloaders.system
import environments.java
import systems.java
import temporaryDirectories.system
import workingDirectories.default
import supervisors.global
import logging.silent
import threading.platform

import strategies.throwUnsafely
import backstops.silent
import autopsies.contrastExpectations
import daemonConfig.supportStderr
import homeDirectories.system

object Tests extends Suite(m"Ethereal Tests"):
  def run(): Unit =

    safely(sh"pkill abcde".exec[Exit]())
    snooze(0.1*Second)

    val stateDir: Path on Linux =
      Xdg.runtimeDir[Path on Linux].or(Xdg.stateHome[Path on Linux]) / t"abcde"

    safely:
      val oldPid = sh"cat $stateDir/pid".exec[Text]().trim.decode[Pid]
      Process(oldPid).abort()
    snooze(0.2*Second)
    sh"rm -f $stateDir/pid $stateDir/port $stateDir/fail".exec[Unit]()

    val launcher = Sandbox("abcde").dispatch:
      ' {
          import executives.completions
          import interpreters.posix
          import environments.daemonClient

          cli:
            arguments match
              case Nil =>
                execute(Out.print(t"ready") yet Exit.Ok)

              case Argument("args") :: arguments =>
                execute:
                  Out.print(arguments.map(_()).join(t"\n")) yet Exit.Ok

              case Argument("lines") :: arguments =>
                execute:
                  Out.print(arguments.map(_()).join(t"\n") + t"\n") yet Exit.Ok

              case Argument("echo") :: text :: Nil =>
                execute(Out.print(text()) yet Exit.Ok)

              case Argument("exit") :: Argument(As[Int](status)) :: Nil =>
                execute(Exit.Fail(status))

              case Argument("stderr") :: text :: Nil =>
                execute(Err.println(text()) yet Exit.Ok)

              case Argument("sleep") :: Argument(As[Int](seconds)) :: Nil =>
                execute:
                  snooze(seconds*Second) yet Exit.Ok

              case Argument("env") :: Argument(variable) :: Nil =>
                execute:
                  Out.print(Environment[Text](variable)) yet Exit.Ok

              case Argument("pid") :: Nil =>
                execute:
                  Out.print(Process().pid.value.show) yet Exit.Ok

              case Argument("pwd") :: Nil =>
                execute:
                  Out.print(workingDirectory[Path on Linux].encode) yet Exit.Ok

              case Argument("cat") :: Nil =>
                execute:
                  val reader = ji.BufferedReader(ji.InputStreamReader(summon[Stdio].in))
                  val line: Text = reader.readLine().nn.tt
                  Out.print(line) yet Exit.Ok

              case Argument("signal") :: Nil =>
                execute:
                  Out.print(summon[Invocation].signals.stream.head.shortName) yet Exit.Ok

              case _ =>
                execute(Exit.Fail(1))

          t"finished"
        }

    sh"rm -f $stateDir/fail".exec[Unit]()

    launcher.sandbox:
        val tool = summon[Sandbox.Tool].path
        suite(m"Basic invocation"):
          test(m"first invocation prints expected output"):
            sh"$tool echo hello".exec[Text]()
          .assert(_ == t"hello")

          test(m"second invocation reuses the daemon"):
            sh"$tool echo hello".exec[Text]()
            sh"$tool echo world".exec[Text]()
          .assert(_ == t"world")

          test(m"exit code 0 is returned on success"):
            sh"$tool".exec[Exit]()
          .assert(_ == Exit.Ok)

          test(m"nonzero exit code is forwarded"):
            sh"$tool exit 42".exec[Exit]()
          .assert(_ == Exit.Fail(42))

          test(m"exit code 1 is forwarded"):
            sh"$tool exit 1".exec[Exit]()
          .assert(_ == Exit.Fail(1))

        suite(m"Argument passing"):
          test(m"single argument is passed through"):
            sh"$tool args one".exec[Text]()
          .assert(_ == t"one")

          test(m"multiple arguments are passed through"):
            sh"$tool args one two three".exec[Text]()
          .assert(_ == t"one\ntwo\nthree")

          test(m"argument with spaces is preserved"):
            val arg = t"hello world"
            sh"$tool args $arg".exec[Text]()
          .assert(_ == t"hello world")

          test(m"empty argument is preserved"):
            sh"$tool args '' something".exec[Text]()
          .assert(_ == t"\nsomething")

          test(m"trailing newline in output is preserved"):
            sh"$tool lines one two three".exec[Text]()
          .assert(_ == t"one\ntwo\nthree\n")

        suite(m"Environment forwarding"):
          test(m"environment variable is forwarded"):
            val env = t"TEST_ETHEREAL_VAR=hello_ethereal"
            sh"env $env $tool env TEST_ETHEREAL_VAR".exec[Text]()
          .assert(_ == t"hello_ethereal")

        suite(m"Working directory"):
          test(m"working directory is forwarded"):
            sh"$tool pwd".exec[Text]()
          .check(_.length > 0)

        suite(m"Stderr forwarding"):
          test(m"stderr output is forwarded"):
            sh"$tool stderr 'error message'".exec[Stderr]().text.trim
          .assert(_ == t"error message")

        suite(m"Signal forwarding"):
          test(m"SIGTERM causes the launcher to exit"):
            val t0 = jl.System.currentTimeMillis
            val proc = sh"$tool sleep 1".fork[Exit]()
            snooze(0.1*Second)
            sh"kill -TERM ${proc.pid.value}".exec[Unit]()
            proc.await()
            jl.System.currentTimeMillis - t0
          .assert(_ < 750L)

          test(m"SIGWINCH is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.1*Second)
            sh"kill -WINCH ${proc.pid.value}".exec[Unit]()
            proc.await()
          . assert(_ == t"WINCH")

          test(m"SIGUSR1 is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.1*Second)
            sh"kill -USR1 ${proc.pid.value}".exec[Unit]()
            proc.await()

          . assert(_ == t"USR1")

          test(m"SIGUSR2 is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.1*Second)
            sh"kill -USR2 ${proc.pid.value}".exec[Unit]()
            proc.await()

          . assert(_ == t"USR2")

          test(m"SIGHUP is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.1*Second)
            sh"kill -HUP ${proc.pid.value}".exec[Unit]()
            proc.await()

          . assert(_ == t"HUP")

          test(m"SIGINT is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.1*Second)
            sh"kill -INT ${proc.pid.value}".exec[Unit]()
            proc.await()

          . assert(_ == t"INT")

        suite(m"State file monitoring"):
          test(m"daemon restarts after pid file is deleted"):
            val oldPid = sh"$tool '{admin}' pid".exec[Text]().trim
            sh"rm -f $stateDir/pid".exec[Unit]()
            snooze(0.1*Second)
            val newPid = sh"$tool '{admin}' pid".fork[Text]().await().trim
            newPid != oldPid

          . assert(_ == true)

          test(m"daemon restarts after port file is deleted"):
            val oldPid = sh"$tool '{admin}' pid".exec[Text]().trim.decode[Pid]
            sh"rm -f $stateDir/port".exec[Unit]()
            val deadline = jl.System.currentTimeMillis + 10000
            while safely(Process(oldPid).alive).or(false) && jl.System.currentTimeMillis < deadline
            do snooze(0.05*Second)
            sh"rm -f $stateDir/fail".exec[Unit]()
            val newPid = sh"$tool '{admin}' pid".exec[Text]().trim.decode[Pid]
            newPid != oldPid

          . assert(_ == true)

        suite(m"Daemon lifecycle"):
          test(m"pid file is present while daemon is running"):
            sh"$tool".exec[Unit]()
            sh"test -f $stateDir/pid".exec[Exit]()

          . assert(_ == Exit.Ok) // daemon persists between invocations

          test(m"recovery after daemon is killed with SIGKILL"):
            sh"rm -f $stateDir/fail".exec[Unit]()
            val pid = sh"$tool '{admin}' pid".exec[Text]().trim.decode[Pid]
            Process(pid).abort()
            snooze(0.1*Second)
            sh"$tool echo recovered".exec[Text]()

          . assert(_ == t"recovered")

          test(m"stale pid file is cleaned up"):
            sh"$tool echo pretest".exec[Text]()
            val daemonPid = sh"cat $stateDir/pid".exec[Text]().trim.decode[Pid]
            Process(daemonPid).abort()
            val deadline = jl.System.currentTimeMillis + 3000
            while safely(Process(daemonPid).alive).or(false)
              && jl.System.currentTimeMillis < deadline
            do snooze(0.05*Second)
            sh"rm -f $stateDir/fail".exec[Unit]()
            sh"$tool echo fresh".exec[Text]()

          . assert(_ == t"fresh")

          test(m"fail file is removed after 2 seconds"):
            sh"mkdir -p $stateDir".exec[Unit]()
            sh"touch $stateDir/fail".exec[Unit]()
            sh"rm -f $stateDir/pid $stateDir/port".exec[Unit]()
            snooze(2.5*Second)
            sh"$tool echo after-fail".exec[Text]()

          . assert(_ == t"after-fail")

        suite(m"Concurrent invocations"):
          test(m"parallel invocations share the same daemon"):
            val proc1 = sh"$tool pid".fork[Text]()
            val proc2 = sh"$tool pid".fork[Text]()
            val proc3 = sh"$tool pid".fork[Text]()
            val pid1 = proc1.await()
            val pid2 = proc2.await()
            val pid3 = proc3.await()
            Set(pid1, pid2, pid3).size

          . assert(_ == 1)

          test(m"rapid sequential invocations succeed"):
            val results = (1 to 5).map: i =>
              sh"$tool echo $i".exec[Text]()
            results.to(List).map(_.trim)

          . assert(_ == List(t"1", t"2", t"3", t"4", t"5"))

        suite(m"Forced kill and cleanup"):
          test(m"daemon survives launcher SIGKILL"):
            val proc = sh"$tool sleep 30".fork[Exit]()
            snooze(0.1*Second)
            val launcherPid = proc.pid.value
            sh"kill -9 $launcherPid".exec[Unit]()
            snooze(0.1*Second)
            sh"$tool echo still-alive".exec[Text]()

          . assert(_ == t"still-alive")

          test(m"launcher exits when daemon is killed"):
            val proc = sh"$tool sleep 30".fork[Exit]()
            snooze(0.1*Second)
            val pid = sh"$tool '{admin}' pid".exec[Text]().trim
            sh"kill -9 $pid".exec[Unit]()
            val exit = proc.await()
            exit != Exit.Ok

          . assert(_ == true)

          test(m"new daemon starts after previous was killed"):
            sh"$tool echo restarted".exec[Text]()

          . assert(_ == t"restarted")

        suite(m"State file integrity"):
          test(m"port file has correct format"):
            sh"$tool echo probe".exec[Unit]()
            val content = sh"cat $stateDir/port".exec[Text]()
            content.cut(t" ").length

          . assert(_ == 3)

          test(m"port file contains a valid port number"):
            sh"$tool echo probe".exec[Unit]()
            val content = sh"cat $stateDir/port".exec[Text]()
            val port = content.cut(t" ").head.decode[Int]
            port > 0 && port < 65536

          . assert(_ == true)

          test(m"pid file contains a valid running PID"):
            sh"$tool".exec[Unit]()
            val pid = sh"$tool '{admin}' pid".exec[Text]().trim.decode[Pid]
            safely(Process(pid).alive).or(false)

          . assert(_ == true)

        suite(m"Pipe mode"):
          test(m"pipe input is forwarded to the application"):
            (sh"echo 'piped input'" | sh"$tool cat").exec[Text]()

          . assert(_ == t"piped input")

    val upgradeStateDir: Path on Linux =
      Xdg.runtimeDir[Path on Linux].or(Xdg.stateHome[Path on Linux]) / t"upgrd"

    sh"rm -f $upgradeStateDir/pid $upgradeStateDir/port $upgradeStateDir/fail".exec[Unit]()
    safely(sh"pkill upgrd".exec[Exit]())
    snooze(0.2*Second)

    val launcherV1 = Sandbox("upgrd", buildId = 1).dispatch:
      ' {
          import executives.completions
          import interpreters.posix
          import environments.daemonClient

          cli:
            arguments match
              case Argument("version") :: Nil =>
                execute(Out.print(t"v1") yet Exit.Ok)

              case _ =>
                execute(Exit.Fail(1))

          t"finished"
        }

    val toolV1 = launcherV1.path

    val launcherV2 = Sandbox("upgrd", buildId = 2).dispatch:
      ' {
          import executives.completions
          import interpreters.posix
          import environments.daemonClient

          cli:
            arguments match
              case Argument("version") :: Nil =>
                execute(Out.print(t"v2") yet Exit.Ok)

              case _ =>
                execute(Exit.Fail(1))

          t"finished"
        }

    val toolV2 = launcherV2.path

    suite(m"Daemon upgrade"):
      test(m"v1 daemon starts and returns v1 output"):
        sh"$toolV1 version".exec[Text]()
      .assert(_ == t"v1")

      test(m"v1 daemon is still running before upgrade"):
        sh"$toolV1 version".exec[Text]()
      .assert(_ == t"v1")

      test(m"v2 launcher replaces v1 daemon and returns v2 output"):
        val v1Pid = sh"$toolV1 '{admin}' pid".exec[Text]().trim.decode[Pid]
        sh"$toolV2 version".exec[Text]()
      .assert(_ == t"v2")

      test(m"v1 daemon is no longer running after upgrade"):
        val v1Pid =
          safely(sh"cat $upgradeStateDir/pid".exec[Text]().trim.decode[Pid]).or(Pid(0))
        val v2Output = sh"$toolV2 version".exec[Text]()
        v2Output == t"v2"
      .assert(_ == true)

    safely(sh"$toolV2 '{admin}' kill".exec[Exit]())
    snooze(0.2*Second)
    safely(sh"pkill upgrd".exec[Exit]())
    sh"rm -rf $upgradeStateDir".exec[Unit]()

    val brokenStateDir: Path on Linux =
      Xdg.runtimeDir[Path on Linux].or(Xdg.stateHome[Path on Linux]) / t"brokn"

    val brokenExe: Path on Linux = Sandbox("brokn").dispatch:
      ' {
          import executives.completions
          import interpreters.posix
          import environments.daemonClient

          if jl.System.getProperty("ethereal.name") != null then jl.System.exit(1)

          cli:
            arguments match
              case _ => execute(Exit.Ok)

          t"unreachable"
        }
    . path

    suite(m"Timeout and launch failure"):

      test(m"launcher exits with error when daemon cannot start"):
        sh"rm -f $brokenStateDir/fail".exec[Unit]()
        safely(sh"$brokenExe echo hello".exec[Exit]()).or(Exit.Fail(1))

      . assert(_ != Exit.Ok)

      test(m"fail file exists after failed daemon start"):
        sh"rm -f $brokenStateDir/fail".exec[Unit]()
        safely(sh"$brokenExe echo hello".exec[Exit]())
        sh"test -f $brokenStateDir/fail".exec[Exit]()

      . assert(_ == Exit.Ok)

    sh"rm -rf $brokenStateDir".exec[Unit]()
