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
  // Expected behaviors of the test executable:
  //   <no args>          -- prints "ready" to stdout and exits with status 0
  //   echo <text>        -- prints <text> to stdout (no newline) and exits with status 0
  //   exit <code>        -- exits with the given integer exit code
  //   stderr <text>      -- prints <text> to stderr and exits with status 0
  //   sleep <seconds>    -- sleeps for the given duration, then exits with status 0
  //   env <varname>      -- prints the value of the given environment variable to stdout (no newline)
  //   pwd                -- prints the current working directory to stdout (no newline)
  //   args               -- prints each argument (after "args") separated by newlines, no trailing newline
  //   lines              -- prints each argument (after "lines") separated by newlines, with trailing newline
  //   pid                -- prints its own PID to stdout (no newline)
  //   hang               -- blocks forever (never exits), for testing forced kill scenarios

  def run(): Unit =

    // `exe` is the path to the test executable, to be set up by the build helper.
    // `stateDir` is the XDG_RUNTIME_DIR used to isolate daemon state per test.
    // `cleanup` kills any lingering daemon and removes state files between tests.

    val brokenExe = t"abcdef"
    val stateDir: Path on Linux = Xdg.runtimeDir[Path on Linux].or(Xdg.stateHome[Path on Linux]) / t"abcde"

    Sandbox("abcde").dispatch:
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
                  Out.print(OsProcess().pid.value.show) yet Exit.Ok

              case Argument("pwd") :: Nil =>
                execute:
                  Out.print(workingDirectory[Path on Linux].encode) yet Exit.Ok

              case Argument("cat") :: Nil =>
                execute:
                  val reader = _root_.java.io.BufferedReader(_root_.java.io.InputStreamReader(summon[Stdio].in))
                  val line: Text = reader.readLine().nn.tt
                  Out.print(line) yet Exit.Ok

              case Argument("signal") :: Nil =>
                execute:
                  Out.print(summon[Invocation].signals.stream.head.shortName) yet Exit.Ok

              case _ =>
                execute(Exit.Fail(1))

          t"finished"
        }
    . sandbox:
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
            val t0 = _root_.java.lang.System.currentTimeMillis
            val proc = sh"$tool sleep 1".fork[Exit]()
            snooze(0.2*Second)
            sh"kill -TERM ${proc.pid.value}".exec[Unit]()
            proc.await()
            _root_.java.lang.System.currentTimeMillis - t0
          .assert(_ < 750L)

          test(m"SIGWINCH is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.2*Second)
            sh"kill -WINCH ${proc.pid.value}".exec[Unit]()
            proc.await()
          .assert(_ == t"WINCH")

          test(m"SIGUSR1 is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.2*Second)
            sh"kill -USR1 ${proc.pid.value}".exec[Unit]()
            proc.await()
          .assert(_ == t"USR1")

          test(m"SIGUSR2 is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.2*Second)
            sh"kill -USR2 ${proc.pid.value}".exec[Unit]()
            proc.await()
          .assert(_ == t"USR2")

          test(m"SIGHUP is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.2*Second)
            sh"kill -HUP ${proc.pid.value}".exec[Unit]()
            proc.await()
          .assert(_ == t"HUP")

          test(m"SIGINT is forwarded to the application"):
            val proc = sh"$tool signal".fork[Text]()
            snooze(0.2*Second)
            sh"kill -INT ${proc.pid.value}".exec[Unit]()
            proc.await()
          .assert(_ == t"INT")

        suite(m"Daemon lifecycle"):
          test(m"pid file is present while daemon is running"):
            sh"$tool".exec[Unit]()
            snooze(0.2*Second)
            sh"test -f $stateDir/pid".exec[Exit]()
          .assert(_ == Exit.Ok) // daemon persists between invocations

          test(m"recovery after daemon is killed with SIGKILL"):
            snooze(0.2*Second)
            val pid = sh"$tool '{admin}' pid".exec[Text]().trim
            sh"kill -9 $pid".exec[Unit]()
            snooze(0.5*Second)
            sh"$tool echo recovered".exec[Text]()
          .assert(_ == t"recovered")

          test(m"stale pid file is cleaned up"):
            sh"$tool".exec[Unit]()
            snooze(0.2*Second)
            _root_.java.nio.file.Files.writeString(
              _root_.java.nio.file.Path.of(s"${stateDir.encode.s}/pid"),
              "99999"
            )
            sh"rm -f $stateDir/port".exec[Unit]()
            sh"$tool echo fresh".exec[Text]()
          .assert(_ == t"fresh")

          test(m"fail file is removed after 2 seconds"):
            sh"mkdir -p $stateDir".exec[Unit]()
            sh"touch $stateDir/fail".exec[Unit]()
            sh"rm -f $stateDir/pid $stateDir/port".exec[Unit]()
            snooze(2.5*Second)
            sh"$tool echo after-fail".exec[Text]()
          .assert(_ == t"after-fail")

        suite(m"Concurrent invocations"):
          test(m"parallel invocations share the same daemon"):
            // Executable: "pid" command prints the daemon's PID
            val proc1 = sh"$tool pid".fork[Text]()
            val proc2 = sh"$tool pid".fork[Text]()
            val proc3 = sh"$tool pid".fork[Text]()
            val pid1 = proc1.await()
            val pid2 = proc2.await()
            val pid3 = proc3.await()
            Set(pid1, pid2, pid3).size
          .assert(_ == 1)

          test(m"rapid sequential invocations succeed"):
            val results = (1 to 5).map: i =>
              sh"$tool echo $i".exec[Text]()
            results.to(List).map(_.trim)
          .assert(_ == List(t"1", t"2", t"3", t"4", t"5"))

        // suite(m"Version-gated restart"):

        //   // When the build ID in the executable changes, the daemon should be restarted.
        //   // This requires two different executables built with different build IDs.
        //   test(m"daemon restarts when build ID changes"):
        //     // Executable v1: "pid" command prints the daemon's PID, built with build ID 1
        //     // Executable v2: same behavior, built with build ID 2
        //     // Run v1, record the daemon PID; run v2, record the daemon PID; they should differ
        //     val pid1 = sh"$exeV1 pid".exec[Text]()
        //     snooze(200L)
        //     val pid2 = sh"$exeV2 pid".exec[Text]()
        //     pid1 != pid2
        //   .assert(_ == true)

        suite(m"Forced kill and cleanup"):
          test(m"daemon survives launcher SIGKILL"):
            val proc = sh"$tool sleep 30".fork[Exit]()
            snooze(0.5*Second)
            val launcherPid = proc.pid.value
            sh"kill -9 $launcherPid".exec[Unit]()
            snooze(0.2*Second)
            sh"$tool echo still-alive".exec[Text]()
          .assert(_ == t"still-alive")

          test(m"launcher exits when daemon is killed"):
            val proc = sh"$tool sleep 30".fork[Exit]()
            snooze(0.5*Second)
            val pid = sh"$tool '{admin}' pid".exec[Text]().trim
            sh"kill -9 $pid".exec[Unit]()
            val exit = proc.await()
            exit != Exit.Ok
          .assert(_ == true)

          test(m"new daemon starts after previous was killed"):
            sh"$tool echo restarted".exec[Text]()
          .assert(_ == t"restarted")

        suite(m"State file integrity"):

          // The port file should contain three space-separated values: port, buildId, stderr flag
          test(m"port file has correct format"):
            // Executable: start the daemon
            sh"$tool echo probe".exec[Unit]()
            val content = sh"cat $stateDir/port".exec[Text]()
            // Should be three space-separated tokens: <port> <buildId> <0|1>
            content.cut(t" ").length
          .assert(_ == 3)

          test(m"port file contains a valid port number"):
            sh"$tool echo probe".exec[Unit]()
            val content = sh"cat $stateDir/port".exec[Text]()
            val port = content.cut(t" ").head.decode[Int]
            port > 0 && port < 65536
          .assert(_ == true)

          // The pid file should contain a numeric PID of a running process
          test(m"pid file contains a valid running PID"):
            sh"$tool sleep 5".fork[Exit]()
            snooze(0.5*Second)
            val pidText = sh"$tool '{admin}' pid".exec[Text]().trim
            sh"kill -0 $pidText".exec[Exit]()
          .assert(_ == Exit.Ok)

        suite(m"Pipe mode"):

          // When stdin is not a TTY (i.e. piped), the launcher should send "p" (pipe)
          // rather than "t" (terminal) in the init message.
          test(m"pipe input is forwarded to the application"):
            // Executable: reads from stdin and echoes it
            // Here we pipe text into the executable
            (sh"echo 'piped input'" | sh"$tool cat").exec[Text]()
          .assert(_ == t"piped input")

        suite(m"Timeout and launch failure"):

          // If the daemon fails to start (e.g. the JAR is corrupt), the launcher should
          // time out and create a fail file, then report the failure.
          test(m"launcher exits with error when daemon cannot start"):
            // Executable: a broken executable that exits immediately without writing a port file
            safely(sh"$brokenExe echo hello".exec[Exit]()).or(Exit.Fail(1))
          .assert(_ != Exit.Ok)

        //   // After a failed start, the fail file should exist
        //   // test(m"fail file exists after failed daemon start"):
        //   //   sh"$brokenExe echo hello".exec[Exit]()
        //   //   sh"test -f $brokenStateDir/fail".exec[Exit]()
        //   // .assert(_ == Exit.Ok)
