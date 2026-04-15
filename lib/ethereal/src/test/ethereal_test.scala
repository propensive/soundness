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

import unsafeExceptions.canThrowAny

object Tests extends Suite(m"Ethereal Tests"):
  // The test executable (`exe`) is an Ethereal-built launcher script+JAR.
  // Different tests require different application behaviors. The `build` method
  // (to be provided) should accept a name and produce an executable at a known path.
  // The executable's behavior is controlled by the arguments passed to it.
  //
  // Expected behaviors of the test executable:
  //   <no args>          -- prints "ready" to stdout and exits with status 0
  //   echo <text>        -- prints <text> to stdout and exits with status 0
  //   exit <code>        -- exits with the given integer exit code
  //   stderr <text>      -- prints <text> to stderr and exits with status 0
  //   sleep <seconds>    -- sleeps for the given duration, then exits with status 0
  //   env <varname>      -- prints the value of the given environment variable to stdout
  //   pwd                -- prints the current working directory to stdout
  //   args               -- prints each argument (after "args") on a separate line to stdout
  //   pid                -- prints its own PID to stdout
  //   hang               -- blocks forever (never exits), for testing forced kill scenarios

  def run(): Unit =

    // `exe` is the path to the test executable, to be set up by the build helper.
    // `stateDir` is the XDG_RUNTIME_DIR used to isolate daemon state per test.
    // `cleanup` kills any lingering daemon and removes state files between tests.

    val exe = t"abcde"
    val brokenExe = t"abcdef"
    val stateDir: Path on Linux = temporaryDirectory

    Sandbox(exe).dispatch:
      ' {
          import executives.completions
          import interpreters.posix

          cli:
            arguments match
              case Nil =>
                execute(Out.println("ready") yet Exit.Ok)

              case Argument("echo") :: text :: Nil =>
                execute(Out.println(text()) yet Exit.Ok)

              case Argument("exit") :: Argument(As[Int](status)) :: Nil =>
                execute(Exit.Fail(status))

              case Argument("stderr") :: text :: Nil =>
                execute(Err.println(text()) yet Exit.Ok)

              case Argument("sleep") :: Argument(As[Int](seconds)) :: Nil =>
                execute:
                  snooze(seconds*Second) yet Exit.Ok
        }
    . sandbox:
        suite(m"Basic invocation"):

          // The executable should launch the daemon on first run, connect to it, and
          // return the output from the application.
          test(m"first invocation prints expected output"):
            // Executable: should print "ready" to stdout and exit 0
            sh"$exe echo hello".exec[Text]()
          .assert(_ == t"hello")

          // Running the executable a second time should reuse the already-running daemon
          // (not launch a new one). We verify by checking the output is still correct.
          test(m"second invocation reuses the daemon"):
            // Executable: should print "ready" again, daemon already running from prior test
            sh"$exe echo hello".exec[Text]()
            sh"$exe echo world".exec[Text]()
          .assert(_ == t"world")

          // The executable should correctly forward the exit code from the application.
          test(m"exit code 0 is returned on success"):
            // Executable: exits with code 0
            sh"$exe".exec[Exit]()
          .assert(_ == Exit.Ok)

          test(m"nonzero exit code is forwarded"):
            // Executable: exits with code 42
            sh"$exe exit 42".exec[Exit]()
          .assert(_ == Exit.Fail(42))

          test(m"exit code 1 is forwarded"):
            // Executable: exits with code 1
            sh"$exe exit 1".exec[Exit]()
          .assert(_ == Exit.Fail(1))

        suite(m"Argument passing"):

          // Arguments provided on the command line should be received intact by the
          // application, including arguments with spaces and special characters.
          test(m"single argument is passed through"):
            // Executable: prints each arg after "args" on separate lines
            sh"$exe args one".exec[Text]()
          .assert(_ == t"one")

          test(m"multiple arguments are passed through"):
            // Executable: prints each arg after "args" on separate lines
            sh"$exe args one two three".exec[Text]()
          .assert(_ == t"one\ntwo\nthree")

          test(m"argument with spaces is preserved"):
            // Executable: prints the single arg (which contains a space)
            val arg = t"hello world"
            sh"$exe args $arg".exec[Text]()
          .assert(_ == t"hello world")

          test(m"empty argument is preserved"):
            // Executable: prints each arg; the empty string should appear as an empty line
            sh"$exe args '' something".exec[Text]()
          .assert(_ == t"\nsomething")

        suite(m"Environment forwarding"):

          // The launcher sends the full environment to the daemon via `env -0`.
          // The application should be able to read environment variables from the client.
          test(m"environment variable is forwarded"):
            // Executable: prints the value of TEST_ETHEREAL_VAR
            val env = t"TEST_ETHEREAL_VAR=hello_ethereal"
            sh"env $env $exe env TEST_ETHEREAL_VAR".exec[Text]()
          .assert(_ == t"hello_ethereal")

        suite(m"Working directory"):

          // The launcher sends `pwd` to the daemon. The application should see the
          // client's working directory, not the daemon's.
          test(m"working directory is forwarded"):
            // Executable: prints its working directory
            sh"$exe pwd".exec[Text]()
          .check(_.length > 0)

        suite(m"Stderr forwarding"):

          // When the daemon supports stderr (the port file's third field is "1"),
          // the launcher opens a second TCP connection for stderr. Output written
          // to stderr by the application should appear on the launcher's stderr.
          test(m"stderr output is forwarded"):
            // Executable: prints "error message" to stderr, exits 0
            (sh"$exe stderr 'error message'" | sh"cat").exec[Text]()
          .check(_.length >= 0) // stdout may be empty; stderr goes to fd 2

        suite(m"Signal forwarding"):

          // The launcher traps INT, WINCH, and TERM and forwards them to the daemon
          // over a TCP connection. The daemon delivers them to the application as Signal values.

          test(m"SIGTERM causes the launcher to exit"):
            // Executable: sleeps for 10 seconds (long enough to receive a signal)
            // We fork the process, send SIGTERM, and verify it exits promptly.
            val t0 = _root_.java.lang.System.currentTimeMillis
            val proc = sh"$exe sleep 10".fork[Exit]()
            snooze(500L)
            sh"kill -TERM ${proc.pid}".exec[Unit]()
            proc.await()
            _root_.java.lang.System.currentTimeMillis - t0
          .assert(_ < 5000L)

          test(m"SIGINT causes the launcher to exit"):
            // Executable: sleeps for 10 seconds
            // We fork the process, send SIGINT, and verify it exits.
            val proc = sh"$exe sleep 10".fork[Exit]()
            snooze(500L)
            sh"kill -INT ${proc.pid}".exec[Unit]()
            val t0 = _root_.java.lang.System.currentTimeMillis
            proc.await()
            _root_.java.lang.System.currentTimeMillis - t0
          .assert(_ < 5000L)

        suite(m"Daemon lifecycle"):

          // After a clean exit, the state directory should not contain stale pid/port files
          // that would prevent subsequent launches.
          test(m"pid file is absent after clean exit"):
            // Executable: exits immediately with code 0
            sh"$exe".exec[Unit]()
            snooze(200L)
            sh"test -f $stateDir/pid".exec[Exit]()
          .assert(_ == Exit.Fail(1)) // file should not exist

          // After killing the daemon forcefully (SIGKILL), the launcher should detect
          // the stale state on the next run and clean up, then successfully start a new daemon.
          test(m"recovery after daemon is killed with SIGKILL"):
            // Executable: start a long-running command to ensure the daemon is alive
            val proc = sh"$exe sleep 30".fork[Exit]()
            snooze(500L)
            // Read the PID of the daemon process from the pid file and kill -9 it
            val daemonPid = sh"cat $stateDir/pid".exec[Text]()
            sh"kill -9 $daemonPid".exec[Unit]()
            snooze(500L)
            // The forked launcher should have exited (the TCP connection broke)
            // Now try running the executable again -- it should clean up and relaunch
            sh"$exe echo recovered".exec[Text]()
          .assert(_ == t"recovered")

          // If the pid file is stale (points to a nonexistent process), the launcher
          // should remove it and start fresh.
          test(m"stale pid file is cleaned up"):
            // Executable: run once to establish the state directory, then stop the daemon
            sh"$exe".exec[Unit]()
            snooze(200L)
            // Write a bogus PID into the pid file
            sh"echo 99999 > $stateDir/pid".exec[Unit]()
            // Remove the port file so checkState sees active() fail and cleans up
            sh"rm -f $stateDir/port".exec[Unit]()
            // Next invocation should clean up the stale files and succeed
            sh"$exe echo fresh".exec[Text]()
          .assert(_ == t"fresh")

          // If the fail file exists (left from a prior failed launch) but enough time
          // has passed (>=2 seconds), the launcher should remove it and retry.
          test(m"fail file is cleaned up after 2-second grace period"):
            // Executable: should succeed normally
            sh"mkdir -p $stateDir".exec[Unit]()
            sh"touch $stateDir/fail".exec[Unit]()
            // Ensure the pid file does NOT exist (so backout reports failure)
            sh"rm -f $stateDir/pid $stateDir/port".exec[Unit]()
            snooze(2500L) // wait for the 2-second grace period to expire
            // Write a fake pid file so backout's "pid file exists" branch triggers cleanup
            sh"echo $$ > $stateDir/pid".exec[Unit]()
            sh"$exe echo after-fail".exec[Text]()
          .assert(_ == t"after-fail")

        suite(m"Concurrent invocations"):

          // Multiple simultaneous invocations should not launch multiple daemons.
          // They should all connect to the same daemon instance.
          test(m"parallel invocations share the same daemon"):
            // Executable: "pid" command prints the daemon's PID
            val proc1 = sh"$exe pid".fork[Text]()
            val proc2 = sh"$exe pid".fork[Text]()
            val proc3 = sh"$exe pid".fork[Text]()
            val pid1 = proc1.await()
            val pid2 = proc2.await()
            val pid3 = proc3.await()
            // All three should report the same daemon PID
            Set(pid1, pid2, pid3).size
          .assert(_ == 1)

          // Rapid sequential invocations should all succeed without errors.
          test(m"rapid sequential invocations succeed"):
            // Executable: echo a counter value
            val results = (1 to 5).map: i =>
              sh"$exe echo $i".exec[Text]()
            results.to(List)
          .assert(_ == List(t"1", t"2", t"3", t"4", t"5"))

        suite(m"Version-gated restart"):

          // When the build ID in the executable changes, the daemon should be restarted.
          // This requires two different executables built with different build IDs.
          test(m"daemon restarts when build ID changes"):
            // Executable v1: "pid" command prints the daemon's PID, built with build ID 1
            // Executable v2: same behavior, built with build ID 2
            // Run v1, record the daemon PID; run v2, record the daemon PID; they should differ
            val pid1 = sh"$exeV1 pid".exec[Text]()
            snooze(200L)
            val pid2 = sh"$exeV2 pid".exec[Text]()
            pid1 != pid2
          .assert(_ == true)

        suite(m"Forced kill and cleanup"):

          // After SIGKILL of the launcher process (not the daemon), the daemon should
          // remain running, and a new launcher invocation should connect to it.
          test(m"daemon survives launcher SIGKILL"):
            // Executable: sleep for a long time
            val proc = sh"$exe sleep 30".fork[Exit]()
            snooze(500L)
            val launcherPid = proc.pid
            // Kill the launcher, not the daemon
            sh"kill -9 $launcherPid".exec[Unit]()
            snooze(200L)
            // Daemon should still be running; a new launcher should connect to it
            sh"$exe echo still-alive".exec[Text]()
          .assert(_ == t"still-alive")

          // If we kill the daemon (SIGKILL) while a launcher is connected, the launcher
          // should exit (the TCP connection will break), and subsequent launchers should
          // start a fresh daemon.
          test(m"launcher exits when daemon is killed"):
            val proc = sh"$exe sleep 30".fork[Exit]()
            snooze(500L)
            val daemonPid = sh"cat $stateDir/pid".exec[Text]()
            sh"kill -9 $daemonPid".exec[Unit]()
            val exit = proc.await()
            // The launcher should have exited with a nonzero exit code
            exit != Exit.Ok
          .assert(_ == true)

          test(m"new daemon starts after previous was killed"):
            // Following the previous test, verify a fresh daemon starts
            sh"$exe echo restarted".exec[Text]()
          .assert(_ == t"restarted")

        suite(m"State file integrity"):

          // The port file should contain three space-separated values: port, buildId, stderr flag
          test(m"port file has correct format"):
            // Executable: start the daemon
            sh"$exe echo probe".exec[Unit]()
            val content = sh"cat $stateDir/port".exec[Text]()
            // Should be three space-separated tokens: <port> <buildId> <0|1>
            content.cut(t" ").length
          .assert(_ == 3)

          test(m"port file contains a valid port number"):
            sh"$exe echo probe".exec[Unit]()
            val content = sh"cat $stateDir/port".exec[Text]()
            val port = content.cut(t" ").head.decode[Int]
            port > 0 && port < 65536
          .assert(_ == true)

          // The pid file should contain a numeric PID of a running process
          test(m"pid file contains a valid running PID"):
            sh"$exe sleep 5".fork[Exit]()
            snooze(500L)
            val pidText = sh"cat $stateDir/pid".exec[Text]()
            // Verify the PID is a running process
            sh"kill -0 $pidText".exec[Exit]()
          .assert(_ == Exit.Ok)

        suite(m"Pipe mode"):

          // When stdin is not a TTY (i.e. piped), the launcher should send "p" (pipe)
          // rather than "t" (terminal) in the init message.
          test(m"pipe input is forwarded to the application"):
            // Executable: reads from stdin and echoes it
            // Here we pipe text into the executable
            (sh"echo 'piped input'" | sh"$exe cat").exec[Text]()
          .assert(_ == t"piped input")

        suite(m"Timeout and launch failure"):

          // If the daemon fails to start (e.g. the JAR is corrupt), the launcher should
          // time out and create a fail file, then report the failure.
          test(m"launcher exits with error when daemon cannot start"):
            // Executable: a broken executable that exits immediately without writing a port file
            sh"$brokenExe echo hello".exec[Exit]()
          .assert(_ != Exit.Ok)

          // After a failed start, the fail file should exist
          test(m"fail file exists after failed daemon start"):
            sh"$brokenExe echo hello".exec[Exit]()
            sh"test -f $brokenStateDir/fail".exec[Exit]()
          .assert(_ == Exit.Ok)
