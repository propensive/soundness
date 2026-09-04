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
package ethereal

import java.io as ji
import java.lang as jl
import java.util.concurrent as juc

import soundness.*

import backstops.silentBackstop
import charDecoders.utf8Decoder
import classloaders.threadContextClassloader
import environments.daemonClientEnvironment
import executives.completions
import interpreters.posixInterpreter
import systems.javaBaseSystem
import textSanitizers.strictSanitizer
import threading.platformThreading
import workingDirectories.systemWorkingDirectory

@main
def fixture(): Unit = cli:
  arguments match
    case Nil =>
      execute(Out.print(t"ready") yet Exit.Ok)

    case Argument("args") :: rest =>
      execute(Out.print(rest.map(_()).join(t"\n")) yet Exit.Ok)

    case Argument("lines") :: rest =>
      execute(Out.print(rest.map(_()).join(t"\n") + t"\n") yet Exit.Ok)

    case Argument("echo") :: text :: Nil =>
      execute(Out.print(text()) yet Exit.Ok)

    case Argument("exit") :: Argument(As[Int](status)) :: Nil =>
      execute(Exit.Fail(status))

    case Argument("stderr") :: text :: Nil =>
      execute(Err.println(text()) yet Exit.Ok)

    case Argument("sleep") :: Argument(As[Int](seconds)) :: Nil =>
      execute:
        Thread.sleep(seconds.toLong*1000L)
        Exit.Ok

    case Argument("env") :: Argument(variable) :: Nil =>
      execute:
        val value: Text = safely(Environment[Text](variable)).or(t"")
        Out.print(value) yet Exit.Ok

    case Argument("pid") :: Nil =>
      execute(Out.print(Process().pid.value.show) yet Exit.Ok)

    case Argument("pwd") :: Nil =>
      execute:
        val cwd: Text = safely(workingDirectory[Path on Local].encode).or:
          jl.System.getProperty("user.dir").nn.tt

        Out.print(cwd) yet Exit.Ok

    case Argument("cat") :: Nil =>
      execute:
        val reader = ji.BufferedReader(ji.InputStreamReader(summon[Stdio].in))
        val line: Text = reader.readLine().nn.tt
        Out.print(line) yet Exit.Ok

    case Argument("cooked") :: Nil =>
      execute:
        service.cooked:
          val reader = ji.BufferedReader(ji.InputStreamReader(summon[Stdio].in))
          val line: Text = reader.readLine().nn.tt
          Out.print(t"[$line]")

        Exit.Ok

    case Argument("version") :: Nil =>
      execute:
        val id: Text = safely(System.properties.build.id[Text]()).or:
          safely((Classpath/"build.id").read[Text].trim).or(t"unknown")

        Out.print(t"v$id") yet Exit.Ok

    case Argument("signal") :: Nil =>
      execute:
        val received: juc.LinkedBlockingQueue[Text] = juc.LinkedBlockingQueue()

        trap:
          case sig: UnixSignal =>
            received.offer(sig.shortName)
            SignalResponse.Accept

          case sig: WindowsSignal =>
            received.offer(sig.shortName)
            SignalResponse.Accept

        val raw: Text | Null = received.poll(2L, juc.TimeUnit.SECONDS)
        val text: Text = if raw == null then t"(timeout)" else raw
        Out.print(text) yet Exit.Ok

    case Argument("trap-reject") :: Nil =>
      execute:
        trap { case _: UnixSignal => SignalResponse.Reject }
        Thread.sleep(5000L)
        Exit.Ok

    case Argument("trap-defer") :: Nil =>
      execute:
        val received: juc.LinkedBlockingQueue[Text] = juc.LinkedBlockingQueue()

        trap:
          case Interrupt.Int =>
            received.offer(t"outer")
            SignalResponse.Accept

        trap { case _: UnixSignal => SignalResponse.Defer }

        val raw: Text | Null = received.poll(2L, juc.TimeUnit.SECONDS)
        val text: Text = if raw == null then t"(timeout)" else raw
        Out.print(text) yet Exit.Ok

    case Argument("trap-undefined") :: Nil =>
      execute:
        trap { case Interrupt.Winch => SignalResponse.Accept }
        Thread.sleep(5000L)
        Exit.Ok

    case Argument("trap-slow") :: Nil =>
      execute:
        trap:
          case _: UnixSignal =>
            Thread.sleep(2000L)
            SignalResponse.Accept

        Thread.sleep(5000L)
        Exit.Ok

    case _ =>
      execute(Exit.Fail(1))
