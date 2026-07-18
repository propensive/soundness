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
package exoskeleton

import ambience.*
import anticipation.*
import contingency.*
import denominative.*
import distillate.*
import escapade.*
import eucalyptus.*
import fulminate.*
import gossamer.*
import guillotine.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*

def execute(block: (erased effectful: Effectful) ?=> Invocation ?=> Exit)(using cli: Cli): Execution =
  cli.absolve match
    case completion: Completion => Execution(Exit.Ok)
    case invocation: Invocation => Execution(block(using !!)(using invocation))

def explain(explanation: (Optional[Text] aka "prior") ?=> Optional[Text])(using cli: Cli): Unit =
  cli.explain(explanation(using Unset.aka["prior"]))

// Build a structured `Help` tree for an application by re-running its pure portion in
// tab-completion mode with synthesized argument prefixes. In completion mode `execute` does no
// IO, so the block runs harmlessly; each run discovers the subcommands and flags reachable at
// one prefix, and the driver descends into every non-hidden subcommand to build the tree.
def helpTree
  ( command:          Text,
    environment:      Environment,
    workingDirectory: WorkingDirectory,
    stdio:            Stdio,
    login:            Login )
  ( block: Cli ?=> Execution )
  ( using interpreter: Interpreter )
:   Help =

  def probe(prefix: List[Text]): (List[Suggestion], List[Flag]) =
    val focus = prefix.length
    val textArguments = prefix :+ t""
    val synthesized = Cli.arguments(textArguments, focus, Unset, Prim)

    val completion =
      Completion
        ( synthesized,
          synthesized,
          environment,
          workingDirectory,
          Shell.Zsh,
          focus,
          Unset,
          stdio,
          t"",
          Prim,
          login )

    block(using completion)
    (completion.cursorSuggestions, completion.flags.keySet.to(List))

  def build
    ( prefix:      List[Text],
      command:     Text,
      description: Optional[Text | Teletype],
      seen:        Set[List[Text]] )
  :   Help =

    if seen.contains(prefix) then Help(command, description, Nil, Nil) else
      val (suggestions, flags) = probe(prefix)

      val parameters = flags.to(List).map: flag =>
        Help.Param
          ( Flag.serialize(flag.name),
            flag.aliases.map(Flag.serialize(_)),
            flag.description,
            flag.repeatable )

      val children =
        suggestions.distinctBy(_.core).flatMap: suggestion =>
          val childPrefix = prefix :+ suggestion.core

          if suggestion.hidden || suggestion.incomplete then Nil
          else List(build(childPrefix, suggestion.core, suggestion.description, seen + prefix))

      Help(command, description, parameters, children.sortBy(_.command))

  build(Nil, command, Unset, Set())

package executives:
  given completions: (backstop: Backstop) => Executive:
    type Interface = Cli
    type Return = Execution


    def invocation
      ( arguments:        Iterable[Text],
        environment:      Environment,
        workingDirectory: WorkingDirectory,
        stdio:            Stdio,
        entrypoint:       Entrypoint^,
        login:            Login )
      ( using interpreter: Interpreter )
    :   Cli =

      arguments match
        case
          t"{completions}" :: t"powershell" :: As.Int(cursor) :: _ :: tty ::
            t"--" ::
            rawLine ::
            Nil =>

          val parts0 = rawLine.cut(t" ")
          val parts = if cursor > rawLine.length then parts0 :+ t"" else parts0
          val wordStarts = parts.scanLeft(0){ (pos, w) => pos + w.length + 1 }.init
          val wordIdx = wordStarts.lastIndexWhere(_ <= cursor).max(0)
          val posInWord = cursor - wordStarts(wordIdx)
          val focus = (wordIdx - 1).max(0)
          val restParts = if parts.length > 1 then parts.tail else List(t"")
          val tab = Completions.tab(tty, Completions.Tab(arguments.to(List), focus, cursor))

          Completion
            ( Cli.arguments(arguments, focus, posInWord, tab),
              Cli.arguments(restParts, focus, posInWord, tab),
              environment,
              workingDirectory,
              Shell.Powershell,
              focus,
              posInWord,
              stdio,
              tty,
              tab,
              login )

        case
          t"{completions}" :: shellName :: As.Int(focus0) :: As.Int(position0) :: tty ::
            t"--" ::
            command ::
            rest =>

            val shell = shellName match
              case t"zsh"        => Shell.Zsh
              case t"fish"       => Shell.Fish
              case t"powershell" => Shell.Powershell
              case _             => Shell.Bash

            val focus1 =
              if shell == Shell.Bash && rest.lastOption == Some(t"=") then focus0 + 1 else focus0


            def read(todo: List[Text], flag: Boolean, done: List[Text]): List[Text] = todo match
              case Nil                                 => done.reverse
              case t"=" :: tail if shell == Shell.Bash => read(tail, false, done)

              case head :: tail =>
                read(tail, head.starts(t"--"), head :: done)

            val rest2 = read(rest.to(List), false, Nil)

            val focus = focus1 - (if shell == Shell.Zsh then 2 else 1)

            val position = if shell == Shell.Bash then Unset else position0
            val tab = Completions.tab(tty, Completions.Tab(arguments.to(List), focus, position0))
            val equalses = rest.take(focus0).count(_ == t"=")
            val focus2 = focus - (if shell == Shell.Bash then equalses else 0)

            Completion
              ( Cli.arguments(arguments, focus2, position, tab),
                Cli.arguments(rest2, focus2, position, tab),
                environment,
                workingDirectory,
                shell,
                focus2,
                position,
                stdio,
                tty,
                tab,
                login )

        case t"{admin}" :: command :: Nil =>
          given Stdio = stdio

          command match
            case t"pid"     => Out.println(Process().pid.value.show) yet Exit.Ok
            case t"kill"    => java.lang.System.exit(0) yet Exit.Ok

            case t"await" =>
              Cli.prepare()
              import parasite.threading.platformThreading
              safely(parasite.supervise(Cli.await())).or(Nil).map(Out.println(_))
              Exit.Ok

            case t"install" =>
              given entrypoint0: (Entrypoint^{entrypoint}) = entrypoint
              given WorkingDirectory = workingDirectory
              import errorDiagnostics.stackTracesDiagnostics
              import logging.silentLogging
              Out.println(Completions.ensure(force = true).join(t"\n"))
              Exit.Ok

            case _ =>
              Exit.Fail(1)

          Invocation
            ( Cli.arguments(arguments),
              environment,
              workingDirectory,
              stdio,
              false,
              login )

        case other =>
          Invocation
            ( Cli.arguments(arguments),
              environment,
              workingDirectory,
              stdio,
              true,
              login )


    def process(cli: Cli)(execution: Cli ?=> Execution): Exit = cli.absolve match
      case completion: Completion =>
        given Stdio = completion.stdio
        completion.serialize.each(Out.println(_))
        Cli.done()
        Exit.Ok

      case invocation: Invocation =>
        given Stdio = invocation.stdio

        try execution(using invocation).exitStatus
        catch case error: Throwable => backstop.handle(error)(using invocation.stdio)


    override def help
      ( command:          Text,
        environment:      Environment,
        workingDirectory: WorkingDirectory,
        stdio:            Stdio,
        login:            Login )
      ( block: Cli ?=> Execution )
      ( using interpreter: Interpreter )
    :   Optional[Help] =

      helpTree(command, environment, workingDirectory, stdio, login)(block)
