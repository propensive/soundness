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
package exoskeleton

import proscenium.compat.*

import scala.collection.mutable as scm

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
import prepositional.*
import rudiments.*
import spectacular.*
import turbulence.*
import vacuous.*
import denominative.asymptotics.linearSizeComplexity

// The block's `result` type is the union of the singleton types of every `Status` it can
// return, accumulated through the contravariant `Status.Registry` capability and kept precise
// (rather than widened to `Status`) by the `Precise` bound. Those statuses are recorded on the
// `Cli` even in completion mode — where the block itself is never run — so that a help-tree
// probe can discover them for documentation.
// `scala.Precise`, not the `Precise` which `-Yimports:proscenium` supplies: the prelude's
// `export scala.Precise` yields a distinct alias symbol which the compiler's union-widening
// suppression does not recognise, so the bound would silently do nothing and the union of
// statuses would be widened to `Status` (soundness#1811).
def execute[result: scala.Precise]
   ( block: (erased effectful: Effectful) ?=> Status.Registry[result] ?=> Invocation ?=> Exit )
   ( using cli: Cli )
   ( using admissible: result is Status.Admissible )
:   Execution to result =

  val statuses = admissible.statuses
  cli.record(statuses)

  cli.absolve match
    case completion: Completion => Execution.of[result](Exit.Ok, statuses)

    case invocation: Invocation =>
      val registry = new Status.Registry[result] {}
      Execution.of[result](block(using !!)(using registry)(using invocation), statuses)

def explain(explanation: (Optional[Text] aka "prior") ?=> Optional[Text])(using cli: Cli): Unit =
  cli.explain(explanation(using Unset.aka["prior"]))

// Everything one run of the application's pure portion, at one argument prefix, reveals about
// its interface.
case class Probe
  ( suggestions: List[Suggestion],
    flags:       List[Flag],
    globals:     Set[Flag],
    operands:    scala.collection.Map[Flag, Text],
    statuses:    List[Status],
    variables:   List[Text] )

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

  def probe(prefix: List[Text]): Probe =
    val focus = prefix.size
    val textArguments = prefix :+ t""
    val synthesized = Cli.arguments(textArguments.stdlib, focus, Unset, Prim)

    // A recording view of the environment: every variable the application reads while its
    // structure is being probed is noted, so that the variables it consults can be documented
    // without being listed by hand. Only reads made before the `execute` block are seen, since
    // that block is not run in completion mode.
    val variables: scm.LinkedHashSet[Text] = scm.LinkedHashSet()

    val recording: Environment = name =>
      variables += name
      environment.variable(name)

    val completion =
      Completion
        ( synthesized,
          synthesized,
          recording,
          workingDirectory,
          Shell.Zsh,
          focus,
          Unset,
          stdio,
          t"",
          Prim,
          login )

    block(using completion)

    Probe
      ( completion.cursorSuggestions,
        completion.flags.keySet.to(List),
        completion.globalFlags.foldLeft(Set[Flag]())(_ + _),
        completion.operandNames,
        List.of(completion.statuses.to(scala.List)),
        List.of(variables.to(scala.List)) )

  def build
    ( prefix:      List[Text],
      command:     Text,
      description: Optional[Text | Teletype],
      group:       Optional[CommandGroup],
      seen:        Set[List[Text]],
      inherited:   Set[Flag] )
  :   Help =

    if seen.has(prefix) then Help(command, description, Nil, Nil, group) else
      val Probe(suggestions, flags, globals, operands, statuses, variables) = probe(prefix)

      // Flags already attributed to an ancestor re-register at every deeper prefix, since each
      // probe re-runs the whole program; they belong to the ancestor, so drop them here.
      val ownFlags = flags.filter(!inherited.has(_))

      val parameters = ownFlags.map: flag =>
        Help.Param
          ( Flag.serialize(flag.name),
            flag.aliases.map(Flag.serialize(_)),
            flag.description,
            flag.repeatable,
            globals.has(flag),
            operands.get(flag).optional )

      val known = flags.stdlib.foldLeft(inherited)(_ + _)

      val children =
        List.of(suggestions.stdlib.distinctBy(_.core)).bind: suggestion =>
          val childPrefix = prefix :+ suggestion.core

          if suggestion.hidden || suggestion.incomplete then Nil
          else
            List
              ( build
                  ( childPrefix,
                    suggestion.core,
                    suggestion.description,
                    suggestion.group,
                    seen + prefix,
                    known ) )

      Help
        ( command,
          description,
          parameters,
          children.sort(_.command),
          group,
          statuses,
          variables )

  build(Nil, command, Unset, Unset, Set(), Set())

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

      List.of(arguments.toList) match
        case
          t"{completions}" :: t"powershell" :: As.Int(cursor) :: _ :: tty ::
            t"--" ::
            rawLine ::
            Nil =>

          val parts0 = rawLine.cut(t" ")
          val parts = if cursor > rawLine.length then parts0 :+ t"" else parts0
          val wordStarts = parts.stdlib.scanLeft(0){ (pos, w) => pos + w.length + 1 }.init
          val wordIdx = wordStarts.lastIndexWhere(_ <= cursor).max(0)
          val posInWord = cursor - wordStarts(wordIdx)
          val focus = (wordIdx - 1).max(0)
          val restParts = if parts.size > 1 then parts.tail else List(t"")
          val tab = Completions.tab(tty, Completions.Tab(arguments.to(List), focus, cursor))

          Completion
            ( Cli.arguments(arguments, focus, posInWord, tab),
              Cli.arguments(restParts.stdlib, focus, posInWord, tab),
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

            val rest2 = read(rest, false, Nil)

            val focus = focus1 - (if shell == Shell.Zsh then 2 else 1)

            val position = if shell == Shell.Bash then Unset else position0
            val tab = Completions.tab(tty, Completions.Tab(arguments.to(List), focus, position0))
            val equalses = rest.stdlib.take(focus0).count(_ == t"=")
            val focus2 = focus - (if shell == Shell.Bash then equalses else 0)

            Completion
              ( Cli.arguments(arguments, focus2, position, tab),
                Cli.arguments(rest2.stdlib, focus2, position, tab),
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
