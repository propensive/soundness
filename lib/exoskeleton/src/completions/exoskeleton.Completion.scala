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
import denominative.*
import escapade.*
import gossamer.*
import guillotine.*
import hieroglyph.*, textMetrics.uniformMetric
import hypotenuse.*
import spectacular.*
import symbolism.*
import turbulence.*
import rudiments.*
import vacuous.*

case class Completion
  ( fullArguments:    List[Argument],
    arguments:        List[Argument],
    environment:      Environment,
    workingDirectory: WorkingDirectory,
    shell:            Shell,
    currentArgument:  Int,
    focusPosition:    Optional[Int],
    stdio:            Stdio,
    tty:              Text,
    tab:              Ordinal,
    login:            Login )
  ( using interpreter: Interpreter )
extends Cli:
  private lazy val parameters: interpreter.Topic = interpreter.interpret(arguments)

  val flags: scm.HashMap[Flag, Discoverable] = scm.HashMap()
  val operandNames: scm.HashMap[Flag, Text] = scm.HashMap()
  val globalFlags: scm.HashSet[Flag] = scm.HashSet()
  val seenFlags: scm.HashSet[Flag] = scm.HashSet()
  val statuses: scm.LinkedHashSet[Status] = scm.LinkedHashSet()

  // Whether any suggestions have been offered yet for the focused argument. Flags registered
  // before this point were checked before subcommand dispatch, so they are "global": they apply
  // regardless of (and may influence) which subcommand is chosen.
  @scala.caps.unsafe.untrackedCaptures
  private var dispatchSuggested: Boolean = false

  @scala.caps.unsafe.untrackedCaptures
  var explanation: Optional[Text] = Unset
  @scala.caps.unsafe.untrackedCaptures
  var cursorSuggestions: List[Suggestion] = Nil

  def proceed: Boolean = true


  def parameter[operand: Interpretable](flag: Flag)(using (? <: operand) is Discoverable)
  :   Optional[operand] =

    // An alias of `this` with its precise capture, not a fresh capability.
    given cli: (Cli^{this}) = this
    interpreter.read(parameters, flag)


  def focused(argument: Argument): Boolean =
    currentArgument == argument.position && argument.format.match
      case Argument.Format.Full              => true
      case Argument.Format.EqualityPrefix    => false
      case Argument.Format.EqualitySuffix    => argument.value.contains(t"=")
      case Argument.Format.CharFlag(ordinal) => false
      case Argument.Format.FlagSuffix        => focusPosition.let(_.z > Sec).or(true)

  override def register(flag: Flag, discoverable: Discoverable, operand: Optional[Text]): Unit =
    val operands = interpreter.find(parameters, flag)

    interpreter.focus(parameters).let: argument =>
      if operands.headOption.contains(argument) then
        val allSuggestions = discoverable.discover(tab).to(List)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

      if flag.matches(argument) && currentArgument == argument.position + 1 then
        val allSuggestions = discoverable.discover(tab).to(List)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

    if !flag.secret then
      flags(flag) = discoverable
      operand.let(operandNames(flag) = _)
      if !dispatchSuggested then globalFlags += flag

  override def record(statuses: List[Status]): Unit = statuses.each(this.statuses += _)

  override def present(flag: Flag): Unit = if !flag.repeatable then seenFlags += flag

  override def explain(update: (Optional[Text] aka "prior") ?=> Optional[Text]): Unit =
    explanation = update(using explanation.aka["prior"])

  override def suggest
    ( argument: Argument,
      update:   (List[Suggestion] aka "prior") ?=> List[Suggestion],
      prefix:   Text,
      suffix:   Text ) =

    if focused(argument) then
      dispatchSuggested = true
      cursorSuggestions = update(using cursorSuggestions.aka["prior"]).map: suggestion =>
        if suggestion.expanded then suggestion
        else suggestion.copy(core = prefix+suggestion.core+suffix, expanded = true)

      . sort(_.core)

  def flagSuggestions(longOnly: Boolean): List[Suggestion] =
    (flags.keySet -- seenFlags).to(List).bind: flag =>
      val allFlags = (flag.name :: flag.aliases)

      if longOnly then
        List.of(allFlags.collect { case text: Text => text }).match
          case main :: aliases =>
            List
              ( Suggestion
                  ( Flag.serialize(main),
                    flag.description,
                    aliases = aliases.map(Flag.serialize(_)) ) )

          case Nil =>
            Nil

      else
        List(Suggestion(Flag.serialize(flag.name), flag.description, aliases =
          flag.aliases.map(Flag.serialize(_))))

  def focusText: Text =
    arguments.find(_.position == currentArgument).fold(t"")(_.value)

  def serialize: List[Text] =
    val items0 =
      if cursorSuggestions.nil then flagSuggestions(focusText.starts(t"--")) else cursorSuggestions

    val items = interpreter.focus(parameters).lay(items0): focus => items0.map(focus.wrap(_))

    shell match
      case Shell.Zsh =>
        val title = explanation.let { explanation => List(sh"'' -X $explanation") }.or(Nil)
        val termcap: Termcap = termcapDefinitions.xtermTrueColorTermcap

        lazy val width = items.map(_.core.length).max
        lazy val aliasesWidth = items.map(_.aliases.join(t" ").length).max + 1

        val itemLines: List[Command] = items.bind:
          case Suggestion(core0, description, hidden, incomplete, aliases, prefix, suffix, _, _) =>
            val hiddenParam = if hidden then sh"-n" else sh""
            val shortFlag = focusText.starts(t"-") && !focusText.starts(t"--")
            val aliasText = if shortFlag then core0 else aliases.join(t" ").fit(aliasesWidth)
            val prefix2 = if prefix.nil then sh"" else sh"-p $prefix"
            val suffix2 = if suffix.nil then sh"" else sh"-s $suffix"
            val core = if shortFlag then aliases.headOption.getOrElse(core0) else core0

            val mainLine = description.absolve match
              case Unset =>
                if prefix.nil then sh"'' $hiddenParam -- $core"
                else sh"'' $hiddenParam $prefix2 $suffix2 -- $core"

              case description: Text =>
                val params = sh"$prefix2 $suffix2 -l -d desc $hiddenParam -- $core"
                sh"'${core.fit(width)} $aliasText -- $description' $params"

              case description: Teletype =>
                val desc = description.render(termcap)
                val params = sh"$prefix2 $suffix2 -l -d desc $hiddenParam -- $core"
                sh"'${core.fit(width)} $aliasText -- $desc' $params"

            val duplicateLine: List[Command] =
              if !incomplete then List()
              else List(sh"'' $prefix2 $suffix2 -S '' -- $core")

            List(mainLine) ::: duplicateLine

        List.of(title.stdlib ++ itemLines.stdlib).map(_.arguments.join(t"\u0000"))

      case Shell.Bash =>
        items.filter(!_.hidden).bind: suggestion =>
          (suggestion.text :: suggestion.aliases): List[Text]

        . filter(_.starts(focusText))

      case Shell.Fish =>
        // Every output line is a distinct candidate in fish, so `incomplete` cannot be
        // rendered as a per-candidate duplicate without doubling the menu. Fish already
        // completes a `/`-terminated candidate without a trailing space, and with several
        // candidates their longest common prefix stops the insertion short anyway; only a
        // sole, non-slash, incomplete candidate needs the trailing-space twin, which makes
        // the LCP the candidate itself so fish inserts it without terminating the word.
        val sole = items.stdlib.count(!_.hidden) == 1

        items.bind:
          case suggestion@Suggestion(core, description, hidden, incomplete, aliases, _, _, _, _) =>
            if hidden then Nil else
              val mainLines = (suggestion.text :: aliases.stdlib).map: text =>
                description.absolve match
                  case Unset                 => t"$text"
                  case description: Text     => t"$text\t$description"
                  case description: Teletype => t"$text\t${description.plain}"

              if !incomplete || !sole || suggestion.text.ends(t"/") then List.of(mainLines)
              else
                List.of:
                  mainLines ++ (suggestion.text :: aliases.stdlib).map: text =>
                    t"$text "

      case Shell.Powershell =>
        // PowerShell inserts a `CompletionResult` verbatim, so a trailing-space twin is
        // just a visible duplicate; `incomplete` has no rendering there.
        items.bind:
          case suggestion@Suggestion(core, description, hidden, _, aliases, _, _, _, _) =>
            if hidden then Nil else
              List.of:
                (suggestion.text :: aliases.stdlib).map: text =>
                  description.absolve match
                    case Unset                 => t"$text"
                    case description: Text     => t"$text\t$description"
                    case description: Teletype => t"$text\t${description.plain}"
