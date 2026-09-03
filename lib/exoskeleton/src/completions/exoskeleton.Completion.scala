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

import scala.collection.mutable as scm

import ambience.*
import anticipation.*
import denominative.*
import escapade.*
import gossamer.*
import gossamer.collationComparable
import gossamer.collations.codepoints
import guillotine.*
import hieroglyph.*, textMetrics.uniformMetric
import hypotenuse.*
import spectacular.*
import symbolism.*
import turbulence.*
import rudiments.*
import vacuous.*
import rudiments.sortingAlgorithms.timsort

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
  val requiredFlags: scm.HashSet[Flag] = scm.HashSet()
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
      // The partially-typed operand text is passed for the `Discoverable` to resolve. In the
      // first case the focused argument is the operand itself (the `--flag=operand` form); in
      // the second the focus is the flag, with the cursor on the following word — the flag's
      // operand at that position, or empty if the word has not been started.
      if operands.prim == argument then
        val allSuggestions = discoverable.discover(argument(), tab)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

      if flag.matches(argument) && currentArgument == argument.position + 1 then
        val operand = operands.seek(_.position == currentArgument).let(_()).or(t"")
        val allSuggestions = discoverable.discover(operand, tab)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

    if !flag.secret then
      flags(flag) = discoverable
      operand.let(operandNames(flag) = _)
      if !dispatchSuggested then globalFlags += flag

  override def record(statuses: List[Status]): Unit = statuses.each(this.statuses += _)

  override def present(flag: Flag): Unit = if !flag.repeatable then seenFlags += flag

  // Requiredness is an interface fact, recorded whether or not the flag is present; a failed
  // requirement never precludes completions.
  override def demand(flag: Flag, present: Boolean): Unit =
    if !flag.secret then requiredFlags += flag

  override def locate(flag: Flag): Optional[List[Argument]] =
    interpreter.locate(parameters, flag)

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

      . order(_.core)

  def flagSuggestions(longOnly: Boolean): List[Suggestion] =
    (flags.keySet -- seenFlags).to(List).bind: flag =>
      val allFlags = (flag.name :: flag.aliases)

      if longOnly then
        allFlags.sweep { case text: Text => text }.match
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
    arguments.seek(_.position == currentArgument).lay(t"")(_.value)

  def serialize: List[Text] =
    val items0 =
      if cursorSuggestions.nil then flagSuggestions(focusText.starts(t"--")) else cursorSuggestions

    val items = interpreter.focus(parameters).lay(items0): focus => items0.map(focus.wrap(_))

    shell match
      case Shell.Zsh =>
        val title = explanation.let { explanation => List(sh"'' -X $explanation") }.or(Nil)
        val termcap: Termcap = termcapDefinitions.xtermTrueColorTermcap

        lazy val width = items.map { item => item.display.or(item.core).length }.maximize(identity).or(0)
        lazy val aliasesWidth = items.map(_.aliases.join(t" ").length).maximize(identity).or(0) + 1

        val itemLines: List[Command] = items.bind:
          case Suggestion(core0, description, hidden, incomplete, aliases, prefix, suffix, _, _, _,
                          display) =>
            val hiddenParam = if hidden then sh"-n" else sh""
            val shortFlag = focusText.starts(t"-") && !focusText.starts(t"--")
            // The short-flag menu names the flag, then its long form. A clustered candidate has
            // already been named in full by `display`, so repeating the bare character there
            // would just print it twice.
            val aliasText =
              if shortFlag && display.absent then core0 else aliases.join(t" ").fit(aliasesWidth)
            val prefix2 = if prefix.nil then sh"" else sh"-p $prefix"
            val suffix2 = if suffix.nil then sh"" else sh"-s $suffix"
            val core = if shortFlag then aliases.prim.or(core0) else core0

            // What the menu shows, where that differs from what is inserted — a clustered short
            // flag inserts one character behind a hidden prefix but is still named in full
            // (#1888). `compadd`'s display array already carries the described form, so only the
            // undescribed line needs `-d` adding.
            val shown = display.or(core)

            val mainLine = description.absolve match
              case Unset =>
                if prefix.nil then sh"'' $hiddenParam -- $core"
                else if display.absent then sh"'' $hiddenParam $prefix2 $suffix2 -- $core"
                else sh"'$shown' $hiddenParam $prefix2 $suffix2 -l -d desc -- $core"

              case description: Text =>
                val params = sh"$prefix2 $suffix2 -l -d desc $hiddenParam -- $core"
                sh"'${shown.fit(width)} $aliasText -- $description' $params"

              case description: Teletype =>
                val desc = description.render(termcap)
                val params = sh"$prefix2 $suffix2 -l -d desc $hiddenParam -- $core"
                sh"'${shown.fit(width)} $aliasText -- $desc' $params"

            val duplicateLine: List[Command] =
              if !incomplete then List()
              else List(sh"'' $prefix2 $suffix2 -S '' -- $core")

            (List(mainLine): List[Command]) + duplicateLine

        val lines: List[Command] = title + itemLines
        lines.map(_.arguments.join(t"\u0000"))

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
        val sole = items.count(!_.hidden) == 1

        items.bind:
          case suggestion@Suggestion(core, description, hidden, incomplete, aliases, _, _, _, _, _, _) =>
            if hidden then Nil else
              // Named methods, not lambdas: an interpolation inside a lambda passed to a
              // collection combinator runs its implicit search while the combinator's element
              // type is still uninstantiated, tripping dotc's `wildApprox` assertion
              // (scala/scala3#24824).
              def line(text: Text): Text = description.absolve match
                case Unset                 => t"$text"
                case description: Text     => t"$text\t$description"
                case description: Teletype => t"$text\t${description.plain}"

              def spaced(text: Text): Text = t"$text "

              val mainLines: List[Text] = (suggestion.text :: aliases).map(line)

              if !incomplete || !sole || suggestion.text.ends(t"/") then mainLines
              else mainLines + (suggestion.text :: aliases).map(spaced)

      case Shell.Powershell =>
        // PowerShell inserts a `CompletionResult` verbatim, so a trailing-space twin is
        // just a visible duplicate; `incomplete` has no rendering there.
        items.bind:
          case suggestion@Suggestion(core, description, hidden, _, aliases, _, _, _, _, _, _) =>
            if hidden then Nil else
              // Named, not a lambda, for the `wildApprox` reason noted above.
              def line(text: Text): Text = description.absolve match
                case Unset                 => t"$text"
                case description: Text     => t"$text\t$description"
                case description: Teletype => t"$text\t${description.plain}"

              (suggestion.text :: aliases).map(line)
