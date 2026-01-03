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
┃    Soundness, version 0.46.0.                                                                    ┃
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
import denominative.*
import escapade.*
import gossamer.*
import guillotine.*
import hieroglyph.*, textMetrics.uniform
import hypotenuse.*
import inimitable.*
import profanity.*
import proscenium.*
import rudiments.*
import spectacular.*
import symbolism.*
import turbulence.*
import vacuous.*

import scala.collection.mutable as scm

case class Completion
   (fullArguments:    List[Argument],
    arguments:        List[Argument],
    environment:      Environment,
    workingDirectory: WorkingDirectory,
    shell:            Shell,
    currentArgument:  Int,
    focusPosition:    Optional[Int],
    stdio:            Stdio,
    signals:          Spool[Signal],
    tty:              Text,
    tab:              Ordinal,
    login:            Login)
   (using interpreter: Interpreter)
extends Cli:
  private lazy val parameters: interpreter.Topic = interpreter.interpret(arguments)

  val flags: scm.HashMap[Flag, Discoverable] = scm.HashMap()
  val seenFlags: scm.HashSet[Flag] = scm.HashSet()
  var explanation: Optional[Text] = Unset
  var cursorSuggestions: List[Suggestion] = Nil
  def proceed: Boolean = true

  def parameter[operand: Interpretable](flag: Flag)(using (? <: operand) is Discoverable)
  : Optional[operand] =

      given cli: Cli = this
      interpreter.read(parameters, flag)


  def focused(argument: Argument): Boolean =
    currentArgument == argument.position && argument.format.match
      case Argument.Format.Full              => true
      case Argument.Format.EqualityPrefix    => false
      case Argument.Format.EqualitySuffix    => argument.value.contains(t"=")
      case Argument.Format.CharFlag(ordinal) => false
      case Argument.Format.FlagSuffix        => focusPosition.let(_.z > Sec).or(true)

  override def register(flag: Flag, discoverable: Discoverable): Unit =
    val operands = interpreter.find(parameters, flag)

    interpreter.focus(parameters).let: argument =>
      if operands.contains(argument) then
        val allSuggestions = discoverable.discover(tab).to(List)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

      if flag.matches(argument) && currentArgument == argument.position + 1 then
        val allSuggestions = discoverable.discover(tab).to(List)
        if allSuggestions != Nil then cursorSuggestions = allSuggestions

    if !flag.secret then flags(flag) = discoverable

  override def present(flag: Flag): Unit = if !flag.repeatable then seenFlags += flag

  override def explain(update: (prior: Optional[Text]) ?=> Optional[Text]): Unit =
    explanation = update(using explanation)

  override def suggest
                (argument: Argument,
                 update:   (prior: List[Suggestion]) ?=> List[Suggestion],
                 prefix:   Text,
                 suffix:   Text) =
    if focused(argument) then
      cursorSuggestions = update(using cursorSuggestions).map: suggestion =>
        if suggestion.expanded then suggestion
        else suggestion.copy(core = prefix+suggestion.core+suffix, expanded = true)

  def flagSuggestions(longOnly: Boolean): List[Suggestion] =
    (flags.keySet.to(Set) -- seenFlags.to(Set)).to(List).flatMap: flag =>
      val allFlags = (flag.name :: flag.aliases)

      if longOnly then
        allFlags.collect { case text: Text => text }.match
          case main :: aliases =>
            List
             (Suggestion
               (Flag.serialize(main), flag.description, aliases = aliases.map(Flag.serialize(_))))

          case Nil => Nil

      else List(Suggestion(Flag.serialize(flag.name), flag.description, aliases =
          flag.aliases.map(Flag.serialize(_))))

  def focusText: Text = arguments.find(_.position == currentArgument).get.value

  def serialize: List[Text] =
    val items0 =
      if cursorSuggestions.isEmpty
      then flagSuggestions(focusText.starts(t"--"))
      else cursorSuggestions


    val items = interpreter.focus(parameters).lay(items0) { focus => items0.map(focus.wrap(_)) }

    shell match
      case Shell.Zsh =>
        val title = explanation.let { explanation => List(sh"'' -X $explanation") }.or(Nil)
        val termcap: Termcap = termcapDefinitions.xtermTrueColor

        lazy val width = items.map(_.core.length).max
        lazy val aliasesWidth = items.map(_.aliases.join(t" ").length).max + 1

        val itemLines: List[Command] = items.flatMap:
          case Suggestion(core0, description, hidden, incomplete, aliases, prefix, suffix, _) =>
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

            val duplicateLine =
              if !incomplete then List() else List(sh"'' $prefix2 $suffix2 -S '' -- $core")

            mainLine :: duplicateLine

        (title ++ itemLines).map(_.arguments.join(t"\u0000"))

      case Shell.Bash =>
        items.filter(!_.hidden).flatMap: suggestion =>
          (suggestion.text :: suggestion.aliases)

        . filter(_.starts(focusText))

      case Shell.Fish =>
        items.flatMap:
          case suggestion@Suggestion(core, description, hidden, incomplete, aliases, _, _, _) =>
            if hidden then Nil else
              (suggestion.text :: aliases).map: text =>
                description.absolve match
                  case Unset                 => t"$text"
                  case description: Text     => t"$text\t$description"
                  case description: Teletype => t"$text\t${description.plain}"
