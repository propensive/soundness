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
┃    Soundness, version 0.41.0.                                                                    ┃
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
    focusPosition:    Int,
    stdio:            Stdio,
    signals:          Spool[Signal],
    tty:              Text,
    tab:              Ordinal,
    userId:           Optional[Int],
    username:         Text)
   (using interpreter: Interpreter)
extends Cli:
  private lazy val parameters: interpreter.Parameters = interpreter.interpret(arguments)

  val flags: scm.HashMap[Flag, Discoverable] = scm.HashMap()
  val seenFlags: scm.HashSet[Flag] = scm.HashSet()
  var explanation: Optional[Text] = Unset
  var cursorSuggestions: List[Suggestion] = Nil
  def proceed: Boolean = true

  def parameter[operand: Interpretable](flag: Flag)(using (? <: operand) is Discoverable)
  : Optional[operand] =
      given cli: Cli = this
      parameters.read(flag)


  def focus: Argument = arguments(currentArgument)

  def focused(argument: Argument): Boolean =
    currentArgument == argument.position && argument.format.match
      case Argument.Format.Full              => true
      case Argument.Format.EqualityPrefix    => false
      case Argument.Format.EqualitySuffix    => argument.value.contains(t"=")
      case Argument.Format.CharFlag(ordinal) => false
      case Argument.Format.FlagSuffix        => focusPosition.z > Sec

  override def register(flag: Flag, discoverable: Discoverable): Unit =
    parameters.focus.let: argument =>
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

  def serialize: List[Text] =
    val items =
      if cursorSuggestions.isEmpty && parameters.focus.absent
      then flagSuggestions(focus().starts(t"--"))
      else cursorSuggestions

    shell match
      case Shell.Zsh =>
        val title = explanation.let { explanation => List(sh"'' -X $explanation") }.or(Nil)
        val termcap: Termcap = termcapDefinitions.xtermTrueColor

        lazy val width = items.map(_.core.length).max
        lazy val aliasesWidth = items.map(_.aliases.join(t" ").length).max + 1

        val itemLines: List[Command] = items.flatMap:
          case Suggestion(core, description, hidden, incomplete, aliases, prefix, suffix, _) =>
            val hiddenParam = if hidden then sh"-n" else sh""
            val aliasText = aliases.join(t" ").fit(aliasesWidth)
            val prefix2 = if prefix.empty then sh"" else sh"-p $prefix"
            val suffix2 = if suffix.empty then sh"" else sh"-s $suffix"

            val text = prefix+core+suffix

            val mainLine = description.absolve match
              case Unset =>
                if prefix.empty then sh"'' $hiddenParam -- $core"
                else sh"'' $hiddenParam $prefix2 $suffix2 -- $core"

              case description: Text =>
                sh"'${core.fit(width)} $aliasText $prefix2 $suffix2 -- $description' -d desc -l $hiddenParam -- $core"

              case description: Teletype =>
                val desc = description.render(termcap)
                sh"'${core.fit(width)} $aliasText $prefix2 $suffix2 -- $desc' -d desc -l $hiddenParam -- $core"

            val duplicateLine =
              if !incomplete then List() else List(sh"'' $prefix2 $suffix2 -S '' -- $core")

            val aliasLines = aliases.map: text =>
              description.absolve match
                case Unset             =>
                  sh"'' -n -- $text"

                case description: Text =>
                  sh"'${core.fit(width)} $aliasText -- $description' -d desc -l -n -- $core"

                case description: Teletype =>
                  val desc = description.render(termcap)
                  sh"'${core.fit(width)} $aliasText -- $desc' -d desc -l -n -- $core"

            mainLine :: duplicateLine ::: aliasLines

        (title ++ itemLines).map(_.arguments.join(t"\u0000"))

      case Shell.Bash =>
        items.filter(!_.hidden).flatMap: suggestion =>
          suggestion.text :: suggestion.aliases

        . filter(_.starts(focus()))

      case Shell.Fish =>
        items.flatMap:
          case suggestion@Suggestion(core, description, hidden, incomplete, aliases, _, _, _) =>
            (suggestion.text :: aliases).map: text =>
              description.absolve match
                case Unset                 => t"$text"
                case description: Text     => t"$text\t$description"
                case description: Teletype => t"$text\t${description.plain}"
