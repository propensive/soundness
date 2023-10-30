/*
    Exoskeleton, version [unreleased]. Copyright 2023 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package exoskeleton

import rudiments.*
import digression.*
import spectacular.*
import gossamer.*
import ambience.*
import anticipation.*

import java.io.*

case class InstallError() extends Error(msg"installation failed")

object Generate extends Application:
  def main(using CliShell): Exit =
    try
      val files = install()
      System.out.nn.println(s"Installed ${files.join(t", ")}")
      
      Exit(0)
    
    catch case e: Exception =>
      e.printStackTrace()
      Exit(1)

  def install()(using CliShell): Set[Text] throws InstallError | EnvError = arguments.to(List) match
    case cmd :: Shell(shell) :: Nil =>
      Set(shell.install(cmd, summon[CliShell].environment))
    
    case cmd :: Nil =>
      Shell.all.map(_.install(cmd, summon[CliShell].environment)).to(Set)
    
    case _ =>
      Set()

  def complete(cli: Cli): Suggestions = cli.index match
    case 1 => Suggestions(Nil, t"Please specify the command to complete")
    case 2 => Suggestions(Shell.all.map { shell =>
                Suggestion(shell.shell, shell.description, false, false)
              })
    case 3 => Suggestions(Nil, t"Please specify the directory in which to install the file")
    case _ => Suggestions(Nil, t"No more parameters")

case class CliShell(args: List[Text], environment: Map[Text, Text], properties: Map[Text, Text])

case class Cli(command: Text,
               args: List[Text],
               environment: Map[Text, Text],
               properties: Map[Text, Text],
               index: Int):
  def currentArg: Text = args.lift(index).getOrElse(t"")

case class Exit(status: Int)

case class Suggestions(defs: List[Suggestion], title: Maybe[Text] = Unset)

case class Suggestion
    (word: Text, description: Maybe[Text] = Unset, hidden: Boolean = false, incomplete: Boolean = false)

object Shell:
  val all: List[Shell] = List(Zsh, Bash, Fish)
  def unapply(string: Text): Option[Shell] = all.find(_.shell == string)

abstract class Shell(val shell: Text):
  def serialize(cli: Cli, completions: Suggestions): LazyList[Text]
  def script(cmd: Text): Text
  def filename(cmd: Text): Text
  def description: Text

  def xdgConfig(env: Map[Text, Text]): File =
    val xdgDefault = t"${env(t"HOME")}/.config"
    File(env.get(t"XDG_CONFIG_HOME").getOrElse(xdgDefault).s)
  
  def destination(env: Map[Text, Text]): File

  def install(cmd: Text, env: Map[Text, Text]): Text throws EnvError | InstallError =
    val dir = destination(env)
    if !dir.exists() then dir.mkdirs()
    val file = File(dir, filename(cmd).s)
    val out = BufferedWriter(FileWriter(file))
    out.write(script(cmd).s)
    out.close()
    Text(file.getAbsolutePath.nn)

object Zsh extends Shell(t"zsh"):
  def description: Text = t"ZSH shell"
  def destination(env: Map[Text, Text]): File = File(File(xdgConfig(env), t"exoskeleton".s), shell.s)
  
  def serialize(cli: Cli, completions: Suggestions): LazyList[Text] =
    val width = maxLength(completions.defs.filter(_.word.starts(cli.currentArg)))
    (completions.title.option.map(List(t"", t"-X", _).join(t"\t")) ++ completions.defs.flatMap {
      case Suggestion(word, desc, hidden, incomplete) =>
        val hide = if hidden then List(t"-n") else Nil
        List(
          List(describe(width, word, desc.option), t"-d", t"desc") ::: hide ::: List(t"--", word),
          if incomplete then List(t"", t"-n") ::: List(t"--", t"$word.") else Nil
        ).map(_.join(t"\t"))
    }).to(LazyList)
  
  def script(cmd: Text): Text =
    t"""#compdef $cmd

_$cmd() {
  ifsx=$$IFS IFS=$$'\\t'
  $cmd '{exoskeleton}' 'zsh' "$$(($$CURRENT - 1))" -- $$words | while read -r -A ln; do
    desc=($${ln[1]})
    compadd -Q $${ln:1}
  done
  IFS=$$ifsx
}

_$cmd

return 0
"""
  
  def filename(cmd: Text): Text = t"_$cmd"
  
  private def maxLength(defs: Seq[Suggestion]): Int = (0 +: defs.map(_.word.length)).max
  private def describe(width: Int, word: Text, desc: Option[Text]): Text =
    desc.fold(word) { desc => t"${word.fit(width)}  -- $desc" }

object Bash extends Shell(t"bash"):
  def description = t"The Bourne Again SHell"

  def destination(env: Map[Text, Text]): File = File(s"${env(t"HOME")}/.bash_completion")

  def serialize(cli: Cli, completions: Suggestions): LazyList[Text] =
    LazyList(completions.defs.filter(_.word.starts(cli.currentArg)).collect {
      case Suggestion(word, desc, false, _) => word
    }.join(t"\t"))
  
  def script(cmd: Text): Text =
    t"""_${cmd}_complete() {
  data=$$($cmd '{exoskeleton}' 'bash' $$COMP_CWORD -- $$COMP_LINE )
  ifsx=$$IFS
  IFS=$$'\t'
  COMPREPLY=($$data)
  IFS=$$ifsx
}

complete -F _${cmd}_complete $cmd
"""

  def filename(cmd: Text): Text = t"_$cmd"
  
object Fish extends Shell(t"fish"):
  def description: Text = t"The Fish Shell"
  
  def destination(env: Map[Text, Text]): File =
    File(File(xdgConfig(env), t"fish".s), t"completions".s)

  def serialize(cli: Cli, completions: Suggestions): LazyList[Text] =
    completions.defs.to(LazyList).map {
      case Suggestion(word, desc, hidden, incomplete) => t"$word\t${desc.or(t"")}"
    }
  
  def script(cmd: Text): Text =
    t"""for line in ($cmd '{exoskeleton}' 'fish' (count (commandline -o)) -- (commandline -o))
  complete -f -c $cmd -a (string escape $$line[1])
end
"""

  def filename(cmd: Text): Text = t"$cmd.fish"

case class ParamMap(args: Text*):

  case class Arg(value: Text)

  case class Part(no: Int, start: Int, end: Int):
    def apply(): Text = args(no).slice(start, end).nn

  object Parameter:
    given Show[Parameter] = param =>
      val prefix = if param.key().length == 1 then "-" else "--"
      t"$prefix${param.key()} ${param.values.map(_()).join(t" ")}"

  case class Parameter(key: Part, values: Vector[Part] = Vector())
