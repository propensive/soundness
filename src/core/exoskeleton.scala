/*
    Exoskeleton, version 2.1.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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
import gossamer.*

import collection.JavaConverters.*

import scala.util.*, scala.annotation.tailrec

import java.io.*

class ExoskeletonError(msg: Txt) extends Exception(str"exoskeleton: $msg".s)

case class EnvError(envVar: Txt)
extends ExoskeletonError(str"the environment variable $envVar was not found")

case class InstallError() extends ExoskeletonError(str"installation failed")

object Generate extends Application:
  def main(using Shell): Exit =
    try
      val files = install()
      println(s"Installed ${files.join(str", ")}")
      
      Exit(0)
    
    catch case e: Exception =>
      e.printStackTrace()
      Exit(1)

  def install()(using Shell): Set[Txt] throws InstallError | EnvError = arguments.to(List) match
    case cmd :: ShellType(shell) :: Nil =>
      Set(shell.install(cmd, summon[Shell].environment))
    
    case cmd :: Nil =>
      ShellType.all.map(_.install(cmd, summon[Shell].environment)).to(Set)
    
    case _ =>
      Set()

  def complete(cli: Cli): Completions = cli.index match
    case 1 => Completions(Nil, str"Please specify the command to complete")
    case 2 => Completions(ShellType.all.map { shell =>
                Choice(shell.shell, shell.description, false, false)
              })
    case 3 => Completions(Nil, str"Please specify the directory in which to install the file")
    case _ => Completions(Nil, str"No more parameters")

case class Shell(args: IArray[Txt], environment: Map[Txt, Txt], properties: Map[Txt, Txt])

case class Cli(command: Txt,
               args: List[Txt],
               environment: Map[Txt, Txt],
               properties: Map[Txt, Txt],
               index: Int):
  def currentArg: Txt = args.lift(index).getOrElse(str"")

case class Exit(status: Int)

case class Completions(defs: List[Choice], title: Maybe[Txt] = Unset)

case class Choice(word: Txt, description: Maybe[Txt] = Unset, hidden: Boolean = false,
                      incomplete: Boolean = false)

object AsInt:
  def unapply(str: Txt): Option[Int] = Try(str.s.toInt).toOption

object ShellType:
  val all: List[ShellType] = List(Zsh, Bash, Fish)
  def unapply(string: Txt): Option[ShellType] = all.find(_.shell == string)

abstract class ShellType(val shell: Txt):
  def serialize(cli: Cli, completions: Completions): LazyList[Txt]
  def script(cmd: Txt): Txt
  def filename(cmd: Txt): Txt
  def description: Txt

  def xdgConfig(env: Map[Txt, Txt]): File =
    val xdgDefault = str"${env(str"HOME")}/.config"
    File(env.get(str"XDG_CONFIG_HOME").getOrElse(xdgDefault).s)
  
  def destination(env: Map[Txt, Txt]): File

  def install(cmd: Txt, env: Map[Txt, Txt]): Txt throws EnvError | InstallError =
    val dir = destination(env)
    if !dir.exists() then dir.mkdirs()
    val file = File(dir, filename(cmd).s)
    val out = BufferedWriter(FileWriter(file))
    out.write(script(cmd).s)
    out.close()
    Txt(file.getAbsolutePath.nn)

object Zsh extends ShellType(str"zsh"):
  def description: Txt = str"ZSH shell"
  def destination(env: Map[Txt, Txt]): File = File(File(xdgConfig(env), str"exoskeleton".s), shell.s)
  
  def serialize(cli: Cli, completions: Completions): LazyList[Txt] =
    val width = maxLength(completions.defs.filter(_.word.startsWith(cli.currentArg)))
    (completions.title.option.map(List(str"", str"-X", _).join(str"\t")) ++ completions.defs.flatMap {
      case Choice(word, desc, hidden, incomplete) =>
        val hide = if hidden then List(str"-n") else Nil
        List(
          List(describe(width, word, desc.option), str"-d", str"desc") ::: hide ::: List(str"--", word),
          if incomplete then List(str"", str"-n") ::: List(str"--", str"$word.") else Nil
        ).map(_.join(str"\t"))
    }).to(LazyList)
  
  def script(cmd: Txt): Txt =
    Txt(str"""|#compdef $cmd
              |
              |_$cmd() {
              |  ifsx=$$IFS IFS=$$'\\t'
              |  $cmd '{exoskeleton}' 'zsh' "$$(($$CURRENT - 1))" -- $$words | while read -r -A ln; do
              |    desc=($${ln[1]})
              |    compadd -Q $${ln:1}
              |  done
              |  IFS=$$ifsx
              |}
              |
              |_$cmd
              |
              |return 0
              |""".s.stripMargin)
  
  def filename(cmd: Txt): Txt = str"_$cmd"
  
  private def maxLength(defs: Seq[Choice]): Int = (0 +: defs.map(_.word.length)).max
  private def describe(width: Int, word: Txt, desc: Option[Txt]): Txt =
    desc.fold(word) { desc => str"${word.fit(width)}  -- $desc" }

object Bash extends ShellType(str"bash"):
  def description = str"The Bourne Again SHell"

  def destination(env: Map[Txt, Txt]): File = File(s"${env(str"HOME")}/.bash_completion")

  def serialize(cli: Cli, completions: Completions): LazyList[Txt] =
    LazyList(completions.defs.filter(_.word.startsWith(cli.currentArg)).collect {
      case Choice(word, desc, false, _) => word
    }.join(str"\t"))
  
  def script(cmd: Txt): Txt =
    Txt(str"""|_${cmd}_complete() {
              |  data=$$($cmd '{exoskeleton}' 'bash' $$COMP_CWORD -- $$COMP_LINE )
              |  ifsx=$$IFS
              |  IFS=$$'\t'
              |  COMPREPLY=($$data)
              |  IFS=$$ifsx
              |}
              |
              |complete -F _${cmd}_complete $cmd
              |""".s.stripMargin)

  def filename(cmd: Txt): Txt = str"_$cmd"
  
object Fish extends ShellType(str"fish"):
  def description: Txt = str"The Fish Shell"
  
  def destination(env: Map[Txt, Txt]): File =
    File(File(xdgConfig(env), str"fish".s), str"completions".s)

  def serialize(cli: Cli, completions: Completions): LazyList[Txt] =
    completions.defs.to(LazyList).map {
      case Choice(word, desc, hidden, incomplete) => str"$word\t${desc.otherwise(str"")}"
    }
  
  def script(cmd: Txt): Txt =
    Txt(str"""|for line in ($cmd '{exoskeleton}' 'fish' (count (commandline -o)) -- (commandline -o))
              |  complete -f -c $cmd -a (string escape $$line[1])
              |end
              |""".s.stripMargin)

  def filename(cmd: Txt): Txt = str"$cmd.fish"

case class ParamMap(args: Txt*):

  case class Arg(value: Txt)

  case class Part(no: Int, start: Int, end: Int):
    def apply(): Txt = args(no).slice(start, end).nn

  case class Parameter(key: Part, values: Vector[Part] = Vector()):
    override def toString =
      val prefix = if key().length == 1 then "-" else "--"
      s"$prefix${key()} ${values.map(_()).join(str" ")}"