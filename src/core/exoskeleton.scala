/*
    Exoskeleton, version 2.0.0. Copyright 2017-21 Jon Pretty, Propensive OÜ.

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

class ExoskeletonError(msg: String) extends Exception(s"exoskeleton: $msg")

case class EnvError(envVar: String)
extends ExoskeletonError(s"the environment variable $envVar was not found")

case class InstallError() extends ExoskeletonError("installation failed")

object Generate extends Application:
  def main(using Shell): Exit =
    try
      val files = install()
      println(s"Installed ${files.join(", ")}")
      
      Exit(0)
    
    catch case e: Exception =>
      e.printStackTrace()
      Exit(1)

  def install()(using Shell): Set[String] throws InstallError | EnvError = arguments.to(List) match
    case cmd :: ShellType(shell) :: Nil =>
      Set(shell.install(cmd, summon[Shell].environment))
    
    case cmd :: Nil =>
      ShellType.all.map(_.install(cmd, summon[Shell].environment)).to(Set)
    
    case _ =>
      Set()

  def complete(cli: Cli): Completions = cli.index match
    case 1 => Completions(Nil, "Please specify the command to complete")
    case 2 => Completions(ShellType.all.map { shell =>
                Choice(shell.shell, shell.description, false, false)
              })
    case 3 => Completions(Nil, "Please specify the directory in which to install the file")
    case _ => Completions(Nil, "No more parameters")

case class Shell(args: IArray[String], environment: Map[String, String], properties: Map[String, String])

case class Cli(command: String,
               args: List[String],
               environment: Map[String, String],
               properties: Map[String, String],
               index: Int):
  def currentArg: String = args.lift(index).getOrElse("")

case class Exit(status: Int)

case class Completions(defs: List[Choice], title: Maybe[String] = Unset)

case class Choice(word: String, description: Maybe[String] = Unset, hidden: Boolean = false,
                      incomplete: Boolean = false)

object AsInt:
  def unapply(str: String): Option[Int] = Try(str.toInt).toOption

object ShellType:
  val all: List[ShellType] = List(Zsh, Bash, Fish)
  def unapply(string: String): Option[ShellType] = all.find(_.shell == string)

abstract class ShellType(val shell: String):
  def serialize(cli: Cli, completions: Completions): LazyList[String]
  def script(cmd: String): String
  def filename(cmd: String): String
  def description: String

  def xdgConfig(env: Map[String, String]): File =
    val xdgDefault = s"${env("HOME")}/.config"
    File(env.get("XDG_CONFIG_HOME").getOrElse(xdgDefault))
  
  def destination(env: Map[String, String]): File

  def install(cmd: String, env: Map[String, String]): String throws EnvError | InstallError =
    val dir = destination(env)
    if !dir.exists() then dir.mkdirs()
    val file = File(dir, filename(cmd))
    val out = BufferedWriter(FileWriter(file))
    out.write(script(cmd))
    out.close()
    file.getAbsolutePath.nn

object Zsh extends ShellType("zsh"):
  def description: String = "ZSH shell"
  def destination(env: Map[String, String]): File = File(File(xdgConfig(env), "exoskeleton"), shell)
  
  def serialize(cli: Cli, completions: Completions): LazyList[String] =
    val width = maxLength(completions.defs.filter(_.word.startsWith(cli.currentArg)))
    (completions.title.option.map(List("", "-X", _).mkString("\t")) ++ completions.defs.flatMap {
      case Choice(word, desc, hidden, incomplete) =>
        val hide = if hidden then List("-n") else Nil
        List(
          List(describe(width, word, desc.option), "-d", "desc") ::: hide ::: List("--", word),
          if incomplete then List("", "-n") ::: List("--", s"$word.") else Nil
        ).map(_.mkString("\t"))
    }).to(LazyList)
  
  def script(cmd: String): String =
    s"""|#compdef $cmd
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
        |""".stripMargin
  
  def filename(cmd: String): String = s"_$cmd"
  
  private def maxLength(defs: Seq[Choice]): Int = (0 +: defs.map(_.word.length)).max
  private def describe(width: Int, word: String, desc: Option[String]): String =
    desc.fold(word) { desc => s"${word.padTo(width, ' ')}  -- $desc" }

object Bash extends ShellType("bash"):
  def description = "The Bourne Again SHell"

  def destination(env: Map[String, String]): File = File(s"${env("HOME")}/.bash_completion")

  def serialize(cli: Cli, completions: Completions): LazyList[String] =
    LazyList(completions.defs.filter(_.word.startsWith(cli.currentArg)).collect {
      case Choice(word, desc, false, _) => word
    }.mkString("\t"))
  
  def script(cmd: String): String =
    s"""|_${cmd}_complete() {
        |  data=$$($cmd '{exoskeleton}' 'bash' $$COMP_CWORD -- $$COMP_LINE )
        |  ifsx=$$IFS
        |  IFS=$$'\t'
        |  COMPREPLY=($$data)
        |  IFS=$$ifsx
        |}
        |
        |complete -F _${cmd}_complete $cmd
        |""".stripMargin

  def filename(cmd: String): String = s"_$cmd"
  
object Fish extends ShellType("fish"):
  def description: String = "The Fish Shell"
  
  def destination(env: Map[String, String]): File =
    File(File(xdgConfig(env), "fish"), "completions")

  def serialize(cli: Cli, completions: Completions): LazyList[String] =
    completions.defs.to(LazyList).map {
      case Choice(word, desc, hidden, incomplete) => s"$word\t${desc.otherwise("")}"
    }
  
  def script(cmd: String): String =
    s"""|for line in ($cmd '{exoskeleton}' 'fish' (count (commandline -o)) -- (commandline -o))
        |  complete -f -c $cmd -a (string escape $$line[1])
        |end
        |""".stripMargin

  def filename(cmd: String): String = s"$cmd.fish"

case class ParamMap(args: String*):

  case class Arg(value: String)

  case class Part(no: Int, start: Int, end: Int):
    def apply(): String = args(no).slice(start, end).nn

  case class Parameter(key: Part, values: Vector[Part] = Vector()):
    override def toString =
      val prefix = if key().length == 1 then "-" else "--"
      s"$prefix${key()} ${values.map(_()).mkString(" ")}"