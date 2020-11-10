/*
   ╔═══════════════════════════════════════════════════════════════════════════════════════════════════════════╗
   ║ Exoskeleton, version 1.0.0. Copyright 2019-20 Jon Pretty, Propensive OÜ.                                  ║
   ║                                                                                                           ║
   ║ The primary distribution site is: https://propensive.com/                                                 ║
   ║                                                                                                           ║
   ║ Licensed under  the Apache License,  Version 2.0 (the  "License"); you  may not use  this file  except in ║
   ║ compliance with the License. You may obtain a copy of the License at                                      ║
   ║                                                                                                           ║
   ║     http://www.apache.org/licenses/LICENSE-2.0                                                            ║
   ║                                                                                                           ║
   ║ Unless required  by applicable law  or agreed to in  writing, software  distributed under the  License is ║
   ║ distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. ║
   ║ See the License for the specific language governing permissions and limitations under the License.        ║
   ╚═══════════════════════════════════════════════════════════════════════════════════════════════════════════╝
*/
package exoskeleton

import collection.JavaConverters._

import scala.util._, scala.annotation.tailrec

import java.io._

sealed trait InstallException extends Exception with Product with Serializable
case class EnvException(envVar: String) extends InstallException
case class NoInstallDirs() extends InstallException
case class CouldNotInstall() extends InstallException

object Log {
  val logFile = new BufferedWriter(new FileWriter(new File(".completion.log"), true))
  def info(str: String): Unit = {
    logFile.write(str)
    logFile.write('\n')
    logFile.flush()
  }
}

object Generate extends Application {
  def main(ctx: Context): Io[Exit] = for {
    files <- install(ctx)
    _     <- Io.out.println(s"Installed ${files.mkString(", ")}")
  } yield Exit(0)

  def install(ctx: Context): Io[Set[String]] = ctx.args match {
    case cmd :: Shell(shell) :: dir :: Nil =>
      Io.from(shell.install(cmd, ctx.environment, Some(dir))).map(Set(_))
    case cmd :: Shell(shell) :: Nil =>
      Io.from(shell.install(cmd, ctx.environment)).map(Set(_))
    case cmd :: Nil =>
      Io(Shell.all.map(_.install(cmd, ctx.environment).toOption.to[Set]).flatten.to[Set])
    case _ =>
      Io.pure(Set())
  }

  def complete(cli: Cli): Completions = cli.index match {
    case 1 => Completions(Nil, Some("Please specify the command to complete"))
    case 2 => Completions(Shell.all.map { shell =>
                CompDef(shell.shell, Some(shell.description), false, false)
              })
    case 3 => Completions(Nil, Some("Please specify the directory in which to install the file"))
    case _ => Completions(Nil, Some("No more parameters"))
  }
}

case class Context(args: List[String],
                   environment: Map[String, String],
                   properties: Map[String, String])

case class Cli(command: String,
               args: List[String],
               environment: Map[String, String],
               properties: Map[String, String],
               index: Int) {
  def currentArg: String = args.lift(index).getOrElse("")
}

case class Exit(status: Int)

case class Completions(defs: List[CompDef], title: Option[String] = None)


case class CompDef(word: String,
                   description: Option[String] = None,
                   hidden: Boolean = false,
                   incomplete: Boolean = false)

object AsInt { def unapply(str: String): Option[Int] = Try(str.toInt).toOption }

object Shell {
  val all: List[Shell] = List(Zsh, Bash, Fish)
  def unapply(string: String): Option[Shell] = all.find(_.shell == string)
}

abstract class Shell(val shell: String) {
  def serialize(cli: Cli, completions: Completions): Stream[String]
  def script(cmd: String): String
  def filename(cmd: String): String
  def install(cmd: String, env: Map[String, String], dir: Option[String] = None): Try[String]
  def description: String
  
  protected def generalInstall(cmd: String, dirs: List[File]): Try[String] = {
    for {
      dir   <- dirs.headOption.toRight(NoInstallDirs())
      file   = new File(dir, filename(cmd))
      out    = new BufferedWriter(new FileWriter(file))
      _      = out.write(script(cmd))
      _      = out.close()
    } yield file.getAbsolutePath
  }.toTry

}

object Zsh extends Shell("zsh") {
  private def maxLength(defs: Seq[CompDef]): Int = (0 +: defs.map(_.word.length)).max
  def description: String = "ZSH shell"
  private def describe(width: Int, word: String, desc: Option[String]): String =
    desc.fold(word) { desc => s"${word.padTo(width, ' ')}  -- $desc" }

  private val fpathEnv = "FPATH"

  def install(cmd: String, env: Map[String, String], dir: Option[String]): Try[String] = { for {
    fpath <- env.get(fpathEnv).toRight(EnvException(fpathEnv))
    dirs  <- Right(fpath.split(":").to[List].map(new File(_)).filter(_.canWrite))
    file  <- generalInstall(cmd, dir.map(new File(_)).to[List] ++ dirs).toOption.toRight(CouldNotInstall())
  } yield file }.toTry

  def serialize(cli: Cli, completions: Completions): Stream[String] = {
    val width = maxLength(completions.defs.filter(_.word.startsWith(cli.currentArg)))
    (completions.title.map(List("", "-X", _).mkString("\t")) ++ completions.defs.flatMap {
      case CompDef(word, desc, hidden, incomplete) =>
        val hide = if(hidden) List("-n") else Nil
        List(
          List(describe(width, word, desc), "-d", "desc") ::: hide ::: List("--", word),
          if(incomplete) List("", "-n") ::: List("--", s"$word.") else Nil
        ).map(_.mkString("\t"))
    }).to[Stream]
  }
  
  def script(command: String): String =
    s"""|#compdef $command
        |
        |_$command() {
        |  OLD_IFS=$$IFS IFS=$$'\\t'
        |  $command '{completion}' 'zsh' "$$(($$CURRENT - 1))" -- $$words | while read -r -A LINE; do
        |    desc=($${LINE[1]})
        |    compadd -Q $${LINE:1}
        |  done
        |  IFS=$$OLD_IFS
        |}
        |
        |_$command
        |
        |return 0
        |""".stripMargin
  
  def filename(cmd: String): String = s"_$cmd"
}

object Bash extends Shell("bash") {

  def description = "The Bourne Again SHell"

  def serialize(cli: Cli, completions: Completions): Stream[String] = {
    Stream(completions.defs.filter(_.word.startsWith(cli.currentArg)).collect {
      case CompDef(word, desc, false, _) => word
    }.mkString("\t"))
  }
  
  def script(cmd: String): String =
    s"""|_${cmd}_complete() {
        |  DATA=$$($cmd '{completion}' 'bash' $$COMP_CWORD -- $$COMP_LINE )
        |  OLD_IFS=$$IFS
        |  IFS=$$'\t'
        |  COMPREPLY=($$DATA)
        |  IFS=$$OLD_IFS
        |}
        |
        |complete -F _${cmd}_complete $cmd
        |""".stripMargin

  def filename(cmd: String): String = s"_$cmd"
  
  def install(cmd: String, env: Map[String, String], dir: Option[String]): Try[String] = { for {
    home <- env.get("HOME").toRight(EnvException("HOME"))
    dirs <- Right(List(new File(new File(home), ".bash_completion.d"),
                new File("/etc/bash_completion.d")).filter(_.canWrite))
    file <- generalInstall(cmd, dir.map(new File(_)).to[List] ++ dirs).toOption.toRight(CouldNotInstall())
  } yield file }.toTry
}

object Fish extends Shell("fish") {

  def description: String = "The Fish Shell"

  def serialize(cli: Cli, completions: Completions): Stream[String] = completions.defs.to[Stream].map {
    case CompDef(word, desc, hidden, incomplete) => s"$word\t${desc.getOrElse("")}"
  }
  
  def script(cmd: String): String =
    s"""|for line in ($cmd '{completion}' 'fish' (count (commandline -o)) -- (commandline -o))
        |  complete -f -c $cmd -a (string escape $$line[1])
        |end
        |""".stripMargin

  def filename(cmd: String): String = s"$cmd.fish"

  private val fpathEnv = "fish_complete_path"

  def install(cmd: String, env: Map[String, String], dir: Option[String]): Try[String] = { for {
    fpath <- env.get(fpathEnv).toRight(EnvException(fpathEnv))
    dirs  <- Right(fpath.split(":").to[List].map(new File(_)).filter(_.canWrite))
    file  <- generalInstall(cmd, dir.map(new File(_)).to[List] ++ dirs).toOption.toRight(CouldNotInstall())
  } yield file }.toTry
}

case class ParamMap(args: String*) {

  case class Arg(value: String)

  case class Part(no: Int, start: Int, end: Int) { def apply() = args(no).substring(start, end) }

  case class Parameter(key: Part, values: Vector[Part] = Vector()) {
    override def toString = {
      val prefix = if(key().length == 1) "-" else "--"
      s"$prefix${key()} ${values.map(_()).mkString(" ")}"
    }
  }
}