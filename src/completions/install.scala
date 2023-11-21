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

import spectacular.*
import gossamer.*
import anticipation.*, fileApi.galileiApi
import rudiments.*, homeDirectories.default
import serpentine.*, hierarchies.unix
import perforate.*
import guillotine.*
import fulminate.*
import eucalyptus.*
import turbulence.*
import ambience.*, environments.jvm, systemProperties.jvm
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    doNotOverwritePreexisting}

object TabCompletions:
  def install
      (shells: Shell*)
      (using service: ShellContext)
      (using WorkingDirectory, Log, Raises[ExecError], Raises[PathError], Raises[IoError],
          Raises[StreamCutError], Raises[OverwriteError], Effectful)
      : List[Message] =
    val scriptPath = sh"sh -c 'command -v ${service.scriptName}'".exec[Text]()
    val command: Text = service.scriptName
    
    if scriptPath.decodeAs[Path] == service.script
    then
      (if shells.isEmpty then List(Shell.Zsh, Shell.Bash, Shell.Fish) else shells.to(List)).flatMap:
        case Shell.Zsh =>
          val dirs = sh"zsh -c 'source ~/.zshrc 2> /dev/null; printf %s, $$fpath'".exec[Text]().cut(t",").filter(_.trim != t"").map(_.decodeAs[Path]).reverse
          install(Shell.Zsh, command, PathName(t"_$command"), dirs)
        
        case Shell.Bash =>
          install(Shell.Bash, command, PathName(command), List(Xdg.dataDirs.last / p"bash-completion" /
              p"completions", Xdg.dataHome / p"bash-completion" / p"completions"))

        case Shell.Fish =>
          install(Shell.Fish, command, PathName(t"$command.fish"), List(Xdg.dataDirs.last / p"fish" /
              p"vendor_completions.d", Xdg.configHome / p"fish" / p"completions"))
    
    else List(
      msg"The ${service.scriptName} command is not on the PATH, so completions scripts cannot be installed."
    )

  def install
      (shell: Shell, command: Text, scriptName: PathName[GeneralForbidden], dirs: List[Path])
      (using Raises[PathError], Raises[IoError], Raises[OverwriteError], Raises[StreamCutError], Effectful)
      : List[Message] =

    dirs.find { dir => dir.exists() && dir.as[Directory].writable() }.map: dir =>
      val path = dir / scriptName
      if path.exists()
      then List(msg"A ${shell.show.lower} completion script already exists for ${command}.")
      else
        script(shell, command).sysBytes.writeTo(path.make[File]())
        List(msg"Completion script for ${shell.show.lower} installed into ${path}.")
    .getOrElse:
      List(msg"No writable install location could be found for completions")

  def messages(shell: Shell, global: Boolean): List[Message] =
    if shell == Shell.Zsh && !global
    then List(msg"""Make sure that your ${t"~/.zshrc"} file contains the following lines:

      fpath=(~/.zsh/completion $$fpath)
      autoload -U compinit
      compinit
    """)
    else Nil

  def script(shell: Shell, command: Text): Text = shell match
    case Shell.Zsh =>
      t"""|#compdef $command
          |local -a ln
          |_$command() {
          |  oldIfs=$$IFS IFS=$$'\t'
          |  $command '{completions}' zsh "$$((CURRENT-1))" "$${#PREFIX}" -- $$words | while read -r -A ln
          |  do
          |    desc=($${ln[1]})
          |    compadd -d desc -Q $${ln:1}
          |  done
          |  IFS=$$oldIfs
          |}
          |_$command
          |return 0
          |""".s.stripMargin.tt
    
    case Shell.Fish =>
      t"""|function completions
          |  set position (count (commandline --tokenize --cut-at-cursor))
          |  ${command} '{completions}' fish $$position (commandline -C -t) -- (commandline -o)
          |end
          |complete -f -c $command -a '(completions)'
          |""".s.stripMargin.tt

    case Shell.Bash =>
      t"""|_${command}_complete() {
          |  output="$$(${command} '{completions}' bash $$COMP_CWORD 0 -- $$COMP_LINE)"
          |  COMPREPLY=($$output)
          |}
          |complete -F _${command}_complete $command
          |""".s.stripMargin.tt