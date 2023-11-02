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
import fulminate.*
import turbulence.*
import ambience.*, environments.jvm, systemProperties.jvm
import galilei.*, filesystemOptions.{dereferenceSymlinks, createNonexistent, createNonexistentParents,
    doNotOverwritePreexisting}

object Install:
  def apply
      (shell: Shell, command: Text, global: Boolean)
      (using Raises[PathError], Raises[IoError], Raises[OverwriteError], Raises[StreamCutError])
      : List[Message] =
    val path: Path = scriptPath(shell, command, global)
    
    script(shell, command).sysBytes.writeTo(path.make[File]())
    
    List(msg"Completion script for ${shell.show.lower} installed to ${path}.") ++ messages(shell, global)

  def scriptPath(shell: Shell, command: Text, global: Boolean)(using Raises[PathError], Raises[IoError]): Path =
    val xdg = Xdg()
    val home = summon[HomeDirectory].path
    
    if global then shell match
      case Shell.Bash => xdg.dataDirs.last / p"bash-completion" / p"completions" / PathName(command)
      case Shell.Fish => xdg.dataDirs.last / p"fish" / p"vendor_completions.d" / PathName(t"$command.fish")
      case Shell.Zsh  => xdg.dataDirs.last / p"zsh" / p"site-functions" / PathName(t"_$command")
    else shell match
      case Shell.Bash => xdg.dataHome / p"bash-completion" / p"completions" / PathName(command)
      case Shell.Fish => xdg.configHome / p"fish" / p"completions" / PathName(t"$command.fish")
      
      case Shell.Zsh =>
        val ohMyZsh = home / p".oh-my-zsh"
        
        if ohMyZsh.exists() then ohMyZsh / p"completions" / PathName(t"_$command")
        else home / p".zsh" / p"completion" / PathName(t"_$command")

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
          |  COMPREPLY=($$(compgen -W $$output -- "$${COMP_WORDS[$$COMP_CWORD]}"))
          |}
          |complete -F _${command}_complete $command
          |""".s.stripMargin.tt