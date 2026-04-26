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
┃    Soundness, version 0.54.0.                                                                    ┃
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
package probably

import ambience.*, environments.java
import anticipation.*
import contingency.*
import gossamer.*
import spectacular.*
import turbulence.*
import vacuous.*

object GithubActions:
  def workspaceRelative(path: Text): Text =
    safely(Environment.githubWorkspace[Text]).let: workspace =>
      if path.starts(workspace) && path.length > workspace.length
         && path.s.charAt(workspace.length) == '/'
      then path.skip(workspace.length + 1)
      else path

    . or(path)


  def error
       ( message: Text,
         file:    Optional[Text] = Unset,
         line:    Optional[Int]  = Unset,
         title:   Optional[Text] = Unset )
       (using Stdio)
  :     Unit =
    emit(t"error", file, line, title, message)


  def warning
       ( message: Text,
         file:    Optional[Text] = Unset,
         line:    Optional[Int]  = Unset,
         title:   Optional[Text] = Unset )
       (using Stdio)
  :     Unit =
    emit(t"warning", file, line, title, message)


  def notice
       ( message: Text,
         file:    Optional[Text] = Unset,
         line:    Optional[Int]  = Unset,
         title:   Optional[Text] = Unset )
       (using Stdio)
  :     Unit =
    emit(t"notice", file, line, title, message)


  def debug(message: Text)(using Stdio): Unit =
    Out.println(t"::debug::${escape(message)}")


  def group(title: Text)(using Stdio): Unit =
    Out.println(t"::group::${escape(title)}")


  def endGroup()(using Stdio): Unit = Out.println(t"::endgroup::")


  inline def grouped[result](title: Text)(inline block: => result)(using Stdio): result =
    group(title)
    try block finally endGroup()


  private def escape(text: Text): Text =
    text.sub(t"%", t"%25").sub(t"\r", t"%0D").sub(t"\n", t"%0A")


  private def escapeProperty(text: Text): Text =
    escape(text).sub(t",", t"%2C").sub(t":", t"%3A")


  private def emit
       ( kind:    Text,
         file:    Optional[Text],
         line:    Optional[Int],
         title:   Optional[Text],
         message: Text )
       (using Stdio)
  :     Unit =

    val props = List
                  ( file.let(workspaceRelative).let(path => t"file=${escapeProperty(path)}").option,
                    line.let(value => t"line=${value.show}").option,
                    title.let(text => t"title=${escapeProperty(text)}").option )
                . flatten

    val propsText = if props.isEmpty then t"" else t" ${props.join(t",")}"
    Out.println(t"::$kind$propsText::${escape(message)}")
