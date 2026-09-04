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
package surveillance

import soundness.*

import errorDiagnostics.emptyDiagnostics
import filesystemOptions.createNonexistentParents
import filesystemOptions.failOnPreexisting
import strategies.throwUnsafely
import systems.javaBaseSystem
import temporaryDirectories.systemTemporaryDirectory

import filesystemBackends.javaBaseFilesystem

object Tests extends Suite(m"Surveillance tests"):
  def run(): Unit =
    test(m"Watching a path beneath a nonexistent directory raises a Watch.Error"):
      val target = t"/surveillance-nonexistent-parent-9d3f17/child".as[Path on Local]
      capture[Watch.Error](target.open[Watch]() { () }).reason

    . assert(_ == Watch.Error.Reason.Nonexistent)

    test(m"Watching a path whose parent is a regular file raises a Watch.Error"):
      val file = temporaryDirectory[Path on Local]/Uuid().show
      file.create[File]()
      capture[Watch.Error]((file/Uuid().show).open[Watch]() { () }).reason

    . assert(_ == Watch.Error.Reason.NotDirectory)

    test(m"A scoped watch on an untouched directory yields a terminating, empty stream"):
      val directory = temporaryDirectory[Path on Local]/Uuid().show
      directory.create[Directory]()

      // Before `unregister` stopped the spool this `to(List)` would block forever. The scoped
      // form cannot express a post-scope read (the handle is confined), so this exercises the
      // `Watch` layer directly.
      val watchSet = Watch(List(directory))
      watchSet.unregister()
      watchSet.stream.stdlib.to(List)

    . assert(_ == Nil)

    test(m"The polling watcher reports a newly-created file"):
      given Watcher = watchers.polling(0.05*Second)
      val directory = temporaryDirectory[Path on Local]/Uuid().show
      directory.create[Directory]()
      val leaf: Text = Uuid().show

      directory.open[Watch](): watcher ?=>
        (directory/leaf).create[File]()

        watcher.stream.stdlib.head match
          case NewFile(_, file) => file == leaf
          case _                => false

    . assert(_ == true)
