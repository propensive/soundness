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
┃    Soundness, version 0.63.0.                                                                    ┃
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
package ambience

import scala.caps

import scala.language.dynamics

import anticipation.*
import distillate.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

object Variable extends Protovariable:
  // Each path-typed `Variable` retains its `Instantiable` evidence, which shares the
  // instance's given-resolution lifetime; laundered pure per the codec-thunk seal pattern
  // (see rep/DECISIONS.md). A capturing result would flow into `Environment`'s reader
  // parameter, which must stay pure: it is summoned inside staged quotes (ethereal daemon
  // `cli` blocks). These two helpers hold that laundering once for all the givens below.
  private def pathVariable[name <: Label, path]
    ( instantiable: (path is Instantiable across Paths from Text)^ )
  :   Variable[name, path] =

    caps.unsafe.unsafeAssumePure(instantiable(_))

  private def pathListVariable[name <: Label, path]
    ( instantiable: (path is Instantiable across Paths from Text)^, system: System )
  :   Variable[name, List[path]] =

    caps.unsafe.unsafeAssumePure:
      _.cut(system(t"path.separator").or(t":")).map(instantiable(_))

  given path: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^, system: System )
  =>  Variable["path", List[path]] =

    pathListVariable(instantiable, system)


  given xdgDataDirs: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^, system: System )
  =>  Variable["xdgDataDirs", List[path]] =

    pathListVariable(instantiable, system)


  given xdgConfigDirs: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^, system: System )
  =>  Variable["xdgConfigDirs", List[path]] =

    pathListVariable(instantiable, system)


  given xdgDataHome: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["xdgDataHome", path] =

    pathVariable(instantiable)


  given xdgConfigHome: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["xdgConfigHome", path] =

    pathVariable(instantiable)


  given xdgStateHome: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["xdgStateHome", path] =

    pathVariable(instantiable)


  given xdgCacheHome: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["xdgCacheHome", path] =

    pathVariable(instantiable)


  given xdgRuntimeDir: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["xdgRuntimeDir", path] =

    pathVariable(instantiable)


  given home: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["home", path] =

    pathVariable(instantiable)


  given mail: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["mail", path] =

    pathVariable(instantiable)


  given shell: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["shell", path] =

    pathVariable(instantiable)


  given oldpwd: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["oldpwd", path] =

    pathVariable(instantiable)


  given windowid: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["windowid", path] =

    pathVariable(instantiable)


  given editor: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["editor", path] =

    pathVariable(instantiable)


  given pager: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["pager", path] =

    pathVariable(instantiable)


  given sshAuthSock: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["sshAuthSock", path] =

    pathVariable(instantiable)


  given manpager: [path]
  =>  ( instantiable: (path is Instantiable across Paths from Text)^ )
  =>  Variable["manpager", path] =

    pathVariable(instantiable)


  given columns: (decodable: (Int is Decodable in Text)^)
  =>  ((Variable["columns", Int])^{decodable}) =
    _.as[Int]
  given lang: Variable["lang", Text] = identity(_)
  given display: Variable["display", Text] = identity(_)
  given term: Variable["term", Text] = identity(_)

trait Variable[alias <: Label, +variable]:
  inline def defaultName: Text = name.or(valueOf[alias].tt.uncamel.snake.upper)
  def name: Optional[Text] = Unset
  def read(value: Text): variable
