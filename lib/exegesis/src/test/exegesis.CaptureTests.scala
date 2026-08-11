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
package exegesis

import soundness.*

import strategies.throwUnsafely

// The handles lent to a handler — the document, the workspace and the client reached through
// it — are capabilities scoped to one dispatch; capture checking prevents any of them (or the
// registry lent to the registration block) being retained beyond its scope.
object CaptureTests extends Suite(m"Handle confinement tests"):
  import Lsp.*

  def run(): Unit =
    test(m"the document cannot be stashed in an outer variable"):
      demilitarize:
        def attempt(using registry: Lsp.Registry^): Unit =
          var stash: () => Text = () => t""

          hover:
            stash = () => document.text
            Unset

          ()
    . assert(_.nonEmpty)

    test(m"the client cannot be stashed in an outer variable"):
      demilitarize:
        def attempt(using registry: Lsp.Registry^): Unit =
          var stash: () => Unit = () => ()

          opened:
            stash = () => client.logMessage(t"late")

          ()
    . assert(_.nonEmpty)

    test(m"the workspace handle cannot be stashed in an outer variable"):
      demilitarize:
        def attempt(using registry: Lsp.Registry^): Unit =
          var stash: () => List[Text] = () => Nil

          opened:
            stash = () => workspace.documents

          ()
    . assert(_.nonEmpty)

    test(m"the registry cannot escape the registration block"):
      demilitarize:
        def attempt()(using Stdio, Monitor, Probate): Optional[Lsp.Registry] =
          var stash: Optional[Lsp.Registry] = Unset
          Lsp.listen(t"escape"):
            stash = summon[Lsp.Registry]
          stash
    . assert(_.nonEmpty)

    // A pure snapshot taken from a handle may escape by design: only the handle is confined.
    test(m"a pure text snapshot may leave the handler"):
      demilitarize:
        def attempt(using registry: Lsp.Registry^): Unit =
          var stash: Text = t""

          opened:
            stash = document.text

          ()
    . assert(_.isEmpty)
