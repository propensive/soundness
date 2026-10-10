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
package aperture

import prepositional.*

// Refines a type's `Grants` member; `Mode granting Grant.Read` reads naturally.
infix type granting [refined <: { type Grants <: Grant }, grants <: Grant] =
  refined { type Grants = grants }

val Read: Mode granting Grant.Read = new Mode { type Grants = Grant.Read }

// `Write` alone does not confer read access: open with `Read & Write` for both.
val Write: Mode granting Grant.Write = new Mode { type Grants = Grant.Write }

val Exclusive: Mode granting Grant.Exclusive = new Mode { type Grants = Grant.Exclusive }

extension [target](value: target)
  // Opens `value` in form `form`: `path.open[Directory]()`, or `path.open[File](Read & Write)`.
  // The form may be omitted when the target has a unique `Openable` instance; with several,
  // the ambiguity error lists the alternatives. Flags — of the instance's `Operand` type, so
  // target-specific — follow the mode. The handle is provided as a contextual value to the
  // block, and capture checking prevents it, or anything derived from it, from escaping.
  def open[form](using o: (target is Openable in form)^)
  :   (Opener { val openable: o.type })^{o} =

    Opener(o, value)

  // Creates `value` in form `form`: `path.create[Directory]()` for an empty artifact, or
  // `path.create[Tar]() { ... }` to author its contents, committed when the scope closes.
  def create[form](using c: (target is Creatable in form)^)
  :   (Creator { val creatable: c.type })^{c} =

    Creator(c, value)

  // Archives `value` in form `form`: `directory.archive[Tar]()` yields the tarball of a
  // directory tree as a value; flags, of the instance's `Operand` type, tune the archiving.
  def archive[form](using a: (target is Archivable in form)^)
  :   (Archiver { val archivable: a.type })^{a} =

    Archiver(a, value)

  // Open a session on the target: the handle (of whatever kind the target's
  // `Sessional` instance provides) is available within `lambda`, and is
  // disposed of when the scope ends. Values borrowing the session cannot escape
  // it. Inline, with the instance as a capturing using-parameter ahead of the
  // lambda: the instance's capabilities and its concrete `Result` type flow
  // through from the use site, where a non-inline def would mint fresh roots
  // the capability-carrying instance cannot flow into.
  transparent inline def session[result](using sessional: (target is Sessional)^)
    ( lambda: (session: sessional.Result) ?=> result )
  :   result =

    sessional.session(value)(lambda)
