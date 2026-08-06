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
package anthology

import anticipation.*
import digression.*
import fulminate.*

object LinkError:
  enum Reason(val number: Int) extends Clarification:
    case Failed(trace: StackTrace)                  extends Reason(1)
    case NoEntryPoint                               extends Reason(3)
    case ManyEntryPoints                            extends Reason(4)
    case NoPath(source: Text, target: Text)         extends Reason(5)
    case AmbiguousPath(source: Text, target: Text)  extends Reason(6)
    case InapplicableSetting                        extends Reason(7)
    case DuplicateEdge(source: Text, target: Text)  extends Reason(8)
    case CyclicToolchain                            extends Reason(9)
    case UnexpectedInput(format: Text)              extends Reason(10)
    case CompilationFailed(errors: Int)             extends Reason(11)
    case MissingSetting(name: Text)                 extends Reason(12)
    case Packaging(detail: Text)                    extends Reason(13)

  given communicable: Reason is Communicable =
    case Reason.Failed(_)       => m"the linker terminated abnormally"
    case Reason.NoEntryPoint    => m"a native executable requires exactly one entry point"
    case Reason.ManyEntryPoints => m"an executable JAR permits at most one entry point"

    case Reason.NoPath(source, target) =>
      m"the toolchain has no path from $source to $target"

    case Reason.AmbiguousPath(source, target) =>
      m"""
        the toolchain has several shortest paths from $source to $target, so an intermediate
        format must be produced explicitly
      """

    case Reason.InapplicableSetting =>
      m"a setting applies to no format produced on the path"

    case Reason.DuplicateEdge(source, target) =>
      m"the toolchain declares more than one edge from $source to $target"

    case Reason.CyclicToolchain => m"the toolchain's edges form a cycle"

    case Reason.UnexpectedInput(format) =>
      m"the tool producing $format cannot consume the content it was given"

    case Reason.CompilationFailed(errors) => m"compilation failed with $errors errors"
    case Reason.MissingSetting(name)      => m"the setting $name is required but unspecified"
    case Reason.Packaging(detail)         => m"packaging failed: $detail"

case class LinkError(reason: LinkError.Reason)(using Diagnostics)
extends Error(443, reason.number)(m"linking failed because $reason")
