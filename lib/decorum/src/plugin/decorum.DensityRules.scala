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
package decorum

import scala.collection.mutable

object DensityRules:
  // R312 — lambda layout, four sub-rules:
  //   312.1 named single-line lambda using `(…)` (must be `{…}` or `: …`)
  //   312.2 named single-line `{…}` at end-of-line (must be `: …`)
  //   312.3 multi-line lambda using `{…}` or `(…)` (must be `: …`)
  //   312.4 anonymous (placeholder) lambda using `{…}` or `: …` (must be `(…)`)
  object LambdaLayout extends Rule:
    def id: String = "312"
    def principle: Principle = Principle.Density

    def check(ctx: Context): List[Violation] =
      import Lambdas.Opener
      val file = ctx.file
      val out  = mutable.ListBuffer[Violation]()

      ctx.lambdaSites.foreach: s =>
        // Multi-line wins regardless of parameter shape — only a colon-arg
        // body can house a multi-line lambda cleanly.
        if s.isMultiLine then
          if s.opener != Opener.Colon then
            out +=
              Violation
                ( file, s.openerLine, s.openerCol, "312.3",
                  "multi-line lambda must use a colon-arg `f: x => …` form, "
                    +s"not `${s.opener.toString.toLowerCase}`" )
        else if s.isAnonymous then
          if s.opener != Opener.Paren then
            out +=
              Violation
                ( file, s.openerLine, s.openerCol, "312.4",
                  "anonymous (`_`-)lambda must be wrapped in `(…)`, not "
                    +s"`${s.opener.toString.toLowerCase}`" )
        else // named, single-line
          if s.opener == Opener.Paren then
            out +=
              Violation
                ( file, s.openerLine, s.openerCol, "312.1",
                  "named-parameter lambda must be wrapped in `{…}`, not `(…)`"
                    +(if s.lastOnLine
                      then " (or use `f: x => …` since the lambda is last on the line)"
                      else "") )
          else if s.opener == Opener.Brace && s.lastOnLine then
            out +=
              Violation
                ( file, s.openerLine, s.openerCol, "312.2",
                  "lambda is the last thing on its line; prefer `f: x => …` "
                    +"over `f { x => … }`" )

      out.toList
