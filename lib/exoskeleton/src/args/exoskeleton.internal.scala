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
package exoskeleton

import scala.collection.immutable as sci
import scala.quoted.*

object internal:
  // Reify a union of `Status` singleton types as the list of their values. Each member of the
  // union is a `TermRef` — the singleton type of the stable value the application declared —
  // so the status values are recovered by referring to those terms again. `Nothing`, the
  // instantiation when a block returns no status at all, contributes nothing.
  def admissible[result: Type](using Quotes): Expr[result is Status.Admissible] =
    import quotes.reflect.*

    def decompose(repr: TypeRepr): sci.List[TypeRepr] = repr.dealias match
      case OrType(left, right) => decompose(left) ++ decompose(right)
      case other               => sci.List(other)

    // A status declared as `object CannotConnect extends Status(…)` appears in the union as a
    // `TypeRef` to its module class, and one referred to by a stable path as a `TermRef`; both
    // yield the term to refer to again. Any other `Termination` — `Exit` and its cases, or
    // `Nothing` when a block never returns normally — sets an exit code without a meaning to
    // document, and so contributes nothing. A non-singleton `Status` member has no value to
    // recover: it means the union has been widened (soundness#1811), so the error is raised
    // here rather than letting the statuses silently vanish from the documentation.
    val statusType = TypeRepr.of[Status]
    val terminationType = TypeRepr.of[rudiments.Termination]

    val values: sci.List[Expr[Status]] =
      decompose(TypeRepr.of[result]).distinct.flatMap: repr =>
        if repr =:= TypeRepr.of[Nothing] then sci.Nil
        else if repr <:< statusType then repr match
          case ref: TermRef => sci.List(Ref.term(ref).asExprOf[Status])

          case other =>
            val symbol = other.termSymbol

            if symbol.exists then sci.List(Ref(symbol).asExprOf[Status]) else
              report.errorAndAbort
               ( ("exoskeleton: every status an execute block returns must be a singleton object ": String)
                 + ("extending Status, but the result type includes ": String)+other.show+(", which is not ": String)
                 + ("a singleton. Declare each status as a top-level object; if this type is ": String)
                 + "Status itself, the union of statuses has been widened (soundness#1811)." )
        else if repr <:< terminationType then sci.Nil
        else
          report.errorAndAbort
           ( ("exoskeleton: an execute block must return a Termination, such as an Exit or a ": String)
             + ("Status, but the result type includes ": String)+repr.show+", which is neither." )

    '{
        new Status.Admissible:
          type Self = result
          def statuses: List[Status] = List.from(${Expr.ofList(values)})
     }
