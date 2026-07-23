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
package contingency

import scala.caps

import scala.language.experimental.pureFunctions

import scala.util.boundary

import fulminate.*

// Captures a `recover` handler: cases that map each covered error type to a *replacement value* of
// the protected block's result type. `recover { case FooError(n) => fallback }.protect { … }` runs
// the block and, on the first covered error, escapes with the case body's value instead.
object Recovery:
  object Escape:
    def apply[result](value: result): Escape[result] = new Escape(value.asInstanceOf[AnyRef])

  // A case body's recovered value, escaping the block via `boundary`. Pure by an
  // `AnyRef` rim: the payload never leaves a scope inside the exception — `record`
  // unwraps it and escapes through the capture-checked `boundary.break` — so the
  // briefcase carries it as a neutral reference between two frames of one call.
  final class Escape[+result](private val value0: AnyRef) extends Exception, caps.Pure:
    def value: result = value0.asInstanceOf[result]
    override def fillInStackTrace(): Throwable = this

  class EscapeTactic[result](label: boundary.Label[result]) extends Tactic[Escape[result]]:
    def diagnostics: Diagnostics = Diagnostics.omit

    def record(error: Diagnostics ?=> Escape[result]): Unit =
      boundary.break(error(using diagnostics).value)(using label)

    def abort(error: Diagnostics ?=> Escape[result]): Nothing =
      boundary.break(error(using diagnostics).value)(using label)

    def certify(): Unit = ()

  extension [lambda[_]](inline recovery: Recovery[lambda])
    inline def protect[result](inline body: lambda[result]): result =
      ${contingency.internal.recoverBody[lambda, result]('recovery, 'body)}

class Recovery[lambda[_]](val handler: PartialFunction[Exception, Any])
