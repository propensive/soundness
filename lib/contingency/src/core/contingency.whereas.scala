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
package contingency

import language.experimental.pureFunctions

import java.util.concurrent.atomic as juca

import scala.language.unsafeNulls
import scala.util.boundary

import fulminate.*


final class Deferred[result, error <: Exception](body: Tactic[error] ?=> result):
  def apply()(using Tactic[error]): result = body


class Whereas[lambda[_]](val handler: PartialFunction[Exception, Any])


object Whereas:
  final case class Escape[+result](value: result) extends Exception:
    override def fillInStackTrace(): Throwable = this


  class AccrueTactic[error <: Exception, accrual <: Exception]
    ( initial: accrual, combine: (accrual, Exception) => accrual )
    ( using val diagnostics: Diagnostics )
  extends Tactic[error]:

    private val ref: juca.AtomicReference[accrual] = juca.AtomicReference(initial)

    def record(error: Diagnostics ?=> error): Unit =
      ref.updateAndGet(curr => combine(curr.nn, error(using diagnostics)))

    def abort(error: Diagnostics ?=> error): Nothing =
      import scala.unsafeExceptions.canThrowAny
      record(error)
      throw accumulated

    def certify(): Unit = ()

    def accumulated: accrual = ref.get().nn
    def changed: Boolean = ref.get().nn != initial


  class EscapeTactic[result](label: boundary.Label[result]) extends Tactic[Escape[result]]:
    def diagnostics: Diagnostics = Diagnostics.omit

    def record(error: Diagnostics ?=> Escape[result]): Unit =
      boundary.break(error(using diagnostics).value)(using label)

    def abort(error: Diagnostics ?=> Escape[result]): Nothing =
      boundary.break(error(using diagnostics).value)(using label)

    def certify(): Unit = ()


  extension [lambda[_]](inline w: Whereas[lambda])

    inline def mitigate[result](inline body: lambda[result]): result =
      ${contingency.internal.mitigateBody[lambda, result]('w, 'body)}

    inline def recover[result](inline body: lambda[result]): result =
      ${contingency.internal.recoverBody[lambda, result]('w, 'body)}

    inline def accrue[accrual <: Exception, result](initial: accrual)
      ( combine: (accrual, Exception) => accrual )
      ( inline body: lambda[result] )
      ( using outer: Tactic[accrual], diagnostics: Diagnostics )
    :   result =

      $ {
          contingency.internal.accrueBody[accrual, lambda, result]
            ( 'w, 'initial, 'combine, 'body, 'outer, 'diagnostics )
        }
