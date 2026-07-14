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
package archimedes

import scala.quoted.*

import anticipation.*
import contingency.*
import fulminate.*
import gigantism.*
import gossamer.*
import prepositional.*
import rudiments.*
import vacuous.*

import Mathml.*

object internal:
  // The macro behind the `ergo""` interpolator. It recovers the literal parts and
  // converts each `$`-substitution (any value that is `Encodable in Math`) into a
  // single atom node, parses the whole expression *at compile time* so a malformed
  // literal fails compilation with the parser's own diagnostic, then emits runtime
  // code that re-parses with the substitutions woven in.
  def ergoInterpolator[parts <: Tuple: Type](insertions: Expr[Seq[Any]]): Macro[Math] =
    import quotes.reflect.*

    def recur[tuple: Type](strings: List[String]): List[String] = Type.of[tuple] match
      case '[head *: tail] => recur[tail](TypeRepr.of[head].literal[String].vouch :: strings)
      case _               => strings

    val parts = recur[parts](Nil)

    val insertionExprs: List[Expr[Any]] = insertions.absolve match
      case Varargs(exprs) => exprs.to(List)

    val atoms: List[Expr[Mathml]] = insertionExprs.map: insertion =>
      insertion.absolve match
        case '{$value: valueType} => Expr.summon[(? >: valueType) is Encodable in Math] match
          case Some('{$encodable: Encodable}) =>
            '{Mathml.atom($encodable.encoded($value))}.asExprOf[Mathml]

          case _ =>
            val kind = TypeRepr.of[valueType].widen.show
            val position = insertion.asTerm.underlyingArgument.pos
            halt(m"a $kind cannot be embedded in ergo: it is not Encodable in Math", position)

    locally:
      import strategies.throwUnsafely
      import errorDiagnostics.emptyDiagnostics

      try Ergo.interpolate(parts.map(_.tt), List.fill(atoms.length)(Mi(t"?")))
      catch case error: ErgoError =>
        halt(m"the ergo expression could not be parsed because ${error.reason}")

    val partExprs: Expr[Seq[Text]] = Expr.ofSeq(parts.map { part => '{${Expr(part)}.tt} })

    '{Ergo.unsafeInterpolate($partExprs, ${Expr.ofSeq(atoms)})}
