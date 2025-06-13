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
┃    Soundness, version 0.34.0.                                                                    ┃
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
package superlunary

import ambience.*, systemProperties.jre
import anthology.*
import anticipation.*
import contingency.*
import distillate.*
import fulminate.*
import gossamer.*
import hellenism.*, classloaders.threadContext
import inimitable.*
import jacinta.*
import prepositional.*
import rudiments.*, temporaryDirectories.systemProperties
import spectacular.*

import scala.quoted.*

case class Example(name: Text, count: Long)

@main
def run(): Unit =
  given jsonError: Tactic[JsonError] = strategies.throwUnsafely
  given compilerError: Tactic[CompilerError] = strategies.throwUnsafely

  inline given [value] => Quotes => (refs: References[Json]) => Conversion[value, Expr[value]] =
    value =>
      compiletime.summonInline[value is Encodable in Json].give:
        val encoded = value.json
        '{  import strategies.throwUnsafely
            compiletime.summonInline[value is Decodable in Json].give:
              ${refs.array}(${ToExpr.IntToExpr(refs.allocate(encoded))}).as[value]  }

  def fn(message: Example): Example = remote.dispatch:
    '{  val x = ${message.name}
        val y = ${message.count}
        println(y)
        Example(t"Time: $x $y ${System.currentTimeMillis - ${message}.count}", 9)  }

  println(fn(Example(t"one", System.currentTimeMillis)))
  println(fn(Example(t"two", System.currentTimeMillis)))
  println(fn(Example(t"three", System.currentTimeMillis)))
  println(fn(Example(t"four", System.currentTimeMillis)))
  println(fn(Example(t"five", System.currentTimeMillis)))
  println(fn(Example(t"six", System.currentTimeMillis)))
  println(fn(Example(t"seven", System.currentTimeMillis)))
