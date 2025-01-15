/*
    Guillotine, version 0.26.0. Copyright 2025 Jon Pretty, Propensive OÜ.

    The primary distribution site is: https://propensive.com/

    Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
    file except in compliance with the License. You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

    Unless required by applicable law or agreed to in writing, software distributed under the
    License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
    either express or implied. See the License for the specific language governing permissions
    and limitations under the License.
*/

package guillotine

import language.experimental.pureFunctions

import scala.quoted.*

import rudiments.*

object Guillotine:
  def sh(context: Expr[StringContext], parts: Expr[Seq[Any]])(using Quotes): Expr[Command] =
    import quotes.reflect.*

    val execType = ConstantType(StringConstant(context.value.get.parts.head.split(" ").nn.head.nn))
    val bounds = TypeBounds(execType, execType)

    (Refinement(TypeRepr.of[Command], "Exec", bounds).asType: @unchecked) match
      case '[type commandType <: Command; commandType] =>
        '{${Sh.Prefix.expand(context, parts)}.asInstanceOf[commandType]}
